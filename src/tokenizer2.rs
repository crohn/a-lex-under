use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::iter::Iterator;
use std::mem;

const DOT: char = '.';
const HYPHEN: char = '-';
const LOWER_E: char = 'e';
const PLUS: char = '+';
const UNDERSCORE: char = '_';
const UPPER_E: char = 'E';

#[derive(Debug)]
enum Action {
    Append,
    EmitError,
    EmitToken,
    None,
}

#[derive(Debug)]
enum State {
    Ready,
    Parse(ParseState),
    Complete(fn(String) -> Token),
    Error,
    End,
}

#[derive(Debug)]
enum ParseState {
    Identifier,
    NumericLiteral { has_dot: bool, has_exp: bool },
    Whitespace,
}

#[derive(Debug)]
pub enum Error {
    InvalidCharacter { buffer: String, cursor: Cursor },
}

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    Whitespace(String),
}

pub struct Tokenizer<'a> {
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn is_char_delimiter(c: char) -> bool {
        c.is_whitespace()
    }

    pub fn is_char_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphanumeric()
    }

    pub fn is_char_start_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphabetic()
    }

    pub fn is_char_numeric_literal(c: char) -> bool {
        c == DOT || c == PLUS || c == HYPHEN || c == UPPER_E || c == LOWER_E || c.is_numeric()
    }

    pub fn is_char_start_numeric_literal(c: char) -> bool {
        c == DOT || c == PLUS || c == HYPHEN || c == UPPER_E || c == LOWER_E || c.is_numeric()
    }

    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            scanner,
            state: State::Ready,
            string_buffer: String::new(),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (next_state, next_action) = match (&self.state, self.scanner.cursor().next) {
                (State::Ready, Some(c)) => {
                    if Tokenizer::is_char_start_identifier(c) {
                        (State::Parse(ParseState::Identifier), Action::Append)
                    } else if Tokenizer::is_char_start_numeric_literal(c) {
                        (
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: false,
                                has_exp: false,
                            }),
                            Action::Append,
                        )
                    } else if Tokenizer::is_char_delimiter(c) {
                        (State::Parse(ParseState::Whitespace), Action::Append)
                    } else {
                        (State::Error, Action::EmitError)
                    }
                }
                (State::Ready, None) => (State::End, Action::None),

                (State::Parse(ParseState::Identifier), Some(c)) => {
                    if Tokenizer::is_char_identifier(c) {
                        (State::Parse(ParseState::Identifier), Action::Append)
                    } else if Tokenizer::is_char_delimiter(c) {
                        (State::Complete(Token::Identifier), Action::EmitToken)
                    } else {
                        (State::Error, Action::EmitError)
                    }
                }
                (State::Parse(ParseState::Identifier), None) => {
                    (State::Complete(Token::Identifier), Action::EmitToken)
                }

                (State::Parse(ParseState::NumericLiteral { has_dot, has_exp }), Some(DOT)) => {
                    if *has_dot || *has_exp {
                        (State::Error, Action::EmitError)
                    } else {
                        (
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: true,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        )
                    }
                }
                (
                    State::Parse(ParseState::NumericLiteral { has_dot, has_exp }),
                    Some(UPPER_E) | Some(LOWER_E),
                ) => {
                    if *has_exp {
                        (State::Error, Action::EmitError)
                    } else {
                        (
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: true,
                            }),
                            Action::Append,
                        )
                    }
                }
                (
                    State::Parse(ParseState::NumericLiteral { has_dot, has_exp }),
                    Some(PLUS) | Some(HYPHEN),
                ) => {
                    if *has_exp
                        && matches!(self.scanner.cursor().curr, Some(UPPER_E) | Some(LOWER_E))
                    {
                        (
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        )
                    } else {
                        (State::Error, Action::EmitError)
                    }
                }
                (State::Parse(ParseState::NumericLiteral { has_dot, has_exp }), Some(c)) => {
                    if Tokenizer::is_char_numeric_literal(c) {
                        (
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        )
                    } else if Tokenizer::is_char_delimiter(c) {
                        (State::Complete(Token::NumericLiteral), Action::EmitToken)
                    } else {
                        (State::Error, Action::EmitError)
                    }
                }
                (State::Parse(ParseState::NumericLiteral { .. }), None) => {
                    (State::Complete(Token::NumericLiteral), Action::EmitToken)
                }

                (State::Parse(ParseState::Whitespace), Some(c)) => {
                    if Tokenizer::is_char_delimiter(c) {
                        (State::Parse(ParseState::Whitespace), Action::Append)
                    } else {
                        (State::Complete(Token::Whitespace), Action::EmitToken)
                    }
                }
                (State::Parse(ParseState::Whitespace), None) => {
                    (State::Complete(Token::Whitespace), Action::EmitToken)
                }

                (State::Complete(_), _) => (State::Ready, Action::None),

                (State::Error, _) => (State::Ready, Action::None),

                (State::End, _) => return None,
            };

            self.state = next_state;

            match next_action {
                Action::Append => {
                    // NOTE do not emit if empty buffer
                    if let Some(cursor) = self.scanner.next() {
                        if let Some(c) = cursor.curr {
                            self.string_buffer.push(c);
                        }
                    }
                }
                Action::EmitError => {
                    self.scanner.next();
                    let buffer = mem::take(&mut self.string_buffer);
                    let error = Some(Err(Error::InvalidCharacter {
                        buffer,
                        cursor: self.scanner.cursor().clone(),
                    }));
                    return error;
                }
                Action::EmitToken => {
                    if let State::Complete(emitter) = self.state {
                        return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
                    }
                }
                Action::None => continue,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn a() {
        let tokenizer = Tokenizer::new(Scanner::new("hello 42.0e-1 world!"));
        for token in tokenizer {
            println!("{:?}", token);
        }
    }
}
