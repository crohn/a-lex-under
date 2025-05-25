use crate::scanner::Scanner;
use crate::tokenization;
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
    EmitToken,
    None,
}

#[derive(Debug)]
enum State {
    Ready,
    Parse(ParseState),
    Complete(fn(String) -> Token),
    End,
}

#[derive(Debug)]
enum ParseState {
    Identifier,
    NumericLiteral { has_dot: bool, has_exp: bool },
    Symbol,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    Symbol(String),
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
        c.is_numeric()
    }

    pub fn is_char_symbol(c: char) -> bool {
        !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
    }

    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            scanner,
            state: State::Ready,
            string_buffer: String::new(),
        }
    }

    fn emit_numeric_literal_or_error(
        &self,
        has_dot: bool,
        has_exp: bool,
    ) -> Result<(State, Action), ()> {
        if has_dot || has_exp {
            if let Some(curr) = self.scanner.cursor().curr {
                if curr.is_numeric() {
                    Ok((State::Complete(Token::NumericLiteral), Action::EmitToken))
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        } else {
            Ok((State::Complete(Token::NumericLiteral), Action::EmitToken))
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, tokenization::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let result = match (&self.state, self.scanner.cursor().next) {
                (State::Ready, Some(c)) => {
                    if Tokenizer::is_char_start_identifier(c) {
                        Ok((State::Parse(ParseState::Identifier), Action::Append))
                    } else if Tokenizer::is_char_start_numeric_literal(c) {
                        Ok((
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: false,
                                has_exp: false,
                            }),
                            Action::Append,
                        ))
                    } else if Tokenizer::is_char_symbol(c) {
                        Ok((State::Parse(ParseState::Symbol), Action::Append))
                    } else if Tokenizer::is_char_delimiter(c) {
                        Ok((State::Parse(ParseState::Whitespace), Action::Append))
                    } else {
                        Err(())
                    }
                }
                (State::Ready, None) => Ok((State::End, Action::None)),

                (State::Parse(ParseState::Identifier), Some(c)) => {
                    if Tokenizer::is_char_identifier(c) {
                        Ok((State::Parse(ParseState::Identifier), Action::Append))
                    } else if Tokenizer::is_char_delimiter(c) {
                        Ok((State::Complete(Token::Identifier), Action::EmitToken))
                    } else if Tokenizer::is_char_symbol(c) {
                        Ok((State::Complete(Token::Identifier), Action::EmitToken))
                    } else {
                        Err(())
                    }
                }
                (State::Parse(ParseState::Identifier), None) => {
                    Ok((State::Complete(Token::Identifier), Action::EmitToken))
                }

                (State::Parse(ParseState::NumericLiteral { has_dot, has_exp }), Some(DOT)) => {
                    if *has_dot || *has_exp {
                        Err(())
                    } else {
                        Ok((
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: true,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        ))
                    }
                }
                (
                    State::Parse(ParseState::NumericLiteral { has_dot, has_exp }),
                    Some(UPPER_E) | Some(LOWER_E),
                ) => {
                    if *has_exp {
                        Err(())
                    } else {
                        Ok((
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: true,
                            }),
                            Action::Append,
                        ))
                    }
                }
                (
                    State::Parse(ParseState::NumericLiteral { has_dot, has_exp }),
                    Some(PLUS) | Some(HYPHEN),
                ) => {
                    if *has_exp
                        && matches!(self.scanner.cursor().curr, Some(UPPER_E) | Some(LOWER_E))
                    {
                        Ok((
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        ))
                    } else {
                        Err(())
                    }
                }
                (State::Parse(ParseState::NumericLiteral { has_dot, has_exp }), Some(c)) => {
                    if Tokenizer::is_char_numeric_literal(c) {
                        Ok((
                            State::Parse(ParseState::NumericLiteral {
                                has_dot: *has_dot,
                                has_exp: *has_exp,
                            }),
                            Action::Append,
                        ))
                    } else if Tokenizer::is_char_delimiter(c) {
                        self.emit_numeric_literal_or_error(*has_dot, *has_exp)
                    } else {
                        Err(())
                    }
                }
                (State::Parse(ParseState::NumericLiteral { has_dot, has_exp }), None) => {
                    self.emit_numeric_literal_or_error(*has_dot, *has_exp)
                }
                (State::Parse(ParseState::Symbol), _) => {
                    Ok((State::Complete(Token::Symbol), Action::EmitToken))
                }

                (State::Parse(ParseState::Whitespace), Some(c)) => {
                    if Tokenizer::is_char_delimiter(c) {
                        Ok((State::Parse(ParseState::Whitespace), Action::Append))
                    } else {
                        Ok((State::Complete(Token::Whitespace), Action::EmitToken))
                    }
                }
                (State::Parse(ParseState::Whitespace), None) => {
                    Ok((State::Complete(Token::Whitespace), Action::EmitToken))
                }

                (State::Complete(_), _) => Ok((State::Ready, Action::None)),

                (State::End, _) => return None,
            };

            match result {
                Err(()) => {
                    self.state = State::Ready;
                    self.scanner.next();
                    let buffer = mem::take(&mut self.string_buffer);
                    let error = Some(Err(tokenization::Error::InvalidCharacter {
                        buffer,
                        cursor: self.scanner.cursor().clone(),
                    }));
                    return error;
                }
                Ok((next_state, next_action)) => {
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
    }
}

#[cfg(test)]
mod test {
    use crate::cursor::CursorBuilder;

    use super::*;

    fn identifier(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Identifier(String::from(s)))
    }

    fn numeric_literal(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::NumericLiteral(String::from(s)))
    }

    fn symbol(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Symbol(String::from(s)))
    }

    fn whitespace(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Whitespace(String::from(s)))
    }

    fn tokenize(input: &str) -> Vec<Result<Token, tokenization::Error>> {
        let tokenizer = Tokenizer::new(Scanner::new(input));
        tokenizer.collect()
    }

    #[test]
    fn empty() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(
            tokenize("\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::new(),
                cursor: CursorBuilder::new().col(1).curr('\u{18}').build(),
            })]
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(
            tokenize("!@\u{2602}$"),
            vec![symbol("!"), symbol("@"), symbol("\u{2602}"), symbol("$"),]
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(tokenize("_"), vec![identifier("_")]);
        assert_eq!(
            tokenize("a-あ"),
            vec![identifier("a"), symbol("-"), identifier("あ")]
        );
        assert_eq!(
            tokenize("アキラ　_f1o"),
            vec![identifier("アキラ"), whitespace("　"), identifier("_f1o"),]
        );
        assert_eq!(
            tokenize("a\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("a"),
                cursor: CursorBuilder::new().col(2).prev('a').curr('\u{18}').build()
            })]
        );
    }

    #[test]
    fn test_numeric_literal() {
        assert_eq!(tokenize("1"), vec![numeric_literal("1")]);
        assert_eq!(
            tokenize("12 "),
            vec![numeric_literal("12"), whitespace(" ")]
        );
        assert_eq!(
            tokenize("1 23 45.6 78e9 1.2E+3 4.5e-6"),
            vec![
                numeric_literal("1"),
                whitespace(" "),
                numeric_literal("23"),
                whitespace(" "),
                numeric_literal("45.6"),
                whitespace(" "),
                numeric_literal("78e9"),
                whitespace(" "),
                numeric_literal("1.2E+3"),
                whitespace(" "),
                numeric_literal("4.5e-6"),
            ]
        );

        assert_eq!(tokenize("1.2"), vec![numeric_literal("1.2")]);
        assert_eq!(tokenize("1e+2"), vec![numeric_literal("1e+2")]);
        assert_eq!(
            tokenize("1\u{18}2"),
            vec![
                Err(tokenization::Error::InvalidCharacter {
                    buffer: String::from("1"),
                    cursor: CursorBuilder::new()
                        .col(2)
                        .prev('1')
                        .curr('\u{18}')
                        .next('2')
                        .build()
                }),
                numeric_literal("2")
            ]
        );
        assert_eq!(
            tokenize("12\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("12"),
                cursor: CursorBuilder::new().col(3).prev('2').curr('\u{18}').build()
            })]
        );
        assert_eq!(
            tokenize("1."),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(2).prev('.').build()
            })]
        );
        assert_eq!(
            tokenize("1. "),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr(' ').build()
            })]
        );
        assert_eq!(
            tokenize("1.\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr('\u{18}').build()
            })],
        );
        assert_eq!(
            tokenize("1.2."),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1.2"),
                cursor: CursorBuilder::new().col(4).prev('2').curr('.').build()
            })]
        );
        assert_eq!(
            tokenize("1e"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1e"),
                cursor: CursorBuilder::new().col(2).prev('e').build()
            })]
        );
        assert_eq!(
            tokenize("1E"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1E"),
                cursor: CursorBuilder::new().col(2).prev('E').build()
            })]
        );
        assert_eq!(
            tokenize("1a"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1"),
                cursor: CursorBuilder::new().col(2).prev('1').curr('a').build()
            })]
        );
        assert_eq!(
            tokenize("11a"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("11"),
                cursor: CursorBuilder::new().col(3).prev('1').curr('a').build()
            })]
        );
    }
}
