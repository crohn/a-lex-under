use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::mem;

const DOT: char = '.';
const HYPHEN: char = '-';
const LOWER_E: char = 'e';
const PLUS: char = '+';
const UNDERSCORE: char = '_';
const UPPER_E: char = 'E';

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum State {
    Begin,
    EmitToken(fn(String) -> Token),
    End,
    Identifier,
    NumericLiteral,
    Whitespace,
}

// I want the following tokens
// - Identifier -> starting with alphabetic or _, then alphanumeric or _
// - Symbol -> any non alphanumeric, non whitespace
// - String Literal -> start with double quotes, supporting escapes
// - Numeric Literal -> start with numeric, supporting scientific notation, negative on parser
// - Whitespace -> whitespace sequence
#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    StringLiteral(String),
    Symbol(String),
    Whitespace(String),
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    cursor: Cursor,
    numeric_exp: bool,
    numeric_float: bool,
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
}

#[derive(Debug, PartialEq)]
pub enum TokenizationError {
    InvalidCharacter(State, Cursor),
    UnexpectedControlCharacter(State, Cursor),
    UnexpectedEndOfInput(State, Cursor),
    UnexpectedExponent(State, Cursor),
    UnexpectedFloatingPoint(State, Cursor),
    UnexpectedSign(State, Cursor),
    UnexpectedWhitespace(State, Cursor),
}

impl<'a> Tokenizer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            cursor: Cursor::default(),
            numeric_exp: false,
            numeric_float: false,
            scanner,
            state: State::Begin,
            string_buffer: String::new(),
        }
    }

    pub fn is_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphanumeric()
    }

    pub fn is_identifier_start(c: char) -> bool {
        c == UNDERSCORE || c.is_alphabetic()
    }

    pub fn is_symbol(c: char) -> bool {
        c != UNDERSCORE && !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
    }

    fn get_next_state(&mut self) -> Result<State, TokenizationError> {
        match self.state {
            State::Begin => self.handle_begin(),
            State::Identifier => self.handle_identifier(),
            State::NumericLiteral => self.handle_numeric_literal(),
            State::Whitespace => self.handle_whitespace(),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_begin(&mut self) -> Result<State, TokenizationError> {
        match self.cursor.curr {
            Some(c) if Tokenizer::is_symbol(c) => {
                self.string_buffer.push(c);
                Ok(State::EmitToken(Token::Symbol))
            }
            Some(c) if Tokenizer::is_identifier_start(c) => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => Ok(State::EmitToken(Token::Identifier)),
                    Some(k) if Tokenizer::is_identifier(k) => Ok(State::Identifier),
                    Some(k) if k.is_whitespace() => Ok(State::EmitToken(Token::Identifier)),
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
                }
            }
            Some(c) if c.is_whitespace() => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => Ok(State::EmitToken(Token::Whitespace)),
                    Some(k) if k.is_whitespace() => Ok(State::Whitespace),
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    Some(_) => Ok(State::EmitToken(Token::Whitespace)),
                }
            }
            Some(c) if c.is_numeric() => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => {
                        self.numeric_exp = false;
                        self.numeric_float = false;
                        Ok(State::EmitToken(Token::NumericLiteral))
                    }
                    Some(DOT) | Some(LOWER_E) | Some(UPPER_E) => Ok(State::NumericLiteral),
                    Some(k) if k.is_numeric() => Ok(State::NumericLiteral),
                    Some(k) if k.is_whitespace() => {
                        self.numeric_exp = false;
                        self.numeric_float = false;
                        Ok(State::EmitToken(Token::NumericLiteral))
                    }
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
                }
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_identifier(&mut self) -> Result<State, TokenizationError> {
        match self.cursor.curr {
            Some(c) if Tokenizer::is_identifier(c) => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => Ok(State::EmitToken(Token::Identifier)),
                    Some(k) if Tokenizer::is_identifier(k) => Ok(State::Identifier),
                    Some(k) if k.is_whitespace() => Ok(State::EmitToken(Token::Identifier)),
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
                }
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_numeric_literal(&mut self) -> Result<State, TokenizationError> {
        match self.cursor.curr {
            Some(DOT) => {
                if self.numeric_float {
                    Err(TokenizationError::UnexpectedFloatingPoint(
                        self.state,
                        self.cursor,
                    ))
                } else {
                    self.numeric_float = true;
                    self.string_buffer.push(DOT);
                    self.lookahead_numeric_literal()
                }
            }
            Some(c) if c == LOWER_E || c == UPPER_E => {
                if self.numeric_exp {
                    Err(TokenizationError::UnexpectedExponent(
                        self.state,
                        self.cursor,
                    ))
                } else {
                    self.numeric_exp = true;
                    self.numeric_float = true;
                    self.string_buffer.push(c);
                    self.lookahead_numeric_literal()
                }
            }
            Some(c) if c == PLUS || c == HYPHEN => {
                if matches!(self.cursor.prev, Some(LOWER_E) | Some(UPPER_E)) {
                    self.string_buffer.push(c);
                    self.lookahead_numeric_literal()
                } else {
                    Err(TokenizationError::UnexpectedSign(self.state, self.cursor))
                }
            }
            Some(c) if c.is_numeric() => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => {
                        self.numeric_exp = false;
                        self.numeric_float = false;
                        Ok(State::EmitToken(Token::NumericLiteral))
                    }
                    Some(k) if k.is_whitespace() => {
                        self.numeric_exp = false;
                        self.numeric_float = false;
                        Ok(State::EmitToken(Token::NumericLiteral))
                    }
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    Some(_) => Ok(State::NumericLiteral),
                }
            }
            Some(c) if c.is_control() => Err(
                TokenizationError::UnexpectedControlCharacter(self.state, self.cursor)
            ),
            Some(_) => Err(TokenizationError::InvalidCharacter(self.state, self.cursor)),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_whitespace(&mut self) -> Result<State, TokenizationError> {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => {
                self.string_buffer.push(c);
                match self.cursor.next {
                    None => Ok(State::EmitToken(Token::Whitespace)),
                    Some(k) if k.is_whitespace() => Ok(State::Whitespace),
                    Some(k) if k.is_control() => Err(
                        TokenizationError::UnexpectedControlCharacter(self.state, self.cursor),
                    ),
                    Some(_) => Ok(State::EmitToken(Token::Whitespace)),
                }
            }
            // there should not be any other cases covered, because of the
            // lookahead in handle_begin
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn lookahead_numeric_literal(&self) -> Result<State, TokenizationError> {
        match self.cursor.next {
            None => Err(TokenizationError::UnexpectedEndOfInput(
                self.state,
                self.cursor,
            )),
            Some(k) if k.is_whitespace() => Err(TokenizationError::UnexpectedWhitespace(
                self.state,
                self.cursor,
            )),
            Some(k) if k.is_control() => Err(TokenizationError::UnexpectedControlCharacter(
                self.state,
                self.cursor,
            )),
            Some(_) => Ok(State::NumericLiteral),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, TokenizationError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.state == State::End {
                return None;
            }

            if let State::EmitToken(emitter) = self.state {
                self.state = if self.cursor.next == None {
                    State::End
                } else {
                    State::Begin
                };

                if self.string_buffer.is_empty() {
                    unreachable!("<{:?}> {:?}", self.state, self.cursor);
                    //continue;
                }

                return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
            }

            if let Some(cursor) = self.scanner.next() {
                self.cursor = cursor;
                match self.get_next_state() {
                    Ok(next_state) => {
                        self.state = next_state;
                    }
                    Err(tokenization_error) => {
                        self.state = State::End;
                        return Some(Err(tokenization_error));
                    }
                }
            } else {
                self.state = State::End;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn u() {
        let tokenizer = Tokenizer::new(Scanner::new(""));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![]);

        let tokenizer = Tokenizer::new(Scanner::new("\t "));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![Ok(Token::Whitespace("\t ".to_string()))]);

        let tokenizer = Tokenizer::new(Scanner::new("\t f\n_\rf0o _foo foo"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace("\t ".to_string())),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace("\n".to_string())),
                Ok(Token::Identifier("_".to_string())),
                Ok(Token::Whitespace("\r".to_string())),
                Ok(Token::Identifier("f0o".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::Identifier("_foo".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::Identifier("foo".to_string()))
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f _ck -o4"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace("\t ".to_string())),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::Identifier("_ck".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::Symbol("-".to_string())),
                Ok(Token::Identifier("o4".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("2 22 22.2 2.2E+2 2.2e-2 2.2e2"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::NumericLiteral("2".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("22".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("22.2".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("2.2E+2".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("2.2e-2".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("2.2e2".to_string())),
            ]
        );
    }
}
