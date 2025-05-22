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
    Error(fn(State, Cursor) -> TokenizationError),
    Identifier,
    NumericLiteral { exp: bool, float: bool },
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
    Unreachable(State, Cursor),
}

impl<'a> Tokenizer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            cursor: Cursor::default(),
            scanner,
            state: State::Begin,
            string_buffer: String::new(),
        }
    }

    fn is_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphanumeric()
    }

    fn is_identifier_start(c: char) -> bool {
        c == UNDERSCORE || c.is_alphabetic()
    }

    fn is_symbol(c: char) -> bool {
        c != UNDERSCORE && !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
    }

    fn get_next_state(&mut self) -> State {
        match self.state {
            State::Begin => self.handle_begin(),
            State::Identifier => self.handle_identifier(),
            State::NumericLiteral { exp, float } => self.handle_numeric_literal(exp, float),
            State::Whitespace => self.handle_whitespace(),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_begin(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if Tokenizer::is_symbol(c) => {
                self.string_buffer.push(c);
                State::EmitToken(Token::Symbol)
            }
            Some(c) if Tokenizer::is_identifier_start(c) => {
                self.string_buffer.push(c);
                self.lookahead_identifier()
            }
            Some(c) if c.is_whitespace() => {
                self.string_buffer.push(c);
                self.lookahead_whitespace()
            }
            Some(c) if c.is_numeric() => {
                self.string_buffer.push(c);
                self.lookahead_numeric_literal_begin()
            }
            Some(c) if c.is_control() => {
                State::Error(TokenizationError::UnexpectedControlCharacter)
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_identifier(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if Tokenizer::is_identifier(c) => {
                self.string_buffer.push(c);
                self.lookahead_identifier()
            }
            _ => State::Error(TokenizationError::Unreachable),
        }
    }

    fn handle_numeric_literal(&mut self, exp: bool, float: bool) -> State {
        match self.cursor.curr {
            Some(DOT) => {
                if float {
                    State::Error(TokenizationError::UnexpectedFloatingPoint)
                } else {
                    self.string_buffer.push(DOT);
                    self.lookahead_numeric_literal_symbol(exp, true)
                }
            }
            Some(c) if c == LOWER_E || c == UPPER_E => {
                if exp {
                    State::Error(TokenizationError::UnexpectedExponent)
                } else {
                    self.string_buffer.push(c);
                    self.lookahead_numeric_literal_symbol(true, true)
                }
            }
            Some(c) if c == PLUS || c == HYPHEN => {
                if matches!(self.cursor.prev, Some(LOWER_E) | Some(UPPER_E)) {
                    self.string_buffer.push(c);
                    self.lookahead_numeric_literal_symbol(exp, float)
                } else {
                    State::Error(TokenizationError::UnexpectedSign)
                }
            }
            Some(c) if c.is_numeric() => {
                self.string_buffer.push(c);
                self.lookahead_numeric_literal_number(exp, float)
            }
            Some(c) if c.is_control() => {
                State::Error(TokenizationError::UnexpectedControlCharacter)
            }
            Some(_) => State::Error(TokenizationError::InvalidCharacter),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_whitespace(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => {
                self.string_buffer.push(c);
                self.lookahead_whitespace()
            }
            _ => State::Error(TokenizationError::Unreachable),
        }
    }

    fn fail_on_next(&mut self, emitter: fn(State, Cursor) -> TokenizationError) -> State {
        if let Some(cursor) = self.scanner.next() {
            self.cursor = cursor;
            State::Error(emitter)
        } else {
            self.cursor.col += 1;
            self.cursor.prev = self.cursor.curr;
            self.cursor.curr = None;
            self.cursor.next = None;
            State::Error(TokenizationError::UnexpectedEndOfInput)
        }
    }

    fn lookahead_identifier(&mut self) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::Identifier),
            Some(k) if Tokenizer::is_symbol(k) => State::EmitToken(Token::Identifier),
            Some(k) if Tokenizer::is_identifier(k) => State::Identifier,
            Some(k) if k.is_whitespace() => State::EmitToken(Token::Identifier),
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            _ => State::Error(TokenizationError::Unreachable),
        }
    }

    fn lookahead_numeric_literal_begin(&mut self) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::NumericLiteral),
            Some(DOT) | Some(LOWER_E) | Some(UPPER_E) => State::NumericLiteral {
                exp: false,
                float: false,
            },
            Some(k) if k.is_numeric() => State::NumericLiteral {
                exp: false,
                float: false,
            },
            Some(k) if k.is_whitespace() => State::EmitToken(Token::NumericLiteral),
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            Some(k) if !k.is_numeric() => self.fail_on_next(TokenizationError::InvalidCharacter),
            _ => State::Error(TokenizationError::Unreachable),
        }
    }

    fn lookahead_numeric_literal_number(&mut self, exp: bool, float: bool) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::NumericLiteral),
            Some(k) if k.is_whitespace() => State::EmitToken(Token::NumericLiteral),
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            Some(_) => State::NumericLiteral { exp, float },
        }
    }

    fn lookahead_numeric_literal_symbol(&mut self, exp: bool, float: bool) -> State {
        match self.cursor.next {
            None => self.fail_on_next(TokenizationError::UnexpectedEndOfInput),
            Some(k) if k.is_whitespace() => {
                self.fail_on_next(TokenizationError::UnexpectedWhitespace)
            }
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            Some(_) => State::NumericLiteral { exp, float },
        }
    }

    fn lookahead_whitespace(&mut self) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::Whitespace),
            Some(k) if k.is_whitespace() => State::Whitespace,
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            _ => State::EmitToken(Token::Whitespace),
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

            if let State::Error(emitter) = self.state {
                self.state = State::End;
                return Some(Err(emitter(self.state, self.cursor)));
            }

            if let Some(cursor) = self.scanner.next() {
                self.cursor = cursor;
                self.state = self.get_next_state();
            } else {
                self.state = State::End;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn identifier(value: &str) -> Result<Token, TokenizationError> {
        Ok(Token::Identifier(String::from(value)))
    }

    fn numeric_literal(value: &str) -> Result<Token, TokenizationError> {
        Ok(Token::NumericLiteral(String::from(value)))
    }

    fn symbol(value: &str) -> Result<Token, TokenizationError> {
        Ok(Token::Symbol(String::from(value)))
    }

    fn whitespace(value: &str) -> Result<Token, TokenizationError> {
        Ok(Token::Whitespace(String::from(value)))
    }

    #[test]
    fn empty() {
        let tokenizer = Tokenizer::new(Scanner::new(""));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![]);

        let tokenizer = Tokenizer::new(Scanner::new("\u{18}"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    row: 1,
                    col: 1,
                    curr: Some('\u{18}'),
                    next: None,
                    prev: None,
                }
            ))]
        );
    }

    #[test]
    fn test_symbol() {
        let tokenizer = Tokenizer::new(Scanner::new("!@\u{2602}$"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![symbol("!"), symbol("@"), symbol("\u{2602}"), symbol("$"),]
        );
    }

    #[test]
    fn test_identifier() {
        let tokenizer = Tokenizer::new(Scanner::new("_"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![identifier("_")]);

        let tokenizer = Tokenizer::new(Scanner::new("a-あ"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![identifier("a"), symbol("-"), identifier("あ")]);

        let tokenizer = Tokenizer::new(Scanner::new("アキラ　_f1o"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![identifier("アキラ"), whitespace("　"), identifier("_f1o"),]
        );

        let tokenizer = Tokenizer::new(Scanner::new("a\u{18}"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    col: 2,
                    row: 1,
                    curr: Some('\u{18}'),
                    prev: Some('a'),
                    next: None,
                }
            ))]
        );
    }

    #[test]
    fn test_numeric_literal() {
        let tokenizer = Tokenizer::new(Scanner::new("1"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![numeric_literal("1")]);

        let tokenizer = Tokenizer::new(Scanner::new("12 "));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![numeric_literal("12"), whitespace(" ")]);

        let tokenizer = Tokenizer::new(Scanner::new("1 23 45.6 78e9 1.2E+3 4.5e-6"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::NumericLiteral("1".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("23".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("45.6".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("78e9".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("1.2E+3".to_string())),
                Ok(Token::Whitespace(" ".to_string())),
                Ok(Token::NumericLiteral("4.5e-6".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1.2"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![numeric_literal("1.2")]);

        let tokenizer = Tokenizer::new(Scanner::new("1e+2"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(tokens, vec![numeric_literal("1e+2")]);

        let tokenizer = Tokenizer::new(Scanner::new("1\u{18}2"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    col: 2,
                    row: 1,
                    curr: Some('\u{18}'),
                    prev: Some('1'),
                    next: Some('2')
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("12\u{18}"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: Some('\u{18}'),
                    prev: Some('2'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1."));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedEndOfInput(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: None,
                    prev: Some('.'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1. "));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedWhitespace(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: Some(' '),
                    prev: Some('.'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1.\u{18}"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: Some('\u{18}'),
                    prev: Some('.'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1.2."));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedFloatingPoint(
                State::End,
                Cursor {
                    col: 4,
                    row: 1,
                    curr: Some('.'),
                    prev: Some('2'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1e"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedEndOfInput(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: None,
                    prev: Some('e'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1e2+"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedSign(
                State::End,
                Cursor {
                    col: 4,
                    row: 1,
                    curr: Some('+'),
                    prev: Some('2'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1E"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedEndOfInput(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: None,
                    prev: Some('E'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("1a"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::InvalidCharacter(
                State::End,
                Cursor {
                    col: 2,
                    row: 1,
                    curr: Some('a'),
                    prev: Some('1'),
                    next: None
                }
            ))]
        );

        let tokenizer = Tokenizer::new(Scanner::new("11a"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::InvalidCharacter(
                State::End,
                Cursor {
                    col: 3,
                    row: 1,
                    curr: Some('a'),
                    prev: Some('1'),
                    next: None
                }
            ))]
        );
    }

    #[test]
    fn test_whitespace() {
        let tokenizer = Tokenizer::new(Scanner::new("\n\r\t    ! a 2 "));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                whitespace("\n\r\t    "),
                symbol("!"),
                whitespace(" "),
                identifier("a"),
                whitespace(" "),
                numeric_literal("2"),
                whitespace(" "),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\n\r\t    \u{18} "));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![Err(TokenizationError::UnexpectedControlCharacter(
                State::End,
                Cursor {
                    col: 7,
                    row: 2,
                    curr: Some('\u{18}'),
                    prev: Some(' '),
                    next: Some(' '),
                }
            ))]
        )
    }
}
