use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::mem;

const DOT: char = '.';
const HYPHEN: char = '-';
const LOWER_E: char = 'e';
const PLUS: char = '+';
const UNDERSCORE: char = '_';
const UPPER_E: char = 'E';

#[derive(Clone, Debug, PartialEq)]
pub enum State {
    Begin,
    EmitToken(fn(String) -> Token),
    End,
    Error(Box<State>, fn(State, Cursor) -> TokenizationError),
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
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
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
            Some(c) if c.is_control() => State::Error(
                Box::new(self.state.clone()),
                TokenizationError::UnexpectedControlCharacter,
            ),
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn handle_identifier(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if Tokenizer::is_identifier(c) => {
                self.string_buffer.push(c);
                self.lookahead_identifier()
            }
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn handle_numeric_literal(&mut self, exp: bool, float: bool) -> State {
        match self.cursor.curr {
            Some(DOT) => {
                if float {
                    State::Error(
                        Box::new(self.state.clone()),
                        TokenizationError::UnexpectedFloatingPoint,
                    )
                } else {
                    self.string_buffer.push(DOT);
                    self.lookahead_numeric_literal_symbol(exp, true)
                }
            }
            Some(c) if c == LOWER_E || c == UPPER_E => {
                if exp {
                    State::Error(
                        Box::new(self.state.clone()),
                        TokenizationError::UnexpectedExponent,
                    )
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
                    State::Error(
                        Box::new(self.state.clone()),
                        TokenizationError::UnexpectedSign,
                    )
                }
            }
            Some(c) if c.is_numeric() => {
                self.string_buffer.push(c);
                self.lookahead_numeric_literal_number(exp, float)
            }
            Some(c) if c.is_control() => State::Error(
                Box::new(self.state.clone()),
                TokenizationError::UnexpectedControlCharacter,
            ),
            Some(_) => State::Error(
                Box::new(self.state.clone()),
                TokenizationError::InvalidCharacter,
            ),
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn handle_whitespace(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => {
                self.string_buffer.push(c);
                self.lookahead_whitespace()
            }
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn fail_on_next(&mut self, emitter: fn(State, Cursor) -> TokenizationError) -> State {
        if let Some(cursor) = self.scanner.next() {
            self.cursor = cursor;
            State::Error(Box::new(self.state.clone()), emitter)
        } else {
            self.cursor.col += 1;
            self.cursor.prev = self.cursor.curr;
            self.cursor.curr = None;
            self.cursor.next = None;
            State::Error(
                Box::new(self.state.clone()),
                TokenizationError::UnexpectedEndOfInput,
            )
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
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn lookahead_numeric_literal_begin(&mut self) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::NumericLiteral),
            Some(DOT) | Some(LOWER_E) | Some(UPPER_E) => State::NumericLiteral {
                exp: false,
                float: false,
            },
            Some(k) if Tokenizer::is_symbol(k) => State::EmitToken(Token::NumericLiteral),
            Some(k) if k.is_numeric() => State::NumericLiteral {
                exp: false,
                float: false,
            },
            Some(k) if k.is_whitespace() => State::EmitToken(Token::NumericLiteral),
            Some(k) if k.is_control() => {
                self.fail_on_next(TokenizationError::UnexpectedControlCharacter)
            }
            Some(k) if !k.is_numeric() => self.fail_on_next(TokenizationError::InvalidCharacter),
            _ => State::Error(Box::new(self.state.clone()), TokenizationError::Unreachable),
        }
    }

    fn lookahead_numeric_literal_number(&mut self, exp: bool, float: bool) -> State {
        match self.cursor.next {
            None => State::EmitToken(Token::NumericLiteral),
            Some(DOT) => State::NumericLiteral { exp, float },
            Some(k) if Tokenizer::is_symbol(k) => State::EmitToken(Token::NumericLiteral),
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

            let is_error = matches!(self.state, State::Error(_, _));
            if is_error {
                let state = mem::replace(&mut self.state, State::End);
                if let State::Error(prev, emitter) = state {
                    return Some(Err(emitter(*prev, self.cursor)));
                } else {
                    unreachable!("<{:?}> {:?}", self.state, self.cursor);
                }
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
    use crate::cursor::CursorBuilder;

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

    fn e_ctrl_char(state: State, cursor: Cursor) -> Result<Token, TokenizationError> {
        Err(TokenizationError::UnexpectedControlCharacter(state, cursor))
    }

    fn e_eof(state: State, cursor: Cursor) -> Result<Token, TokenizationError> {
        Err(TokenizationError::UnexpectedEndOfInput(state, cursor))
    }

    fn e_float(state: State, cursor: Cursor) -> Result<Token, TokenizationError> {
        Err(TokenizationError::UnexpectedFloatingPoint(state, cursor))
    }

    fn e_invalid(state: State, cursor: Cursor) -> Result<Token, TokenizationError> {
        Err(TokenizationError::InvalidCharacter(state, cursor))
    }

    fn e_whitespace(state: State, cursor: Cursor) -> Result<Token, TokenizationError> {
        Err(TokenizationError::UnexpectedWhitespace(state, cursor))
    }

    fn tokenize(input: &str) -> Vec<Result<Token, TokenizationError>> {
        let tokenizer = Tokenizer::new(Scanner::new(input));
        tokenizer.collect()
    }

    #[test]
    fn empty() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(
            tokenize("\u{18}"),
            vec![e_ctrl_char(
                State::Begin,
                CursorBuilder::new().curr('\u{18}').build()
            )]
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
            vec![e_ctrl_char(
                State::Begin,
                CursorBuilder::new().col(2).prev('a').curr('\u{18}').build()
            )]
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
            vec![e_ctrl_char(
                State::Begin,
                CursorBuilder::new()
                    .col(2)
                    .prev('1')
                    .curr('\u{18}')
                    .next('2')
                    .build()
            )]
        );
        assert_eq!(
            tokenize("12\u{18}"),
            vec![e_ctrl_char(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('2').curr('\u{18}').build()
            )]
        );
        assert_eq!(
            tokenize("1."),
            vec![e_eof(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('.').build()
            )]
        );
        assert_eq!(
            tokenize("1. "),
            vec![e_whitespace(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('.').curr(' ').build()
            )]
        );
        assert_eq!(
            tokenize("1.\u{18}"),
            vec![e_ctrl_char(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('.').curr('\u{18}').build()
            )],
        );
        assert_eq!(
            tokenize("1.2."),
            vec![e_float(
                State::NumericLiteral {
                    exp: false,
                    float: true
                },
                CursorBuilder::new().col(4).prev('2').curr('.').build()
            )]
        );
        assert_eq!(
            tokenize("1e"),
            vec![e_eof(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('e').build()
            )]
        );
        assert_eq!(
            tokenize("1E"),
            vec![e_eof(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('E').build()
            )]
        );
        assert_eq!(
            tokenize("1a"),
            vec![e_invalid(
                State::Begin,
                CursorBuilder::new().col(2).prev('1').curr('a').build()
            )]
        );
        assert_eq!(
            tokenize("11a"),
            vec![e_invalid(
                State::NumericLiteral {
                    exp: false,
                    float: false
                },
                CursorBuilder::new().col(3).prev('1').curr('a').build()
            )]
        );
    }

    #[test]
    fn test_expression() {
        assert_eq!(
            tokenize("2+2"),
            vec![numeric_literal("2"), symbol("+"), numeric_literal("2")]
        );
        assert_eq!(
            tokenize("2.0+2.0"),
            vec![numeric_literal("2.0"), symbol("+"), numeric_literal("2.0")]
        );
        assert_eq!(tokenize("1e2+"), vec![numeric_literal("1e2"), symbol("+")]);
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(
            tokenize("\n\r\t    ! a 2 "),
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
        assert_eq!(
            tokenize("\n\r\t    \u{18} "),
            vec![e_ctrl_char(
                State::Whitespace,
                CursorBuilder::new()
                    .row(2)
                    .col(7)
                    .prev(' ')
                    .curr('\u{18}')
                    .next(' ')
                    .build()
            )]
        )
    }
}
