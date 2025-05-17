use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::mem;

const EQUALS: char = '=';
const HYPHEN: char = '-';

#[derive(Debug, PartialEq)]
pub enum State {
    Begin,
    EmitTokenString(fn(String) -> Token),
    EmitTokenWhitespace,
    End,
    Identifier,
    OptionLong,
    OptionSelect,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Operator(String),
    OptionShort(String),
    OptionLong(String),
    Whitespace,
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
    InvalidControlCharacter,
    UnexpectedEndOfInput,
    UnexpectedSymbol,
    UnexpectedWhitespace,
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

    fn get_next_state(&mut self) -> Result<State, TokenizationError> {
        match self.state {
            State::Begin => self.handle_begin(),
            State::Identifier => self.handle_identifier(),
            State::OptionLong => self.handle_option_long(),
            State::OptionSelect => self.handle_option_select(),
            State::Whitespace => self.handle_whitespace(),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_begin(&mut self) -> Result<State, TokenizationError> {
        match (self.cursor.curr, self.cursor.next) {
            (Some(EQUALS), None) => Err(TokenizationError::UnexpectedEndOfInput),
            (Some(EQUALS), _) => {
                self.string_buffer.push(EQUALS);
                Ok(State::EmitTokenString(Token::Operator))
            }
            (Some(HYPHEN), None) => Err(TokenizationError::UnexpectedEndOfInput),
            (Some(HYPHEN), _) => Ok(State::OptionSelect),
            (Some(c), None) if c.is_whitespace() => Ok(State::EmitTokenWhitespace),
            (Some(c), None) => {
                self.string_buffer.push(c);
                Ok(State::EmitTokenString(Token::Identifier))
            }
            (Some(c), Some(k)) if c.is_whitespace() => {
                if k.is_whitespace() {
                    Ok(State::Whitespace)
                } else {
                    Ok(State::EmitTokenWhitespace)
                }
            }
            (Some(c), Some(k)) => {
                self.string_buffer.push(c);
                if k.is_whitespace() {
                    Ok(State::EmitTokenString(Token::Identifier))
                } else {
                    Ok(State::Identifier)
                }
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_identifier(&mut self) -> Result<State, TokenizationError> {
        match (self.cursor.curr, self.cursor.next) {
            (Some(c), None) => {
                self.string_buffer.push(c);
                Ok(State::EmitTokenString(Token::Identifier))
            }
            (Some(c), Some(k)) => {
                self.string_buffer.push(c);
                if k.is_whitespace() {
                    Ok(State::EmitTokenString(Token::Identifier))
                } else {
                    Ok(State::Identifier)
                }
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_option_long(&mut self) -> Result<State, TokenizationError> {
        // NOTE curr can't be None, because of lookahead in handle_option_select
        match (self.cursor.curr, self.cursor.next) {
            (Some(c), _) if c.is_whitespace() => Err(TokenizationError::UnexpectedWhitespace),
            (Some(c), None) => {
                self.string_buffer.push(c);
                Ok(State::EmitTokenString(Token::OptionLong))
            },
            (Some(c), Some(k)) => {
                self.string_buffer.push(c);
                if k.is_whitespace() {
                    Ok(State::EmitTokenString(Token::OptionLong))
                } else if k == EQUALS {
                    Ok(State::EmitTokenString(Token::OptionLong))
                } else {
                    Ok(State::OptionLong)
                }
            },
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_option_select(&mut self) -> Result<State, TokenizationError> {
        // NOTE curr can't be None, because of lookahead in handle_begin
        match (self.cursor.curr, self.cursor.next) {
            (Some(HYPHEN), None) => Err(TokenizationError::UnexpectedEndOfInput),
            (Some(HYPHEN), _) => Ok(State::OptionLong),
            (Some(c), _) if c.is_whitespace() => Err(TokenizationError::UnexpectedWhitespace),
            (Some(c), _) => {
                self.string_buffer.push(c);
                Ok(State::EmitTokenString(Token::OptionShort))
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_whitespace(&self) -> Result<State, TokenizationError> {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => {
                if let Some(k) = self.cursor.next {
                    if k.is_whitespace() {
                        Ok(State::Whitespace)
                    } else {
                        Ok(State::EmitTokenWhitespace)
                    }
                } else {
                    Ok(State::EmitTokenWhitespace)
                }
            }
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
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

            if self.state == State::EmitTokenWhitespace {
                self.state = State::Begin;
                return Some(Ok(Token::Whitespace));
            }

            if let State::EmitTokenString(emitter) = self.state {
                self.state = if self.cursor.next == None {
                    State::End
                } else {
                    State::Begin
                };

                if self.string_buffer.is_empty() {
                    unreachable!();
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
        assert_eq!(tokens, vec![Ok(Token::Whitespace)]);

        let tokenizer = Tokenizer::new(Scanner::new("\t f foo"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::Identifier("foo".to_string()))
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f -"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Err(TokenizationError::UnexpectedEndOfInput)
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f -o 4"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::OptionShort("o".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::Identifier("4".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f -o4"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::OptionShort("o".to_string())),
                Ok(Token::Identifier("4".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f --o4 7"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::OptionLong("o4".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::Identifier("7".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f --o4=7"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::OptionLong("o4".to_string())),
                Ok(Token::Operator("=".to_string())),
                Ok(Token::Identifier("7".to_string())),
            ]
        );

        let tokenizer = Tokenizer::new(Scanner::new("\t f --o4= 7"));
        let tokens: Vec<Result<Token, TokenizationError>> = tokenizer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok(Token::Whitespace),
                Ok(Token::Identifier("f".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::OptionLong("o4".to_string())),
                Ok(Token::Operator("=".to_string())),
                Ok(Token::Whitespace),
                Ok(Token::Identifier("7".to_string())),
            ]
        );
    }
}
