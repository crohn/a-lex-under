use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::mem;

#[derive(Debug, PartialEq)]
pub enum State {
    Begin,
    EmitTokenString(fn(String) -> Token),
    EmitTokenWhitespace,
    End,
    Identifier,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
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
            State::Whitespace => self.handle_whitespace(),
            _ => unreachable!("<{:?}> {:?}", self.state, self.cursor),
        }
    }

    fn handle_begin(&mut self) -> Result<State, TokenizationError> {
        match (self.cursor.curr, self.cursor.next) {
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
    }
}
