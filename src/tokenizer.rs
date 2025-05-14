use crate::cursor::Cursor;
use crate::scanner::Scanner;
use std::mem;

#[derive(Debug)]
pub enum State {
    Begin,
    Identifier,
    EmitToken(fn(String) -> Token),
    End,
}

#[derive(Debug)]
pub enum Token {
    Identifier(String),
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    cursor: Cursor,
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
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

    fn get_next_state(&mut self) -> State {
        match self.state {
            State::Begin => self.handle_begin(),
            State::Identifier => self.handle_identifier(),
            State::End => self.handle_end(),
            State::EmitToken(_) => unreachable!(),
        }
    }

    fn handle_begin(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => {
                State::Begin
            }
            Some(c) => {
                self.string_buffer.push(c);
                State::Identifier
            }
            None => State::End,
        }
    }

    fn handle_identifier(&mut self) -> State {
        match self.cursor.curr {
            Some(c) if c.is_whitespace() => State::EmitToken(Token::Identifier),
            Some(c) => {
                self.string_buffer.push(c);
                State::Identifier
            }
            None => State::EmitToken(Token::Identifier)
        }
    }

    fn handle_end(&self) -> State {
        State::End
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(cursor) = self.scanner.next() {
            self.cursor = cursor;
            self.state = self.get_next_state();

            match self.state {
                State::EmitToken(constructor) => {
                    self.state = State::Begin;
                    return Some(constructor(mem::take(&mut self.string_buffer)));
                }
                _ => continue,
            }
        }

        self.state = State::End;
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn t() {
        let scanner = Scanner::new("hello world");
        let mut tokenizer = Tokenizer::new(scanner);
        let token = tokenizer.next();
        println!("{:?}", token);
        println!("{:?}", tokenizer);
    }
}
