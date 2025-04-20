use std::mem;

const UNDERSCORE: char = '_';

enum State {
    Begin,
    Identifier,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
}

pub struct Scanner {
    buf: String,
    state: State,
}

impl Default for Scanner {
    fn default() -> Self {
        Self {
            buf: String::new(),
            state: State::Begin,
        }
    }
}

impl Scanner {
    pub fn new() -> Self {
        Scanner::default()
    }

    /// Transforms `input` string slice into a `Token` vector.
    ///
    /// The scanner uses a string buffer during the scan to accumulate
    /// characters and a state machine to produce tokens.
    pub fn scan(&mut self, input: &str) -> Result<Vec<Token>, ()> {
        let mut tokens = Vec::new();

        // main-loop check
        for c in input.chars() {
            self.state = match self.state {
                State::Begin => self.handle_begin(c),
                State::Identifier => self.handle_identifier(c, &mut tokens),
            }?;
        }

        // post-loop check
        // those states that produce tokens encountering whitespace chars need
        // one more check after end of input is reached, because the main-loop
        // ended and they will never receive the last delimiter.
        match self.state {
            State::Begin => {
                // Having a non-empty buffer while in Begin state is
                // inconsistent, because that's the starting state, after every
                // token production.
                if !self.buf.is_empty() {
                    return Err(());
                }
            }
            State::Identifier => {
                if !self.buf.is_empty() {
                    tokens.push(Token::Identifier(mem::take(&mut self.buf)))
                }
            }
        }

        Ok(tokens)
    }

    /// The scanner is in `Begin` state at start or every time a token is
    /// produced.
    ///
    /// Any lexeme starting with an UTF-8 alphabetic character produces an
    /// `Identifier` token.
    fn handle_begin(&mut self, c: char) -> Result<State, ()> {
        match c {
            _ if c.is_alphabetic() => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            _ if c.is_whitespace() => Ok(State::Begin),
            _ => Err(()),
        }
    }

    /// The scanner is in `Identifier` state while processing an `Identifier`
    /// token.
    ///
    /// `Identifier` tokens can contain UTF-8 alphanumeric characters or `'_'`
    /// (underscore) characters.
    ///
    /// When entering this state, the scanner has already processed the first
    /// char.
    fn handle_identifier(&mut self, c: char, tokens: &mut Vec<Token>) -> Result<State, ()> {
        match c {
            UNDERSCORE => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            // whitespace characters are the delimiter that trigger token
            // production
            _ if c.is_whitespace() => {
                tokens.push(Token::Identifier(mem::take(&mut self.buf)));
                Ok(State::Begin)
            }
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let tokens = Scanner::new().scan("");
        assert_eq!(tokens, Ok(vec![]));

        let tokens = Scanner::new().scan(" \n \r \t ");
        assert_eq!(tokens, Ok(vec![]));
    }

    #[test]
    fn token_identifier() {
        let tokens = Scanner::new().scan("foo foo_bar foo_123_bar アキラ");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Identifier("foo".to_string()),
                Token::Identifier("foo_bar".to_string()),
                Token::Identifier("foo_123_bar".to_string()),
                Token::Identifier("アキラ".to_string()),
            ])
        );

        let tokens = Scanner::new().scan("foo\tアキラ");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Identifier("foo".to_string()),
                Token::Identifier("アキラ".to_string()),
            ])
        );

        let tokens = Scanner::new().scan(" valid");
        assert_eq!(tokens, Ok(vec![Token::Identifier("valid".to_string())]));

        let tokens = Scanner::new().scan("_invalid");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("1nvalid");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("inv@lid");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("123");
        assert_eq!(tokens, Err(()));
    }
}
