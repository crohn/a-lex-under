use std::mem;

const BACKSLASH: char = '\\';
const CARRIAGE_RETURN: char = '\r';
const DOUBLE_QUOTES: char = '"';
const LOWERCASE_N: char = 'n';
const LOWERCASE_R: char = 'r';
const LOWERCASE_T: char = 't';
const NEWLINE: char = '\n';
const TAB: char = '\t';
const UNDERSCORE: char = '_';

enum State {
    Begin,
    EscapedStringLiteral,
    Identifier,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
}

pub struct Scanner {
    buf: String,
    escape: bool,
    stack: Vec<char>,
    state: State,
}

impl Default for Scanner {
    fn default() -> Self {
        Self {
            buf: String::new(),
            escape: false,
            stack: Vec::new(),
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
                State::EscapedStringLiteral => self.handle_escaped_string_literal(c, &mut tokens),
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
            State::EscapedStringLiteral => {} // main error is unbalance
            State::Identifier => {
                if !self.buf.is_empty() {
                    tokens.push(Token::Identifier(mem::take(&mut self.buf)))
                }
            }
        }

        // There is an unbalanced token somewhere. Stack needs to keep track of
        // col and row.
        if !self.stack.is_empty() {
            return Err(());
        }

        Ok(tokens)
    }

    /// The scanner is in `Begin` state at start or every time a token is
    /// produced.
    ///
    /// Any lexeme starting with an UTF-8 alphabetic character produces an
    /// `Identifier` token.
    ///
    /// Any lexeme starting with quotation mark (`"`) produces a `StringLiteral`
    /// token.
    fn handle_begin(&mut self, c: char) -> Result<State, ()> {
        match c {
            DOUBLE_QUOTES => {
                self.stack.push(c);
                Ok(State::EscapedStringLiteral)
            }
            _ if c.is_alphabetic() => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            _ if c.is_whitespace() => Ok(State::Begin),
            _ => Err(()),
        }
    }

    /// The scanner is in `EscapedStringLiteral` state while processing a
    /// `StringLiteral` token.
    ///
    /// The token carries the content, excluding the wrapping quotation marks
    /// and converts escape sequences into whitespaces or UTF-8 characters.
    ///
    /// When entering this state, the scanner has already processed the opening
    /// double quotes, pushing it into the stack to check the closing balance.
    fn handle_escaped_string_literal(
        &mut self,
        c: char,
        tokens: &mut Vec<Token>,
    ) -> Result<State, ()> {
        match c {
            BACKSLASH => self.in_literal_backslash(c),
            DOUBLE_QUOTES => self.in_literal_quotes(c, tokens),
            LOWERCASE_N => self.in_literal_whitespace(c, NEWLINE),
            LOWERCASE_R => self.in_literal_whitespace(c, CARRIAGE_RETURN),
            LOWERCASE_T => self.in_literal_whitespace(c, TAB),
            _ if c.is_whitespace() => {
                self.buf.push(c);
                Ok(State::EscapedStringLiteral)
            }
            _ if !c.is_control() => {
                if self.escape {
                    Err(()) // invalid escape sequence
                } else {
                    self.buf.push(c);
                    Ok(State::EscapedStringLiteral)
                }
            }
            _ if c.is_control() => Err(()),
            _ => unreachable!(),
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

    fn in_literal_backslash(&mut self, c: char) -> Result<State, ()> {
        if self.escape {
            self.escape = false;
            self.buf.push(c);
        } else {
            self.escape = true;
        }
        Ok(State::EscapedStringLiteral)
    }

    fn in_literal_quotes(&mut self, c: char, tokens: &mut Vec<Token>) -> Result<State, ()> {
        if self.escape {
            self.escape = false;
            self.buf.push(c);
            Ok(State::EscapedStringLiteral)
        } else if matches!(self.stack.last(), Some(&DOUBLE_QUOTES)) {
            self.stack.pop();
            let content = if self.buf.is_empty() {
                String::new()
            } else {
                mem::take(&mut self.buf)
            };
            tokens.push(Token::StringLiteral(content));
            Ok(State::Begin)
        } else {
            Err(())
        }
    }

    fn in_literal_whitespace(&mut self, c: char, escaped: char) -> Result<State, ()> {
        if self.escape {
            self.escape = false;
            self.buf.push(escaped);
        } else {
            self.buf.push(c);
        }
        Ok(State::EscapedStringLiteral)
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

    #[test]
    fn token_string_literal() {
        let tokens = Scanner::new().scan("\"\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("".to_string())]));

        let tokens = Scanner::new().scan("\"\\\"\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\"".to_string())]));

        let tokens = Scanner::new().scan("\"\t\n\r\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\t\n\r".to_string())]));

        let tokens = Scanner::new().scan("\"\\t\\n\\r\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\t\n\r".to_string())]));

        let tokens = Scanner::new().scan("\"v@lid\ts3quence アキラ\"");
        assert_eq!(
            tokens,
            Ok(vec![Token::StringLiteral(
                "v@lid\ts3quence アキラ".to_string()
            )])
        );

        let tokens = Scanner::new().scan("\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\z\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\ア\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"foo");
        assert_eq!(tokens, Err(()));
    }
}
