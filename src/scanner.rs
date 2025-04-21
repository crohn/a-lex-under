use std::mem;

const BACKSLASH: char = '\\';
const CARRIAGE_RETURN: char = '\r';
const CURLY_BRACKET_LEFT: char = '{';
const CURLY_BRACKET_RIGHT: char = '}';
const DOUBLE_QUOTES: char = '"';
const LOWERCASE_N: char = 'n';
const LOWERCASE_R: char = 'r';
const LOWERCASE_T: char = 't';
const LOWERCASE_U: char = 'u';
const NEWLINE: char = '\n';
const TAB: char = '\t';
const UNDERSCORE: char = '_';

enum State {
    Begin,
    EscapedStringLiteral,
    Identifier,
    UnicodeEscapeSequence,
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
    utf_codepoint: u32,
}

impl Default for Scanner {
    fn default() -> Self {
        Self {
            buf: String::new(),
            escape: false,
            stack: Vec::new(),
            state: State::Begin,
            utf_codepoint: 0,
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
                State::UnicodeEscapeSequence => self.handle_unicode_escape_sequence(c),
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
            State::EscapedStringLiteral => {} // main error is unbalance "
            State::Identifier => {
                if !self.buf.is_empty() {
                    tokens.push(Token::Identifier(mem::take(&mut self.buf)))
                }
            }
            State::UnicodeEscapeSequence => {} // main error is unbalance {
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
            LOWERCASE_U => self.in_literal_unicode(c),
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

    /// The scanner is in `UnicodeEscapeSequence` state after a `\u` escape
    /// sequence is encountered.
    ///
    /// The escape sequence continues with `{<HEX_DIGITS>}`.
    fn handle_unicode_escape_sequence(&mut self, c: char) -> Result<State, ()> {
        match c {
            CURLY_BRACKET_LEFT => {
                if matches!(self.stack.last(), Some(&CURLY_BRACKET_LEFT)) {
                    Err(()) // cannot push { over {
                } else {
                    self.stack.push(c);
                    Ok(State::UnicodeEscapeSequence)
                }
            }
            CURLY_BRACKET_RIGHT => {
                if matches!(self.stack.last(), Some(&CURLY_BRACKET_LEFT)) {
                    if let Some(character) = char::from_u32(mem::take(&mut self.utf_codepoint)) {
                        if character.is_control() && !character.is_whitespace() {
                            Err(()) // control characters are not allowed unless they are whitespace
                        } else {
                            self.stack.pop();
                            self.buf.push(character);
                            Ok(State::EscapedStringLiteral)
                        }
                    } else {
                        Err(()) // invalid escape sequence
                    }
                } else {
                    // unbalanced closing curly bracket
                    Err(())
                }
            }
            _ => {
                if let Some(value) = c.to_digit(16) {
                    self.utf_codepoint = self.utf_codepoint * 16 + value;
                    Ok(State::UnicodeEscapeSequence)
                } else {
                    Err(()) // invalid hex digit
                }
            }
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

    /// The scanner receiving a `u` character checks whether it's a `\u` escape
    /// sequence or just the `u` ASCII character.
    fn in_literal_unicode(&mut self, c: char) -> Result<State, ()> {
        if self.escape {
            self.escape = false;
            Ok(State::UnicodeEscapeSequence)
        } else {
            self.buf.push(c);
            Ok(State::EscapedStringLiteral)
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

        let tokens = Scanner::new().scan("\"\\\\\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\\".to_string())]));

        let tokens = Scanner::new().scan("\"\t\n\r\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\t\n\r".to_string())]));

        let tokens = Scanner::new().scan("\"\\u{9}\\u{a}\\u{d}\\u{20}\"");
        assert_eq!(
            tokens,
            Ok(vec![Token::StringLiteral("\t\n\r ".to_string())])
        );

        let tokens = Scanner::new().scan("\"\\t\\n\\r\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("\t\n\r".to_string())]));

        let tokens = Scanner::new().scan("\"v@lid\ts3quence アキラ\"");
        assert_eq!(
            tokens,
            Ok(vec![Token::StringLiteral(
                "v@lid\ts3quence アキラ".to_string()
            )])
        );

        let tokens = Scanner::new().scan("\"\\u{30a2}\\u{30ad}\\u{30e9}\"");
        assert_eq!(tokens, Ok(vec![Token::StringLiteral("アキラ".to_string())]));

        let tokens = Scanner::new().scan("\"\\u{30a2}\\u{30ad}\\u{30e9}\"");
        assert_eq!(
            tokens,
            Ok(vec![Token::StringLiteral(
                "\u{30a2}\u{30ad}\u{30e9}".to_string()
            )])
        );

        // invalid codepoint
        let tokens = Scanner::new().scan("\"\\u{d801}\"");
        assert_eq!(tokens, Err(()));

        // invalid character
        let tokens = Scanner::new().scan("\"hello world\\{0}\"");
        assert_eq!(tokens, Err(()));

        // invalid escape sequence
        let tokens = Scanner::new().scan("\"\\u\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\z\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\ア\"");
        assert_eq!(tokens, Err(()));

        // unbalanced quotes
        let tokens = Scanner::new().scan("\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"\\\"");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("\"foo");
        assert_eq!(tokens, Err(()));

        // unbalanced curly bracer
        let tokens = Scanner::new().scan("\"\\u{7f");
        assert_eq!(tokens, Err(()));
    }
}
