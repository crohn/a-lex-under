use std::mem;

const BACKSLASH: char = '\\';
const CARRIAGE_RETURN: char = '\r';
const CURLY_BRACKET_LEFT: char = '{';
const CURLY_BRACKET_RIGHT: char = '}';
const DOUBLE_QUOTES: char = '"';
const EQUALS_SIGN: char = '=';
const HYPHEN: char = '-';
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
    LongOption,
    Option,
    ShortOption,
    UnicodeEscapeSequence,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    LongOption(String),
    ShortOption(String),
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
                State::LongOption => self.handle_long_option(c, &mut tokens),
                State::Option => self.handle_option(c),
                State::ShortOption => self.handle_short_option(c, &mut tokens),
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
            State::LongOption => {
                if !self.buf.is_empty() {
                    tokens.push(Token::LongOption(mem::take(&mut self.buf)));
                }
            }
            State::ShortOption => {
                if self.buf.chars().count() == 1 {
                    tokens.push(Token::ShortOption(mem::take(&mut self.buf)));
                } else {
                    return Err(());
                }
            }
            State::Option => return Err(()), // unexpected end of input, input ends with '-'
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
    ///
    /// Any lexeme starting with a single hyphen (`-`) produces a `ShortOption`.
    /// Any lexeme starting with a double hyphen (`--`) produces a `LongOption`.
    fn handle_begin(&mut self, c: char) -> Result<State, ()> {
        match c {
            DOUBLE_QUOTES => {
                self.stack.push(c);
                Ok(State::EscapedStringLiteral)
            }
            HYPHEN => Ok(State::Option),
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

    /// The scanner in is `LongOption` state after `'--'` is encountered.
    ///
    /// The token admits alphanumeric characters, hyphens and underscores, but
    /// hyphens and underscores cannot be mixed.
    ///
    /// --foo bar      <- LongOption Identifier
    /// --foo "bar"    <- LongOption StringLiteral
    /// --foo="bar"    <- LongOption StringLiteral
    /// --foo-bar      <- LongOption
    /// --foo_bar      <- LongOption
    /// --foo2bar      <- LongOption
    /// --アキラ        <- LongOption
    /// --foo--bar     <- Invalid, double hyphen
    /// --foo__bar     <- Invalid, double underscore
    /// --foo-_bar     <- Invalid, succession hyphen underscore
    /// --foo_-bar     <- Invalid, succession underscore hyphen
    /// ---foo         <- Invalid, starts with hyphen
    /// --_foo         <- Invalid, starts with underscore
    /// --foo$bar      <- Invalid, non-alphanumeric
    fn handle_long_option(&mut self, c: char, tokens: &mut Vec<Token>) -> Result<State, ()> {
        if self.buf.is_empty() && !c.is_alphanumeric() {
            return Err(()); // must start with alphanumeric
        }

        let last_option_char = self.buf.chars().last();
        match (last_option_char, c) {
            (Some(HYPHEN), HYPHEN) => Err(()),          // invalid --
            (Some(HYPHEN), UNDERSCORE) => Err(()),      // invalid -_
            (Some(UNDERSCORE), HYPHEN) => Err(()),      // invalid _-
            (Some(UNDERSCORE), UNDERSCORE) => Err(()),  // invalid __
            (Some(HYPHEN), EQUALS_SIGN) => Err(()),     // invalid -=
            (Some(UNDERSCORE), EQUALS_SIGN) => Err(()), // invalid _=
            (Some(_), HYPHEN) => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            (Some(_), UNDERSCORE) => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            (Some(_), EQUALS_SIGN) => {
                tokens.push(Token::LongOption(mem::take(&mut self.buf)));
                Ok(State::Begin)
            }
            _ if c.is_whitespace() => {
                tokens.push(Token::LongOption(mem::take(&mut self.buf)));
                Ok(State::Begin)
            }
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            _ => Err(()), // invalid character
        }
    }

    /// The scanner is in `Option` state after a `'-'` is encountered.
    ///
    /// From this state, it can transition to long or short option, according to
    /// the value of `c`.
    fn handle_option(&mut self, c: char) -> Result<State, ()> {
        match c {
            HYPHEN => Ok(State::LongOption),
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::ShortOption)
            }
            _ => Err(()), // invalid character
        }
    }

    /// The scanner is in `ShortOption` state after a `-<CHAR>` input is
    /// encountered, thus we expect a non-empty buffer. Also <CHAR> is an
    /// alphanumeric UTF-8 char.
    fn handle_short_option(&mut self, c: char, tokens: &mut Vec<Token>) -> Result<State, ()> {
        if !c.is_whitespace() {
            return Err(()); // illegal character, this means that we had a '-ab' input
                            // this prevents that empty option is pushed into tokens, '- '
        }
        tokens.push(Token::ShortOption(mem::take(&mut self.buf)));
        Ok(State::Begin)
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
                if !matches!(self.stack.last(), Some(&CURLY_BRACKET_LEFT)) {
                    return Err(()); // unbalanced closing curly bracket
                }

                let codepoint = mem::take(&mut self.utf_codepoint);
                let Some(character) = char::from_u32(codepoint) else {
                    return Err(()); // invalid codepoint
                };

                if character.is_control() && !character.is_whitespace() {
                    return Err(()); // unexpected control character;
                }

                self.stack.pop();
                self.buf.push(character);
                Ok(State::EscapedStringLiteral)
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
    fn token_long_option() {
        let tokens = Scanner::new().scan("--123");
        assert_eq!(tokens, Ok(vec![Token::LongOption("123".to_string()),]));

        let tokens = Scanner::new().scan("--foo");
        assert_eq!(tokens, Ok(vec![Token::LongOption("foo".to_string()),]));

        let tokens = Scanner::new().scan("--foo bar");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::LongOption("foo".to_string()),
                Token::Identifier("bar".to_string())
            ])
        );

        let tokens = Scanner::new().scan("--foo \"bar\"");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::LongOption("foo".to_string()),
                Token::StringLiteral("bar".to_string())
            ])
        );

        let tokens = Scanner::new().scan("--foo=bar");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::LongOption("foo".to_string()),
                Token::Identifier("bar".to_string())
            ])
        );

        let tokens = Scanner::new().scan("--foo=\"bar\"");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::LongOption("foo".to_string()),
                Token::StringLiteral("bar".to_string())
            ])
        );

        let tokens = Scanner::new().scan("--foo-bar");
        assert_eq!(tokens, Ok(vec![Token::LongOption("foo-bar".to_string()),]));

        let tokens = Scanner::new().scan("--foo_bar");
        assert_eq!(tokens, Ok(vec![Token::LongOption("foo_bar".to_string()),]));

        let tokens = Scanner::new().scan("--foo2bar");
        assert_eq!(tokens, Ok(vec![Token::LongOption("foo2bar".to_string()),]));

        let tokens = Scanner::new().scan("--アキラ");
        assert_eq!(tokens, Ok(vec![Token::LongOption("アキラ".to_string()),]));

        let tokens = Scanner::new().scan("--foo--bar");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--foo-_bar");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--foo_-bar");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--foo__bar");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("---foo");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--_foo");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--=foo");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--f-=foo");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("--foo$bar");
        assert_eq!(tokens, Err(()));

        // this fails because identifier cannot start with =
        let tokens = Scanner::new().scan("--foo==bar");
        assert_eq!(tokens, Err(()));
    }

    #[test]
    fn token_short_option() {
        let tokens = Scanner::new().scan("-a -ア");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::ShortOption("a".to_string()),
                Token::ShortOption("ア".to_string())
            ])
        );

        let tokens = Scanner::new().scan("-");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("-?");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("- ");
        assert_eq!(tokens, Err(()));

        let tokens = Scanner::new().scan("-ab");
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
