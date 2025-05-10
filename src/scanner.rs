pub mod error;

use std::mem;

use error::{EXPECTED_TOKEN_BEGIN, ScanError, ScanErrorKind};

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
    col: usize,
    escape: bool,
    row: usize,
    stack: Vec<char>,
    state: State,
    utf_codepoint: u32,
}

impl Default for Scanner {
    fn default() -> Self {
        Self {
            buf: String::new(),
            col: 0,
            escape: false,
            row: 1,
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
    pub fn scan(&mut self, input: &str) -> Result<Vec<Token>, ScanError> {
        let mut tokens = Vec::new();

        let mut iterator = input.chars().peekable();

        // main-loop check
        while let Some(curr) = iterator.next() {
            self.col += 1;
            if curr == NEWLINE {
                self.row += 1;
                self.col = 1;
            }

            self.state = match self.state {
                State::Begin => self.handle_begin(curr),
                State::EscapedStringLiteral => {
                    self.handle_escaped_string_literal(curr, &mut tokens)
                }
                State::Identifier => self.handle_identifier(curr),
                State::LongOption => self.handle_long_option(curr, &mut tokens),
                State::Option => self.handle_option(curr),
                // State::ShortOption => self.handle_short_option(curr, &mut tokens),
                State::UnicodeEscapeSequence => self.handle_unicode_escape_sequence(curr),
                _ => unreachable!(" --- {}", curr),
            }?;

            // use lookahead to handle end of input and token emission
            match (&self.state, iterator.peek()) {
                (State::Begin, None) => {
                    // Having a non-empty buffer while in State::Begin means
                    // that the Scanner is in an inconsistent state, because
                    // that's the state after every token emission.
                    if !self.buf.is_empty() {
                        unreachable!("fatal: unexpected end of input.")
                    }
                }
                (State::Identifier, None) => {
                    if !self.buf.is_empty() {
                        self.emit_token(Token::Identifier, &mut tokens);
                    }
                }
                (State::LongOption, None) => {
                    if !self.buf.is_empty() {
                        self.emit_token(Token::LongOption, &mut tokens);
                    }
                }
                (State::Option, None) => {
                    return Err(self.scan_error(ScanErrorKind::UnexpectedOptionEndOfInput));
                }
                (State::ShortOption, None) => {
                    if !self.buf.is_empty() {
                        self.emit_token(Token::ShortOption, &mut tokens);
                    }
                }
                (State::Identifier, Some(c)) if c.is_whitespace() => {
                    self.emit_token(Token::Identifier, &mut tokens)
                }
                (State::LongOption, Some(c)) if c.is_whitespace() => {
                    self.emit_token(Token::LongOption, &mut tokens)
                }
                (State::ShortOption, Some(c)) if c.is_whitespace() => {
                    self.emit_token(Token::ShortOption, &mut tokens)
                }
                (State::ShortOption, Some(c)) => {
                    self.col += 1;
                    return Err(
                        self.scan_error(ScanErrorKind::UnexpectedShortOptionContinuation(*c))
                    );
                }
                _ => continue,
            }
        }

        match self.stack.last() {
            Some(&CURLY_BRACKET_LEFT) => {
                Err(self.scan_error(ScanErrorKind::UnbalancedCurlyBracket))
            }
            Some(&DOUBLE_QUOTES) => Err(self.scan_error(ScanErrorKind::UnbalancedDoubleQuotes)),
            Some(c) => unreachable!("fatal: unexpected character in stack: '{}'", c),
            None => Ok(tokens),
        }
    }

    fn emit_token(&mut self, constructor: fn(String) -> Token, tokens: &mut Vec<Token>) {
        tokens.push(constructor(mem::take(&mut self.buf)));
        self.state = State::Begin;
    }

    /// The scanner is in `Begin` state at start or after a token is produced.
    ///
    /// Valid characters and state machine transitions:
    /// ```txt
    /// UTF-8 alphabetic character -> Identifier
    ///         quotation mark (") -> StringLiteral
    ///                 hyphen (-) -> Option
    ///    whitespace (\n|\t|\r| ) -> Begin
    /// ```
    fn handle_begin(&mut self, c: char) -> Result<State, ScanError> {
        match c {
            DOUBLE_QUOTES => {
                self.stack.push(c);
                Ok(State::EscapedStringLiteral)
            }
            HYPHEN => Ok(State::Option),
            c if c.is_alphabetic() => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            c if c.is_whitespace() => Ok(State::Begin),
            c => Err(self.scan_error(ScanErrorKind::InvalidSymbol {
                expected: EXPECTED_TOKEN_BEGIN,
                got: c,
            })),
        }
    }

    /// The scanner is in `EscapedStringLiteral` state while processing a
    /// `StringLiteral` token.
    ///
    /// The token carries the content, excluding the wrapping quotation marks
    /// and converts escape sequences into whitespaces or UTF-8 characters.
    /// Control characters are excluded.
    ///
    /// When entering this state, the scanner has already processed the opening
    /// double quotes, pushing it into the stack to check the closing balance.
    fn handle_escaped_string_literal(
        &mut self,
        c: char,
        tokens: &mut Vec<Token>,
    ) -> Result<State, ScanError> {
        match c {
            BACKSLASH => Ok(self.in_literal_backslash(c)),
            DOUBLE_QUOTES => self.handle_quotes_in_literal(tokens),
            LOWERCASE_N => Ok(self.in_literal_whitespace(c, NEWLINE)),
            LOWERCASE_R => Ok(self.in_literal_whitespace(c, CARRIAGE_RETURN)),
            LOWERCASE_T => Ok(self.in_literal_whitespace(c, TAB)),
            LOWERCASE_U => Ok(self.in_literal_unicode(c)),
            _ if c.is_whitespace() => {
                self.buf.push(c);
                Ok(State::EscapedStringLiteral)
            }
            c if !c.is_control() => {
                if self.escape {
                    Err(self.scan_error(ScanErrorKind::InvalidLiteralEscape(c)))
                } else {
                    self.buf.push(c);
                    Ok(State::EscapedStringLiteral)
                }
            }
            c if c.is_control() => {
                Err(self.scan_error(ScanErrorKind::UnexpectedControlCharacter(c)))
            }
            _ => unreachable!(),
        }
    }

    /// The scanner is in `Identifier` state while processing an `Identifier`
    /// token.
    ///
    /// `Identifier` tokens can contain UTF-8 alphanumeric characters or
    /// underscor (`'_'`) characters.
    ///
    /// When entering this state, the scanner has already processed the first
    /// character, adding it to the string buffer.
    fn handle_identifier(&mut self, c: char) -> Result<State, ScanError> {
        match c {
            UNDERSCORE => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::Identifier)
            }
            c => Err(self.scan_error(ScanErrorKind::InvalidIdentifier(c))),
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
    fn handle_long_option(&mut self, c: char, tokens: &mut Vec<Token>) -> Result<State, ScanError> {
        if self.buf.is_empty() && !c.is_alphanumeric() {
            return Err(self.scan_error(ScanErrorKind::InvalidLongOptionStart(c)));
        }

        let last_option_char = self.buf.chars().last();
        match (last_option_char, c) {
            (Some(HYPHEN), HYPHEN) => {
                Err(self.scan_error(ScanErrorKind::InvalidLongOptionSequence(HYPHEN, HYPHEN)))
            }
            (Some(HYPHEN), UNDERSCORE) => {
                Err(self.scan_error(ScanErrorKind::InvalidLongOptionSequence(HYPHEN, UNDERSCORE)))
            }
            (Some(UNDERSCORE), HYPHEN) => {
                Err(self.scan_error(ScanErrorKind::InvalidLongOptionSequence(UNDERSCORE, HYPHEN)))
            }
            (Some(UNDERSCORE), UNDERSCORE) => Err(self.scan_error(
                ScanErrorKind::InvalidLongOptionSequence(UNDERSCORE, UNDERSCORE),
            )),
            (Some(HYPHEN), EQUALS_SIGN) => Err(self.scan_error(
                ScanErrorKind::InvalidLongOptionSequence(HYPHEN, EQUALS_SIGN),
            )),
            (Some(UNDERSCORE), EQUALS_SIGN) => Err(self.scan_error(
                ScanErrorKind::InvalidLongOptionSequence(UNDERSCORE, EQUALS_SIGN),
            )),
            (Some(_), HYPHEN) => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            (Some(_), UNDERSCORE) => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            // FIXME move this to `scan` <<< search for mem::take
            (Some(_), EQUALS_SIGN) => {
                tokens.push(Token::LongOption(mem::take(&mut self.buf)));
                Ok(State::Begin)
            }
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::LongOption)
            }
            (_, c) => Err(self.scan_error(ScanErrorKind::InvalidLongOption(c))),
        }
    }

    /// The scanner is in `Option` state after a '`-`' is encountered.
    ///
    /// ```txt
    /// UTF-8 alphanumeric character -> ShortOption
    ///                   hyphen (-) -> LongOption
    /// ```
    fn handle_option(&mut self, c: char) -> Result<State, ScanError> {
        match c {
            HYPHEN => Ok(State::LongOption),
            _ if c.is_alphanumeric() => {
                self.buf.push(c);
                Ok(State::ShortOption)
            }
            c => Err(self.scan_error(ScanErrorKind::UnexpectedOptionContinuation(c))),
        }
    }

    /// The scanner is in `UnicodeEscapeSequence` state after a `\u` escape
    /// sequence is encountered.
    ///
    /// The escape sequence continues with `{<HEX_DIGITS>}`.
    fn handle_unicode_escape_sequence(&mut self, c: char) -> Result<State, ScanError> {
        // FIXME -- should enforce first character in this state to be {
        // at the moment invalid hex digit catches all
        match c {
            CURLY_BRACKET_LEFT => {
                if matches!(self.stack.last(), Some(&CURLY_BRACKET_LEFT)) {
                    Err(self.scan_error(ScanErrorKind::UnexpectedUnicodeOpeningCurlyBracket))
                } else {
                    self.stack.push(c);
                    Ok(State::UnicodeEscapeSequence)
                }
            }
            CURLY_BRACKET_RIGHT => {
                if !matches!(self.stack.last(), Some(&CURLY_BRACKET_LEFT)) {
                    return Err(self.scan_error(ScanErrorKind::UnbalancedCurlyBracket));
                }

                let codepoint = mem::take(&mut self.utf_codepoint);
                let Some(character) = char::from_u32(codepoint) else {
                    return Err(self.scan_error(ScanErrorKind::InvalidCodepoint(codepoint)));
                };

                if character.is_control() && !character.is_whitespace() {
                    return Err(
                        self.scan_error(ScanErrorKind::UnexpectedControlCharacter(character))
                    );
                }

                // FIXME popping here may leave in inconsistent state
                self.stack.pop();
                self.buf.push(character);
                Ok(State::EscapedStringLiteral)
            }
            c => {
                if let Some(value) = c.to_digit(16) {
                    self.utf_codepoint = self.utf_codepoint * 16 + value;
                    Ok(State::UnicodeEscapeSequence)
                } else {
                    Err(self.scan_error(ScanErrorKind::InvalidHexDigit(c)))
                }
            }
        }
    }

    fn in_literal_backslash(&mut self, c: char) -> State {
        if self.escape {
            self.escape = false;
            self.buf.push(c);
        } else {
            self.escape = true;
        }
        State::EscapedStringLiteral
    }

    /// The scanner is in `EscapedStringLiteral` state and got a '"' character.
    ///
    /// - If the scanner is in escape state, the quotes are part of the content of
    ///   the string literal.
    /// - If the scanner has a '"' on top of the stack, the current character is
    ///   the closing quotes, so produce a `StringLiteral` token.
    /// - In any other case, the presence of '"' is unexpected.
    fn handle_quotes_in_literal(&mut self, tokens: &mut Vec<Token>) -> Result<State, ScanError> {
        if self.escape {
            self.escape = false;
            self.buf.push(DOUBLE_QUOTES);
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
            // This function is invoked when the state is `EscapedStringLiteral`
            // and current character is `"`. If the state is
            // `EscapedStringLiteral` there must be a matching `"` on top of the
            // stack, otherwise it means that the scanner is in an inconsistent
            // state.
            unreachable!(
                "fatal@{},{}: inconsistent `EscapedStringLiteral` state. Unbalanced closing quotes.",
                self.row, self.col
            )
        }
    }

    /// The scanner receiving a `u` character checks whether it's a `\u` escape
    /// sequence or just the `u` ASCII character.
    fn in_literal_unicode(&mut self, c: char) -> State {
        if self.escape {
            self.escape = false;
            State::UnicodeEscapeSequence
        } else {
            self.buf.push(c);
            State::EscapedStringLiteral
        }
    }

    fn in_literal_whitespace(&mut self, c: char, escaped: char) -> State {
        if self.escape {
            self.escape = false;
            self.buf.push(escaped);
        } else {
            self.buf.push(c);
        }
        State::EscapedStringLiteral
    }

    fn scan_error(&self, kind: ScanErrorKind) -> ScanError {
        ScanError::new(kind, self.row, self.col)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let tokens = Scanner::new().scan("").expect("Should parse empty string.");
        assert_eq!(
            tokens,
            vec![],
            "Expected empty Token vector for empty string."
        );

        let tokens = Scanner::new()
            .scan(" \n \r \t ")
            .expect("Should parse string containing whitespaces only.");
        assert_eq!(
            tokens,
            vec![],
            "Expected empty Token vector for string containing whitespaces only."
        );
    }

    #[test]
    fn token_identifier() {
        let tokens = Scanner::new()
            .scan("foo foo_bar foo_123_bar アキラ")
            .expect("Should parse alphanumeric UTF-8 identifiers connected by underscores.");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Identifier("foo_bar".to_string()),
                Token::Identifier("foo_123_bar".to_string()),
                Token::Identifier("アキラ".to_string()),
            ]
        );

        let tokens = Scanner::new()
            .scan("foo\tアキラ")
            .expect("Should parse identifiers separated by whitespace characters.");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("foo".to_string()),
                Token::Identifier("アキラ".to_string()),
            ]
        );

        let tokens = Scanner::new()
            .scan(" valid")
            .expect("Should parse a string starting with whitespace characters.");
        assert_eq!(tokens, vec![Token::Identifier("valid".to_string())]);

        let tokens = Scanner::new()
            .scan("_invalid")
            .expect_err("Identifier cannot start with symbol '_'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,1: invalid symbol. Expected '\"', '-' or alphabetic UTF-8 character, got '_'."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("1nvalid")
            .expect_err("Identifier cannot start with number.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,1: invalid symbol. Expected '\"', '-' or alphabetic UTF-8 character, got '1'."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("inv@lid")
            .expect_err("Identifier cannot contain symbol '@'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,4: unexpected character '@'. Identifier can only contain alphanumeric or '_' characters.".to_string()
        );
    }

    #[test]
    fn token_long_option() {
        let tokens = Scanner::new()
            .scan("--123")
            .expect("Should parse long options containing only numeric characters.");
        assert_eq!(tokens, vec![Token::LongOption("123".to_string()),]);

        let tokens = Scanner::new()
            .scan("--foo")
            .expect("Should parse long options containing alphabetic characters.");
        assert_eq!(tokens, vec![Token::LongOption("foo".to_string()),]);

        let tokens = Scanner::new()
            .scan("--foo bar")
            .expect("Should parse long option with argument as identifier.");
        assert_eq!(
            tokens,
            vec![
                Token::LongOption("foo".to_string()),
                Token::Identifier("bar".to_string())
            ]
        );

        let tokens = Scanner::new()
            .scan("--foo \"bar\"")
            .expect("Should parse long option with argument as string literal.");
        assert_eq!(
            tokens,
            vec![
                Token::LongOption("foo".to_string()),
                Token::StringLiteral("bar".to_string())
            ]
        );

        let tokens = Scanner::new()
            .scan("--foo=bar")
            .expect("Should parse long option using equals sign as separator with identifier.");
        assert_eq!(
            tokens,
            vec![
                Token::LongOption("foo".to_string()),
                Token::Identifier("bar".to_string())
            ]
        );

        let tokens = Scanner::new()
            .scan("--foo=\"bar\"")
            .expect("Should parse long option using equals sign as separator with string literal.");
        assert_eq!(
            tokens,
            vec![
                Token::LongOption("foo".to_string()),
                Token::StringLiteral("bar".to_string())
            ]
        );

        let tokens = Scanner::new()
            .scan("--foo-bar")
            .expect("Should parse hyphened long options.");
        assert_eq!(tokens, vec![Token::LongOption("foo-bar".to_string()),]);

        let tokens = Scanner::new()
            .scan("--foo_bar")
            .expect("Should parse underscored long options.");
        assert_eq!(tokens, vec![Token::LongOption("foo_bar".to_string()),]);

        let tokens = Scanner::new()
            .scan("--foo2bar")
            .expect("Should parse long options including numbers.");
        assert_eq!(tokens, vec![Token::LongOption("foo2bar".to_string()),]);

        let tokens = Scanner::new()
            .scan("--アキラ")
            .expect("Should parse long options including UTF-8 characters.");
        assert_eq!(tokens, vec![Token::LongOption("アキラ".to_string()),]);

        let tokens = Scanner::new()
            .scan("--foo--bar")
            .expect_err("Long option cannot contain '--'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: unexpected character '-'. Invalid '--' sequence in long option."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("--foo-_bar")
            .expect_err("Long option cannot include '-_'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: unexpected character '_'. Invalid '-_' sequence in long option."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("--foo_-bar")
            .expect_err("Long option cannot include '_-'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: unexpected character '-'. Invalid '_-' sequence in long option."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("--foo__bar")
            .expect_err("Long option cannot include '__'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: unexpected character '_'. Invalid '__' sequence in long option."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("---foo")
            .expect_err("Long option cannot start with '-'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unexpected character '-'. Long option starting character must be alphanumeric.".to_string()
        );

        let tokens = Scanner::new()
            .scan("--_foo")
            .expect_err("Long options cannot start with '_'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unexpected character '_'. Long option starting character must be alphanumeric.".to_string()
        );

        let tokens = Scanner::new()
            .scan("--=foo")
            .expect_err("Long options cannot start with '='.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unexpected character '='. Long option starting character must be alphanumeric.".to_string()
        );

        let tokens = Scanner::new()
            .scan("--f-=foo")
            .expect_err("Long option cannot include '-='.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,5: unexpected character '='. Invalid '-=' sequence in long option."
                .to_string()
        );

        let tokens = Scanner::new()
            .scan("--foo$bar")
            .expect_err("Long option cannot include non-alphanumeric characters.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,6: unexpected character '$'. Long option can only contain alphanumeric or '_', '-', '=' characters.".to_string()
        );

        // this fails because identifier cannot start with '=', as the first '='
        // is the delimiter
        let tokens = Scanner::new()
            .scan("--foo==bar")
            .expect_err("Identifier cannot start with symbol '='.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: invalid symbol. Expected '\"', '-' or alphabetic UTF-8 character, got '='."
                .to_string()
        );
    }

    #[test]
    fn token_short_option() {
        let tokens = Scanner::new().scan("-a -ア -1").expect(
            "Should parse short options that include a single UTF-8 alphanumeric character.",
        );
        assert_eq!(
            tokens,
            vec![
                Token::ShortOption("a".to_string()),
                Token::ShortOption("ア".to_string()),
                Token::ShortOption("1".to_string())
            ]
        );

        let tokens = Scanner::new()
            .scan("-")
            .expect_err("Should reject empty short options.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,1: unexpected end of input after '-' character.".to_string()
        );

        let tokens = Scanner::new()
            .scan("-?")
            .expect_err("Non-alphanumeric short option.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,2: unexpected character '?'. Expected one alphanumeric or '-' to continue to Short or Long option.".to_string()
        );

        let tokens = Scanner::new()
            .scan("- ")
            .expect_err("Whitespace in short option '- '");
        assert_eq!(
            *tokens.to_string(),
            "error@1,2: unexpected character ' '. Expected one alphanumeric or '-' to continue to Short or Long option.".to_string()
        );

        let tokens = Scanner::new()
            .scan("-\n")
            .expect_err("Whitespace in short option '-\\n'");
        assert_eq!(
            *tokens.to_string(),
            "error@2,1: unexpected character '\n'. Expected one alphanumeric or '-' to continue to Short or Long option.".to_string()
        );

        let tokens = Scanner::new()
            .scan("-ab")
            .expect_err("Short option accepts only one character.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unexpected character 'b'. Short option can contain only one UTF-8 alphanumeric character.".to_string()
        );
    }

    #[test]
    fn token_string_literal() {
        let tokens = Scanner::new().scan("\"\"").expect("String literal: empty.");
        assert_eq!(tokens, vec![Token::StringLiteral("".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\\\"\"")
            .expect("String literal: single quote.");
        assert_eq!(tokens, vec![Token::StringLiteral("\"".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\\\\\"")
            .expect("String literal: backslash.");
        assert_eq!(tokens, vec![Token::StringLiteral("\\".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\t\n\r\"")
            .expect("String literal: whitespaces.");
        assert_eq!(tokens, vec![Token::StringLiteral("\t\n\r".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\\u{9}\\u{a}\\u{d}\\u{20}\"")
            .expect("String literal: whitespace escape sequences UTF-8.");
        assert_eq!(tokens, vec![Token::StringLiteral("\t\n\r ".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\\t\\n\\r\"")
            .expect("String literal: whitespace escape sequences.");
        assert_eq!(tokens, vec![Token::StringLiteral("\t\n\r".to_string())]);

        let tokens = Scanner::new().scan("\"v@lid\ts3quence アキラ\"").expect(
            "String literal: supports alphanumeric UTF-8 characters, whitespaces and symbols.",
        );
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("v@lid\ts3quence アキラ".to_string())]
        );

        let tokens = Scanner::new()
            .scan("\"\\u{30a2}\\u{30ad}\\u{30e9}\"")
            .expect("String literal: UTF-8 escape sequences.");
        assert_eq!(tokens, vec![Token::StringLiteral("アキラ".to_string())]);

        let tokens = Scanner::new()
            .scan("\"\\u{30a2}\\u{30ad}\\u{30e9}\"")
            .expect("String literal: UTF-8 escape sequences vs Rust");
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("\u{30a2}\u{30ad}\u{30e9}".to_string())]
        );

        let tokens = Scanner::new()
            .scan("\"\\u{d801}\"")
            .expect_err("String literal: invalid codepoint.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,9: invalid UTF-8 codepoint 'd801'".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\z\"")
            .expect_err("Invalid escape sequence, only whitespaces are supported.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unexpected character 'z'. The sequence '\\z' is not valid, only '\\n', '\\r', '\\t' are supported.".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\u{8}\"")
            .expect_err("Unexpected control escape sequence, only whitespaces are supported.");
        assert_eq!(*tokens.to_string(), "error@1,2: unexpected control character '\\u{8}'. Only '\\n', '\\r', '\\t' are supported.".to_string());

        let tokens = Scanner::new()
            .scan("\"\\u\"")
            .expect_err("Invalid UTF-8 escape sequence");
        assert_eq!(
            *tokens.to_string(),
            // FIXME -- invalid hex digit is out of scope because curly is expected
            "error@1,4: unexpected character '\"'. Invalid hex digit.".to_string()
        );

        let tokens = Scanner::new().scan("\"").expect_err("Unbalanced quotes");
        assert_eq!(
            *tokens.to_string(),
            "error@1,1: unbalanced closing quote '\"'.".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\\"")
            .expect_err("Unbalanced quotes (2)");
        assert_eq!(
            *tokens.to_string(),
            "error@1,3: unbalanced closing quote '\"'.".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"foo")
            .expect_err("Unbalanced quotes (3)");
        assert_eq!(
            *tokens.to_string(),
            "error@1,4: unbalanced closing quote '\"'.".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\u{7f")
            .expect_err("Unbalanced curly bracket");
        assert_eq!(
            *tokens.to_string(),
            "error@1,6: unbalanced closing curly bracket '}'".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\u{7f{")
            .expect_err("Expected hex digit, got '}'.");
        assert_eq!(
            *tokens.to_string(),
            "error@1,7: unexpected character '{'. Expected codepoint hex digit.".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\u}")
            .expect_err("Unbalanced curly bracket");
        assert_eq!(
            *tokens.to_string(),
            "error@1,4: unbalanced closing curly bracket '}'".to_string()
        );

        let tokens = Scanner::new()
            .scan("\"\\u{8}")
            .expect_err("Unbalanced curly bracket");
        assert_eq!(
            *tokens.to_string(),
            "error@1,6: unexpected control character '\\u{8}'. Only '\\n', '\\r', '\\t' are supported.".to_string()
        );
    }
}
