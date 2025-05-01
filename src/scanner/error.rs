use std::error::Error;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
pub enum ScanErrorKind {
    InvalidIdentifier(char),
    InvalidLiteralEscape(char),
    InvalidLongOption(char),
    InvalidLongOptionSequence(char, char),
    InvalidLongOptionStart(char),
    InvalidTokenStart(char),
    UnbalancedCurlyBracket,
    UnbalancedDoubleQuotes,
    UnexpectedContolCharacter(char),
    UnexpectedOptionContinuation(char),
    UnexpectedShortOptionContinuation(char),
    UnexpectedUnicodeOpeningCurlyBracket,
}

#[derive(Debug)]
pub struct ScanError {
    col: usize,
    kind: ScanErrorKind,
    row: usize,
}

impl ScanError {
    pub fn new(kind: ScanErrorKind, row: usize, col: usize) -> ScanError {
        ScanError { col, kind, row }
    }
}

impl Error for ScanError {}

impl Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ScanErrorKind::InvalidIdentifier(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Identifier can only contain alphanumeric or '_' characters.",
                self.row, self.col, c
            ),
            ScanErrorKind::InvalidLiteralEscape(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. The sequence '\\{}' is not valid, only '\\n', '\\r', '\\t' are supported.",
                self.row, self.col, c, c
            ),
            ScanErrorKind::InvalidLongOption(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Long option can only contain alphanumeric or '_', '-', '=' characters.",
                self.row, self.col, c
            ),
            ScanErrorKind::InvalidLongOptionSequence(p, c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Invalid '{}{}' sequence in long option.",
                self.row, self.col, c, p, c
            ),
            ScanErrorKind::InvalidLongOptionStart(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Long option starting character must be alphanumeric.",
                self.row, self.col, c
            ),
            ScanErrorKind::InvalidTokenStart(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Token starting character must be alphabetic, \" or -.",
                self.row, self.col, c
            ),
            ScanErrorKind::UnbalancedCurlyBracket => {
                write!(
                    f,
                    "error@{},{}: unbalanced closing curly bracket '}}'",
                    self.row, self.col
                )
            }
            ScanErrorKind::UnbalancedDoubleQuotes => write!(
                f,
                "error@{},{}: unbalanced closing quote '\"'.",
                self.row, self.col
            ),
            ScanErrorKind::UnexpectedOptionContinuation(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Expected one alphanumeric or '-' to continue to Short or Long option.",
                self.row, self.col, c
            ),
            ScanErrorKind::UnexpectedShortOptionContinuation(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Short option can contain only one UTF-8 alphanumeric character.",
                self.row, self.col, c
            ),
            ScanErrorKind::UnexpectedContolCharacter(c) => write!(
                f,
                "error@{},{}: unexpected control character '{}'. Only '\\n', '\\r', '\\t' are supported.",
                self.row,
                self.col,
                c.escape_unicode()
            ),
            ScanErrorKind::UnexpectedUnicodeOpeningCurlyBracket => {
                write!(
                    f,
                    "error@{},{}: unexpected character '{{'. Expected codepoint hex digit.",
                    self.row, self.col
                )
            }
        }
    }
}
