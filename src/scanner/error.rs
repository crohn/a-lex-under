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
    InvalidTokenStart(char),
    UnexpecetdContolCharacter(char),
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
            ScanErrorKind::InvalidTokenStart(c) => write!(
                f,
                "error@{},{}: unexpected character '{}'. Token starting character must be alphabetic, \" or -.",
                self.row, self.col, c
            ),
            ScanErrorKind::UnexpecetdContolCharacter(c) => write!(
                f,
                "error@{},{}: unexpected control character '{}'. Only '\\n', '\\r', '\\t' are supported.",
                self.row,
                self.col,
                c.escape_unicode()
            ),
        }
    }
}
