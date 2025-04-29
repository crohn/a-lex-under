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
    InvalidStartOfToken(char),
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
            ScanErrorKind::InvalidStartOfToken(c) => write!(
                f,
                "error: expected '\"' | '-' | UTF-8 alphabetic | whitespace, found '{}'\n --> ({}:{})",
                c, self.row, self.col
            ),
        }
    }
}
