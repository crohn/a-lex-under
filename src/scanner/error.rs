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
