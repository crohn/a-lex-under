use super::State;
use crate::cursor::Cursor;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Codepoint,
    EndOfInput,
    Escape,
    Invalid,
    Unbalance,
}

#[derive(Debug, PartialEq)]
pub struct Error {
    buffer: String,
    cursor: Cursor,
    kind: ErrorKind,
    state: State,
}

impl Error {
    pub fn new(kind: ErrorKind, state: State, buffer: String, cursor: Cursor) -> Self {
        Self {
            buffer,
            cursor,
            kind,
            state,
        }
    }
}
