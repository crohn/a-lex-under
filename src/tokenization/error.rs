use crate::cursor::Cursor;

use super::State;

#[derive(Debug, PartialEq)]
pub struct Error {
    buffer: String,
    cursor: Cursor,
    kind: ErrorKind,
    state: State,
}

impl Error {
    pub fn new(kind: ErrorKind, state: State, buffer: String, cursor: Cursor) -> Error {
        Error {
            buffer,
            cursor,
            kind,
            state,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Codepoint,
    EndOfInput,
    Escape,
    Invalid,
    Unbalance,
}
