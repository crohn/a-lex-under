use crate::cursor::Cursor;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidCharacter { buffer: String, cursor: Cursor },
}
