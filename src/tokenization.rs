pub mod num_lit_state;

use crate::cursor::Cursor;
use num_lit_state::NumericLiteralState;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub(crate) buffer: String,
    pub(crate) cursor: Cursor,
    pub(crate) state: State,
}

#[derive(Debug)]
pub enum CharClass {
    Alphabetic,
    Numeric,
    Symbol,
    SymbolIdentifier,
    SymbolNumericLiteral,
    Whitespace,
    Invalid,
    None,
}

#[derive(Debug)]
pub enum Action {
    Append,
    EmitToken,
    Noop,
}

#[derive(Debug, PartialEq)]
pub enum State {
    Ready,
    Parse(ParseState),
    Complete(fn(String) -> Token),
    End,
}

impl Default for State {
    fn default() -> State {
        State::Ready
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseState {
    Identifier,
    NumericLiteral(NumericLiteralState),
    Symbol,
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    Symbol(String),
    Whitespace(String),
}
