use crate::cursor::Cursor;
use std::mem;

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
pub struct NumericLiteralState {
    pub(crate) has_dot: bool,
    pub(crate) has_exp: bool,
}

impl Default for NumericLiteralState {
    fn default() -> NumericLiteralState {
        NumericLiteralState {
            has_dot: false,
            has_exp: false,
        }
    }
}

impl NumericLiteralState {
    pub fn apply_dot(&mut self) -> Result<Self, Self> {
        if self.has_dot || self.has_exp {
            Err(mem::take(self))
        } else {
            self.has_dot = true;
            Ok(mem::take(self))
        }
    }

    pub fn apply_exp(&mut self) -> Result<Self, Self> {
        if self.has_exp {
            Err(mem::take(self))
        } else {
            self.has_exp = true;
            Ok(mem::take(self))
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    Symbol(String),
    Whitespace(String),
}
