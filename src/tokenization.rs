pub mod char_class;
pub mod error;
pub mod num_lit_state;
pub mod str_lit_state;
pub mod utf8_state;

use num_lit_state::NumericLiteralState;
use str_lit_state::StringLiteralState;
use utf8_state::Utf8State;

#[derive(Debug, PartialEq)]
pub enum Action {
    Noop,         // state
    Skip,         // state, scan
    Append(char), // state, scan, buffer
    Pop(char),    // state, scan, stack
    Push(char),   // state, scan, stack
}

#[derive(Debug, PartialEq)]
pub enum State {
    Ready,
    Parse(ParseState),
    Complete(fn(String) -> Token),
    Codepoint(u32),
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
    StringLiteral(StringLiteralState),
    Symbol,
    Utf8(Utf8State),
    Whitespace,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    StringLiteral(String),
    Symbol(String),
    Whitespace(String),
}
