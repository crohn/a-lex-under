pub mod char_class;
pub mod error;
pub mod num_lit_state;
pub mod str_lit_state;

use num_lit_state::NumericLiteralState;
use str_lit_state::StringLiteralState;

#[derive(Debug)]
pub enum Action {
    Append(char),
    EmitToken,
    Noop,
    Pop,
    Push,
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
    StringLiteral(StringLiteralState),
    Symbol,
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
