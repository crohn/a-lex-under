pub const DOT: char = '.';
pub const DOUBLE_QUOTE: char = '"';
pub const HYPHEN: char = '-';
pub const LOWER_E: char = 'e';
pub const PLUS: char = '+';
pub const UNDERSCORE: char = '_';
pub const UPPER_E: char = 'E';

#[derive(Debug)]
pub enum CharClass {
    Alphabetic,
    Numeric,
    Symbol,
    SymbolIdentifier,
    SymbolNumericLiteral,
    SymbolStringLiteral,
    Whitespace,
    Invalid,
    None,
}

impl CharClass {
    pub fn classify(c: Option<char>) -> CharClass {
        match c {
            Some(UNDERSCORE) => CharClass::SymbolIdentifier,
            Some(DOT) | Some(PLUS) | Some(HYPHEN) => CharClass::SymbolNumericLiteral,
            Some(DOUBLE_QUOTE) => CharClass::SymbolStringLiteral,
            Some(c) if c.is_numeric() => CharClass::Numeric,
            Some(c) if c.is_alphabetic() => CharClass::Alphabetic,
            Some(c) if c.is_whitespace() => CharClass::Whitespace,
            Some(c) if !c.is_control() => CharClass::Symbol,
            Some(_) => CharClass::Invalid,
            None => CharClass::None,
        }
    }
}
