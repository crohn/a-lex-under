pub const BACKSLASH: char = '\\';
pub const DOT: char = '.';
pub const DOUBLE_QUOTE: char = '"';
pub const HYPHEN: char = '-';
pub const LOWER_E: char = 'e';
pub const PLUS: char = '+';
pub const UNDERSCORE: char = '_';
pub const UPPER_E: char = 'E';

#[derive(Debug)]
pub enum CharClass {
    Alphabetic(char),
    Numeric(char),
    Symbol(char),
    SymbolIdentifier(char),
    SymbolNumericLiteral(char),
    SymbolStringLiteral,
    Whitespace(char),
    Invalid,
    None,
}

impl CharClass {
    pub fn classify(c: Option<char>) -> CharClass {
        match c {
            Some(UNDERSCORE) => CharClass::SymbolIdentifier(UNDERSCORE),
            Some(DOT) => CharClass::SymbolNumericLiteral(DOT),
            Some(PLUS) => CharClass::SymbolNumericLiteral(PLUS),
            Some(HYPHEN) => CharClass::SymbolNumericLiteral(HYPHEN),
            Some(DOUBLE_QUOTE) => CharClass::SymbolStringLiteral,
            Some(c) if c.is_numeric() => CharClass::Numeric(c),
            Some(c) if c.is_alphabetic() => CharClass::Alphabetic(c),
            Some(c) if c.is_whitespace() => CharClass::Whitespace(c),
            Some(c) if !c.is_control() => CharClass::Symbol(c),
            Some(_) => CharClass::Invalid,
            None => CharClass::None,
        }
    }
}
