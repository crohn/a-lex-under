pub const BACKSLASH: char = '\\';
pub const DOT: char = '.';
pub const DOUBLE_QUOTE: char = '"';
pub const HYPHEN: char = '-';
pub const LEFT_CURLY: char = '{';
pub const LOWER_E: char = 'e';
pub const LOWER_U: char = 'u';
pub const PLUS: char = '+';
pub const RIGHT_CURLY: char = '}';
pub const UNDERSCORE: char = '_';
pub const UPPER_E: char = 'E';
pub const UPPER_U: char = 'U';

#[derive(Debug)]
pub enum CharClass {
    Alphabetic(char),
    Numeric(char),
    Symbol(char),
    SymbolIdentifier(char),
    SymbolNumericLiteral(char),
    SymbolStringLiteral(char),
    Whitespace(char),
    Invalid,
    None,
}

impl CharClass {
    pub fn is_balanced(a: Option<char>, b: char) -> bool {
        match (a, b) {
            (Some(DOUBLE_QUOTE), DOUBLE_QUOTE) => true,
            (Some('{'), '}') => true,
            _ => false,
        }
    }

    pub fn classify(c: Option<char>) -> Self {
        match c {
            Some(UNDERSCORE) => CharClass::SymbolIdentifier(UNDERSCORE),
            Some(c @ (DOT | HYPHEN | PLUS)) => CharClass::SymbolNumericLiteral(c),
            Some(c @ DOUBLE_QUOTE) => CharClass::SymbolStringLiteral(c),
            Some(c) if c.is_numeric() => CharClass::Numeric(c),
            Some(c) if c.is_alphabetic() => CharClass::Alphabetic(c),
            Some(c) if c.is_whitespace() => CharClass::Whitespace(c),
            Some(c) if !c.is_control() => CharClass::Symbol(c),
            Some(_) => CharClass::Invalid,
            None => CharClass::None,
        }
    }
}
