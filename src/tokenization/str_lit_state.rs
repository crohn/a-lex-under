#[derive(Debug, PartialEq)]
pub struct StringLiteralState {
    pub escape: bool,
}

impl StringLiteralState {
    pub fn to_escaped(&self, c: char) -> Result<char, ()> {
        if !self.escape {
            panic!("Expected 'escape' state when trying to escape a character.");
        }

        match c {
            '"' => Ok('"'),
            '\\' => Ok('\\'),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            _ => Err(()),
        }
    }
}

impl Default for StringLiteralState {
    fn default() -> StringLiteralState {
        StringLiteralState { escape: false }
    }
}
