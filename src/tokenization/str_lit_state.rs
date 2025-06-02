#[derive(Debug, PartialEq)]
pub struct StringLiteralState {
    pub escape: bool,
}

impl Default for StringLiteralState {
    fn default() -> Self {
        Self { escape: false }
    }
}

impl StringLiteralState {
    pub fn to_escaped(&self, c: char) -> Result<char, ()> {
        if !self.escape {
            return Err(());
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
