#[derive(Debug, PartialEq)]
pub struct Utf8State {
    codepoint: u32,
}

impl Utf8State {
    pub fn add(&mut self, c: char) {
        self.codepoint = self.codepoint * 16 + c.to_digit(16).unwrap();
    }

    pub fn codepoint(&self) -> u32 {
        self.codepoint
    }
}

impl Default for Utf8State {
    fn default() -> Self {
        Self { codepoint: 0 }
    }
}
