use std::mem;

#[derive(Debug, PartialEq)]
pub struct NumericLiteralState {
    has_dot: bool,
    has_exp: bool,
}

impl Default for NumericLiteralState {
    fn default() -> Self {
        Self {
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

pub struct NumericLiteralStateBuilder {
    has_dot: bool,
    has_exp: bool,
}

impl NumericLiteralStateBuilder {
    pub fn new() -> Self {
        Self {
            has_dot: false,
            has_exp: false,
        }
    }

    pub fn has_dot(mut self) -> Self {
        self.has_dot = true;
        self
    }

    pub fn has_exp(mut self) -> Self {
        self.has_exp = true;
        self
    }

    pub fn build(self) -> NumericLiteralState {
        NumericLiteralState {
            has_dot: self.has_dot,
            has_exp: self.has_exp,
        }
    }
}
