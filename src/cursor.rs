const NEWLINE: char = '\n';

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Cursor {
    col: usize,
    row: usize,
    prev: Option<char>,
    curr: Option<char>,
    next: Option<char>,
}

impl Default for Cursor {
    fn default() -> Self {
        Self {
            col: 0,
            row: 1,
            prev: None,
            curr: None,
            next: None,
        }
    }
}

impl Cursor {
    pub fn col(&self) -> usize {
        self.col
    }
    pub fn row(&self) -> usize {
        self.row
    }
    pub fn prev(&self) -> Option<char> {
        self.prev
    }
    pub fn curr(&self) -> Option<char> {
        self.curr
    }
    pub fn next(&self) -> Option<char> {
        self.next
    }

    pub fn step(&mut self, curr: Option<char>, next: Option<&char>) -> Option<()> {
        self.prev = self.curr;
        self.curr = curr;
        self.next = next.copied();

        if self.prev == Some(NEWLINE) {
            self.row += 1;
            self.col = 1;
        } else if self.curr.is_some() {
            self.col += 1;
        }

        self.curr.map(|_| ())
    }
}

pub struct CursorBuilder {
    row: usize,
    col: usize,
    prev: Option<char>,
    curr: Option<char>,
    next: Option<char>,
}

impl Default for CursorBuilder {
    fn default() -> Self {
        Self {
            col: 0,
            row: 1,
            prev: None,
            curr: None,
            next: None,
        }
    }
}

impl CursorBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn row(mut self, row: usize) -> Self {
        self.row = row;
        self
    }

    pub fn col(mut self, col: usize) -> Self {
        self.col = col;
        self
    }

    pub fn prev(mut self, prev: char) -> Self {
        self.prev = Some(prev);
        self
    }

    pub fn curr(mut self, curr: char) -> Self {
        self.curr = Some(curr);
        self
    }

    pub fn next(mut self, next: char) -> Self {
        self.next = Some(next);
        self
    }

    pub fn build(self) -> Cursor {
        Cursor {
            row: self.row,
            col: self.col,
            prev: self.prev,
            curr: self.curr,
            next: self.next,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn default() {
        let cursor = Cursor::default();
        assert_eq!(cursor.row, 1);
        assert_eq!(cursor.col, 0);
        assert_eq!(cursor.prev, None);
        assert_eq!(cursor.curr, None);
        assert_eq!(cursor.next, None);
    }

    #[test]
    fn cursor_builder() {
        let builder = CursorBuilder::new();
        assert_eq!(builder.row, 1);
        assert_eq!(builder.col, 0);
        assert_eq!(builder.prev, None);
        assert_eq!(builder.curr, None);
        assert_eq!(builder.next, None);
    }

    #[test]
    fn cursor_builder_build() {
        let cursor = CursorBuilder::new()
            .row(8)
            .col(4)
            .prev('a')
            .curr('@')
            .next('g')
            .build();
        assert_eq!(cursor.row, 8);
        assert_eq!(cursor.col, 4);
        assert_eq!(cursor.prev, Some('a'));
        assert_eq!(cursor.curr, Some('@'));
        assert_eq!(cursor.next, Some('g'));
    }
}
