#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Cursor {
    pub(crate) col: usize,
    pub(crate) row: usize,
    pub(crate) curr: Option<char>,
    pub(crate) prev: Option<char>,
    pub(crate) next: Option<char>,
}

impl Default for Cursor {
    fn default() -> Cursor {
        Cursor {
            col: 0,
            row: 1,
            prev: None,
            curr: None,
            next: None,
        }
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
    fn default() -> CursorBuilder {
        CursorBuilder {
            col: 0,
            row: 1,
            prev: None,
            curr: None,
            next: None,
        }
    }
}

impl CursorBuilder {
    pub fn new() -> CursorBuilder {
        CursorBuilder::default()
    }

    pub fn row(mut self, row: usize) -> CursorBuilder {
        self.row = row;
        self
    }

    pub fn col(mut self, col: usize) -> CursorBuilder {
        self.col = col;
        self
    }

    pub fn prev(mut self, prev: char) -> CursorBuilder {
        self.prev = Some(prev);
        self
    }

    pub fn curr(mut self, curr: char) -> CursorBuilder {
        self.curr = Some(curr);
        self
    }

    pub fn next(mut self, next: char) -> CursorBuilder {
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
