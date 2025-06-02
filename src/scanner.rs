use crate::cursor::{Cursor, CursorBuilder};
use std::iter::{Iterator, Peekable};
use std::str::Chars;

#[derive(Debug)]
pub struct Scanner<'a> {
    cursor: Cursor,
    iterator: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut iterator = input.chars().peekable();
        let cursor = iterator.peek().cloned().map_or_else(
            || CursorBuilder::new().build(),
            |c| CursorBuilder::new().next(c).build(),
        );

        Scanner { cursor, iterator }
    }

    pub fn cursor(&self) -> &Cursor {
        &self.cursor
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Cursor;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor
            .step(self.iterator.next(), self.iterator.peek())
            .map(|()| self.cursor)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cursor::CursorBuilder;

    #[test]
    fn scan() {
        let cursors: Vec<Cursor> = Scanner::new("h\nwb\u{18}").collect();
        assert_eq!(cursors.len(), 5);
        assert_eq!(
            cursors,
            vec![
                CursorBuilder::new().col(1).curr('h').next('\n').build(),
                CursorBuilder::new()
                    .col(2)
                    .prev('h')
                    .curr('\n')
                    .next('w')
                    .build(),
                CursorBuilder::new()
                    .row(2)
                    .col(1)
                    .prev('\n')
                    .curr('w')
                    .next('b')
                    .build(),
                CursorBuilder::new()
                    .row(2)
                    .col(2)
                    .prev('w')
                    .curr('b')
                    .next('\u{18}')
                    .build(),
                CursorBuilder::new()
                    .row(2)
                    .col(3)
                    .prev('b')
                    .curr('\u{18}')
                    .build(),
            ]
        );
    }
}
