use crate::cursor::Cursor;
use std::iter::{Iterator, Peekable};
use std::str::Chars;

const NEWLINE: char = '\n';

#[derive(Debug)]
pub struct Scanner<'a> {
    cursor: Cursor,
    iterator: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Scanner<'a> {
        Scanner {
            cursor: Cursor::default(),
            iterator: input.chars().peekable(),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Cursor;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.prev = self.cursor.curr;

        if let Some(curr) = self.iterator.next() {
            self.cursor.curr = Some(curr);
            self.cursor.next = self.iterator.peek().cloned();

            if self.cursor.prev == Some(NEWLINE) {
                self.cursor.row += 1;
                self.cursor.col = 1;
            } else {
                self.cursor.col += 1;
            }

            Some(self.cursor)
        } else {
            self.cursor.curr = None;
            self.cursor.next = None;

            None
        }
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
