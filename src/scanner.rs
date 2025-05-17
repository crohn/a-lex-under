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

    #[test]
    fn t() {
        let scanner = Scanner::new("hello world");

        for cursor in scanner {
            println!("{:?}", cursor);
        }
    }

    #[test]
    fn u() {
        let scanner = Scanner::new("hello\nworld");

        for cursor in scanner {
            println!("{:?}", cursor);
        }
    }
}
