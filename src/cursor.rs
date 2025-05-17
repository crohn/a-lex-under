#[derive(Clone, Copy, Debug)]
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
