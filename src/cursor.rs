#[derive(Clone, Copy, Debug)]
pub struct Cursor {
    pub(crate) col: usize,
    pub(crate) curr: Option<char>,
    pub(crate) prev: Option<char>,
    pub(crate) row: usize,
}

impl Default for Cursor {
    fn default() -> Cursor {
        Cursor {
            col: 0,
            prev: None,
            curr: None,
            row: 1,
        }
    }
}
