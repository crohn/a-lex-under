use std::error::Error;
use std::fmt::{self, Display};

#[derive(Debug)]
pub enum ScanErrorKind {
    InvalidCodepoint(u32),
    InvalidHexDigit(char),
    InvalidIdentifier(char),
    InvalidLiteralEscape(char),
    InvalidLongOption(char),
    InvalidLongOptionSequence(char, char),
    InvalidLongOptionStart(char),
    InvalidTokenStart(char),
    UnbalancedCurlyBracket,
    UnbalancedDoubleQuotes,
    UnexpectedControlCharacter(char),
    UnexpectedOptionContinuation(char),
    UnexpectedOptionEndOfInput,
    UnexpectedShortOptionContinuation(char),
    UnexpectedUnicodeOpeningCurlyBracket,
}

#[derive(Debug)]
pub struct ScanError {
    col: usize,
    kind: ScanErrorKind,
    row: usize,
}

impl ScanError {
    pub fn new(kind: ScanErrorKind, row: usize, col: usize) -> ScanError {
        ScanError { col, kind, row }
    }
}

impl Error for ScanError {}

impl Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error@{},{}: ", self.row, self.col)?;

        match self.kind {
            ScanErrorKind::InvalidCodepoint(codepoint) => {
                write!(f, "invalid UTF-8 codepoint '{:x}'", codepoint)
            }
            ScanErrorKind::InvalidHexDigit(c) => {
                write!(f, "unexpected character '{}'. Invalid hex digit.", c)
            }
            ScanErrorKind::InvalidIdentifier(c) => write!(
                f,
                "unexpected character '{}'. Identifier can only contain alphanumeric or '_' characters.",
                c
            ),
            ScanErrorKind::InvalidLiteralEscape(c) => write!(
                f,
                "unexpected character '{}'. The sequence '\\{}' is not valid, only '\\n', '\\r', '\\t' are supported.",
                c, c
            ),
            ScanErrorKind::InvalidLongOption(c) => write!(
                f,
                "unexpected character '{}'. Long option can only contain alphanumeric or '_', '-', '=' characters.",
                c
            ),
            ScanErrorKind::InvalidLongOptionSequence(p, c) => write!(
                f,
                "unexpected character '{}'. Invalid '{}{}' sequence in long option.",
                c, p, c
            ),
            ScanErrorKind::InvalidLongOptionStart(c) => write!(
                f,
                "unexpected character '{}'. Long option starting character must be alphanumeric.",
                c
            ),
            ScanErrorKind::InvalidTokenStart(c) => write!(
                f,
                "unexpected character '{}'. Token starting character must be alphabetic, \" or -.",
                c
            ),
            ScanErrorKind::UnbalancedCurlyBracket => {
                write!(f, "unbalanced closing curly bracket '}}'")
            }
            ScanErrorKind::UnbalancedDoubleQuotes => {
                write!(f, "unbalanced closing quote '\"'.")
            }
            ScanErrorKind::UnexpectedControlCharacter(c) => write!(
                f,
                "unexpected control character '{}'. Only '\\n', '\\r', '\\t' are supported.",
                c.escape_unicode()
            ),
            ScanErrorKind::UnexpectedOptionContinuation(c) => write!(
                f,
                "unexpected character '{}'. Expected one alphanumeric or '-' to continue to Short or Long option.",
                c
            ),
            ScanErrorKind::UnexpectedOptionEndOfInput => {
                write!(f, "unexpected end of input after '-' character.",)
            }
            ScanErrorKind::UnexpectedShortOptionContinuation(c) => write!(
                f,
                "unexpected character '{}'. Short option can contain only one UTF-8 alphanumeric character.",
                c
            ),
            ScanErrorKind::UnexpectedUnicodeOpeningCurlyBracket => {
                write!(
                    f,
                    "unexpected character '{{'. Expected codepoint hex digit."
                )
            }
        }
    }
}
