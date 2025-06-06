use crate::scanner::Scanner;
use crate::tokenization::char_class::{
    BACKSLASH, CharClass, DOT, HYPHEN, LEFT_CURLY, LOWER_E, LOWER_U, PLUS, RIGHT_CURLY, UPPER_E,
    UPPER_U,
};
use crate::tokenization::error::{Error, ErrorKind};
use crate::tokenization::num_lit_state::NumericLiteralState;
use crate::tokenization::str_lit_state::StringLiteralState;
use crate::tokenization::utf8_state::Utf8State;
use crate::tokenization::{Action, ParseState, State, Token};
use std::iter::Iterator;
use std::mem;

use Action::*;
use ParseState::*;
use State::*;

pub struct Tokenizer<'a> {
    scanner: Scanner<'a>,
    stack: Vec<char>,
    state: State,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner,
            stack: Vec::new(),
            state: State::default(),
            string_buffer: String::new(),
        }
    }

    fn error(&mut self, kind: ErrorKind, state: State) -> Error {
        Error::new(
            kind,
            state,
            mem::take(&mut self.string_buffer),
            self.scanner.cursor().clone(),
        )
    }

    fn transition_from_parse(
        &mut self,
        parse_state: &mut ParseState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match parse_state {
            Identifier => self.transition_from_parse_identifier(),
            NumericLiteral(num_lit_state) => self.transition_from_parse_num_lit(num_lit_state),
            StringLiteral(str_lit_state) => self.transition_from_parse_str_lit(str_lit_state),
            Symbol => Ok((Complete(Token::Symbol), Noop)),
            Utf8(utf8_state) => self.transition_from_parse_utf8(utf8_state),
            Whitespace => self.transition_from_parse_whitespace(),
        }
    }

    fn transition_from_parse_identifier(&mut self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Alphabetic(c) | CharClass::Numeric(c) | CharClass::SymbolIdentifier(c) => {
                Ok((Parse(Identifier), Append(c)))
            }
            CharClass::SymbolNumericLiteral(_)
            | CharClass::SymbolStringLiteral(_)
            | CharClass::Symbol(_)
            | CharClass::Whitespace(_)
            | CharClass::None => Ok((Complete(Token::Identifier), Noop)),
            CharClass::Invalid => Err((Parse(Identifier), ErrorKind::Invalid)),
        }
    }

    fn transition_from_parse_num_lit(
        &mut self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match (self.scanner.cursor().curr(), self.scanner.cursor().next()) {
            (_, Some(DOT)) => num_lit_state.apply_dot().map_or_else(
                |next| Err((Parse(NumericLiteral(next)), ErrorKind::Invalid)),
                |next| Ok((Parse(NumericLiteral(next)), Append(DOT))),
            ),
            (_, Some(c @ (LOWER_E | UPPER_E))) => num_lit_state.apply_exp().map_or_else(
                |next| Err((Parse(NumericLiteral(next)), ErrorKind::Invalid)),
                |next| Ok((Parse(NumericLiteral(next)), Append(c))),
            ),
            (Some(LOWER_E | UPPER_E), Some(c @ (PLUS | HYPHEN))) => {
                Ok((Parse(NumericLiteral(mem::take(num_lit_state))), Append(c)))
            }
            (curr, next) => match (CharClass::classify(curr), CharClass::classify(next)) {
                (CharClass::Numeric(_), CharClass::Whitespace(_) | CharClass::None) => {
                    Ok((Complete(Token::NumericLiteral), Noop))
                }
                (_, CharClass::Numeric(c) | CharClass::SymbolNumericLiteral(c)) => {
                    Ok((Parse(NumericLiteral(mem::take(num_lit_state))), Append(c)))
                }
                _ => Err((
                    Parse(NumericLiteral(mem::take(num_lit_state))),
                    ErrorKind::Invalid,
                )),
            },
        }
    }

    fn transition_from_parse_str_lit(
        &self,
        str_lit_state: &mut StringLiteralState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match (str_lit_state.escape, self.scanner.cursor().next()) {
            (false, Some(BACKSLASH)) => {
                str_lit_state.escape = true;
                Ok((Parse(StringLiteral(mem::take(str_lit_state))), Skip))
            }
            (false, c) => match CharClass::classify(c) {
                CharClass::None => Err((
                    Parse(StringLiteral(mem::take(str_lit_state))),
                    ErrorKind::EndOfInput,
                )),
                CharClass::Invalid => Err((
                    Parse(StringLiteral(mem::take(str_lit_state))),
                    ErrorKind::Invalid,
                )),
                CharClass::SymbolStringLiteral(c) => Ok((Complete(Token::StringLiteral), Pop(c))),
                CharClass::Alphabetic(c)
                | CharClass::Numeric(c)
                | CharClass::Symbol(c)
                | CharClass::SymbolIdentifier(c)
                | CharClass::SymbolNumericLiteral(c)
                | CharClass::Whitespace(c) => {
                    Ok((Parse(StringLiteral(mem::take(str_lit_state))), Append(c)))
                }
            },
            (true, None) => Err((
                Parse(StringLiteral(mem::take(str_lit_state))),
                ErrorKind::EndOfInput,
            )),
            (true, Some(LOWER_U | UPPER_U)) => Ok((Parse(Utf8(Utf8State::default())), Skip)),
            (true, Some(c)) => str_lit_state.to_escaped(c).map_or_else(
                |()| {
                    Err((
                        Parse(StringLiteral(StringLiteralState::default())),
                        ErrorKind::Escape,
                    ))
                },
                |c| {
                    Ok((
                        Parse(StringLiteral(StringLiteralState::default())),
                        Append(c),
                    ))
                },
            ),
        }
    }

    fn transition_from_parse_utf8(
        &self,
        utf8_state: &mut Utf8State,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match (self.stack.last(), self.scanner.cursor().next()) {
            (Some(&LEFT_CURLY), Some(c @ RIGHT_CURLY)) => {
                Ok((Codepoint(utf8_state.codepoint()), Pop(c)))
            }
            (Some(&LEFT_CURLY), Some(c @ ('0'..='9' | 'a'..='f' | 'A'..='F'))) => {
                utf8_state.add(c);
                Ok((Parse(Utf8(mem::take(utf8_state))), Skip))
            }
            (_, Some(c @ LEFT_CURLY)) => Ok((Parse(Utf8(mem::take(utf8_state))), Push(c))),
            _ => Err((Parse(Utf8(mem::take(utf8_state))), ErrorKind::Invalid)),
        }
    }

    fn transition_from_parse_whitespace(&self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Whitespace(c) => Ok((Parse(Whitespace), Append(c))),
            _ => Ok((Complete(Token::Whitespace), Noop)),
        }
    }

    fn transition_from_ready(&mut self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Alphabetic(c) | CharClass::SymbolIdentifier(c) => {
                Ok((Parse(Identifier), Append(c)))
            }
            CharClass::Numeric(c) => Ok((
                Parse(NumericLiteral(NumericLiteralState::default())),
                Append(c),
            )),
            CharClass::SymbolStringLiteral(c) => {
                Ok((Parse(StringLiteral(StringLiteralState::default())), Push(c)))
            }
            CharClass::Symbol(c) | CharClass::SymbolNumericLiteral(c) => {
                Ok((Parse(Symbol), Append(c)))
            }
            CharClass::Whitespace(c) => Ok((Parse(Whitespace), Append(c))),
            CharClass::None => Ok((End, Noop)),
            CharClass::Invalid => Err((Ready, ErrorKind::Invalid)),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut curr_state = mem::take(&mut self.state);

            let result = match &mut curr_state {
                Ready => self.transition_from_ready(),
                Parse(parse_state) => self.transition_from_parse(parse_state),
                Codepoint(_) => Ok((Parse(StringLiteral(StringLiteralState::default())), Noop)),
                Complete(_) => Ok((Ready, Noop)),
                End => return None,
            };

            if let Err((state, kind)) = result {
                self.scanner.next();
                return Some(Err(self.error(kind, state)));
            }

            if let Ok((next_state, action)) = result {
                if action != Noop {
                    self.scanner.next();
                }

                match action {
                    Append(c) => self.string_buffer.push(c),
                    Push(c) => self.stack.push(c),
                    Pop(c) => {
                        if !CharClass::is_balanced(self.stack.pop(), c) {
                            return Some(Err(self.error(ErrorKind::Unbalance, next_state)));
                        }
                    }
                    Skip | Noop => {}
                }

                self.state = next_state;
            }

            if let Codepoint(codepoint) = self.state {
                if let Some(c) = char::from_u32(codepoint) {
                    self.string_buffer.push(c);
                } else {
                    return Some(Err(self.error(ErrorKind::Codepoint, Codepoint(codepoint))));
                }
            }

            if let Complete(emitter) = self.state {
                return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{cursor::CursorBuilder, tokenization::num_lit_state::NumericLiteralStateBuilder};

    use super::*;

    fn identifier(s: &str) -> Result<Token, Error> {
        Ok(Token::Identifier(String::from(s)))
    }

    fn numeric_literal(s: &str) -> Result<Token, Error> {
        Ok(Token::NumericLiteral(String::from(s)))
    }

    fn string_literal(s: &str) -> Result<Token, Error> {
        Ok(Token::StringLiteral(String::from(s)))
    }

    fn symbol(s: &str) -> Result<Token, Error> {
        Ok(Token::Symbol(String::from(s)))
    }

    fn whitespace(s: &str) -> Result<Token, Error> {
        Ok(Token::Whitespace(String::from(s)))
    }

    fn tokenize(input: &str) -> Vec<Result<Token, Error>> {
        let tokenizer = Tokenizer::new(Scanner::new(input));
        tokenizer.collect()
    }

    #[test]
    fn empty() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(
            tokenize("\u{18}"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Ready,
                String::new(),
                CursorBuilder::new().col(1).curr('\u{18}').build(),
            ))]
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(
            tokenize("!@\u{2602}$"),
            vec![symbol("!"), symbol("@"), symbol("\u{2602}"), symbol("$"),]
        );
    }

    #[test]
    fn test_identifier() {
        assert_eq!(tokenize("_"), vec![identifier("_")]);
        assert_eq!(
            tokenize("a-あ"),
            vec![identifier("a"), symbol("-"), identifier("あ")]
        );
        assert_eq!(
            tokenize("アキラ　_f1o"),
            vec![identifier("アキラ"), whitespace("　"), identifier("_f1o"),]
        );
        assert_eq!(tokenize("mEme"), vec![identifier("mEme")]);

        assert_eq!(
            tokenize("m\"Eme\""),
            vec![identifier("m"), string_literal("Eme")]
        );

        assert_eq!(
            tokenize("a\u{18}"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(Identifier),
                String::from("a"),
                CursorBuilder::new().col(2).prev('a').curr('\u{18}').build(),
            ))]
        );
    }

    #[test]
    fn test_numeric_literal() {
        assert_eq!(tokenize("1"), vec![numeric_literal("1")]);
        assert_eq!(
            tokenize("12 "),
            vec![numeric_literal("12"), whitespace(" ")]
        );
        assert_eq!(
            tokenize("1 23 45.6 78e9 1.2E+3 4.5e-6"),
            vec![
                numeric_literal("1"),
                whitespace(" "),
                numeric_literal("23"),
                whitespace(" "),
                numeric_literal("45.6"),
                whitespace(" "),
                numeric_literal("78e9"),
                whitespace(" "),
                numeric_literal("1.2E+3"),
                whitespace(" "),
                numeric_literal("4.5e-6"),
            ]
        );

        assert_eq!(tokenize("1.2"), vec![numeric_literal("1.2")]);
        assert_eq!(tokenize("1e+2"), vec![numeric_literal("1e+2")]);
        assert_eq!(
            tokenize("1\u{18}2"),
            vec![
                Err(Error::new(
                    ErrorKind::Invalid,
                    Parse(NumericLiteral(NumericLiteralStateBuilder::new().build())),
                    String::from("1"),
                    CursorBuilder::new()
                        .col(2)
                        .prev('1')
                        .curr('\u{18}')
                        .next('2')
                        .build(),
                )),
                numeric_literal("2")
            ]
        );
        assert_eq!(
            tokenize("12\u{18}"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(NumericLiteralStateBuilder::new().build())),
                String::from("12"),
                CursorBuilder::new().col(3).prev('2').curr('\u{18}').build(),
            ))]
        );
        assert_eq!(
            tokenize("1."),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_dot().build()
                )),
                String::from("1."),
                CursorBuilder::new().col(2).prev('.').build(),
            ))]
        );
        assert_eq!(
            tokenize("1. "),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_dot().build()
                )),
                String::from("1."),
                CursorBuilder::new().col(3).prev('.').curr(' ').build(),
            ))]
        );
        assert_eq!(
            tokenize("1.\u{18}"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_dot().build()
                )),
                String::from("1."),
                CursorBuilder::new().col(3).prev('.').curr('\u{18}').build(),
            ))],
        );
        assert_eq!(
            tokenize("1.2."),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_dot().build()
                )),
                String::from("1.2"),
                CursorBuilder::new().col(4).prev('2').curr('.').build(),
            ))]
        );
        assert_eq!(
            tokenize("1e"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_exp().build()
                )),
                String::from("1e"),
                CursorBuilder::new().col(2).prev('e').build(),
            ))]
        );
        assert_eq!(
            tokenize("1E"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(
                    NumericLiteralStateBuilder::new().has_exp().build()
                )),
                String::from("1E"),
                CursorBuilder::new().col(2).prev('E').build(),
            ))]
        );
        assert_eq!(
            tokenize("1a"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(NumericLiteralStateBuilder::new().build())),
                String::from("1"),
                CursorBuilder::new().col(2).prev('1').curr('a').build(),
            ))]
        );
        assert_eq!(
            tokenize("11a"),
            vec![Err(Error::new(
                ErrorKind::Invalid,
                Parse(NumericLiteral(NumericLiteralStateBuilder::new().build())),
                String::from("11"),
                CursorBuilder::new().col(3).prev('1').curr('a').build(),
            ))]
        );
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(
            tokenize("\"hello world\""),
            vec![string_literal("hello world")]
        );

        assert_eq!(
            tokenize("\"hello \\\"\\n world\""),
            vec![string_literal("hello \"\n world")]
        );

        assert_eq!(
            tokenize("\"hello \\u{2602} world\""),
            vec![string_literal("hello ☂ world")]
        );
    }
}
