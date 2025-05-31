use crate::scanner::Scanner;
use crate::tokenization::char_class::{
    CharClass, DOT, DOUBLE_QUOTE, HYPHEN, LOWER_E, PLUS, UPPER_E,
};
use crate::tokenization::error::{Error, ErrorKind};
use crate::tokenization::num_lit_state::NumericLiteralState;
use crate::tokenization::str_lit_state::StringLiteralState;
use crate::tokenization::{Action, ParseState, State, Token};
use std::iter::Iterator;
use std::mem;

use Action::*;
use ParseState::*;
use State::*;

pub struct Tokenizer<'a> {
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            scanner,
            state: State::default(),
            string_buffer: String::new(),
        }
    }

    fn transition_from_parse(
        &mut self,
        parse_state: &mut ParseState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match parse_state {
            Identifier => self.transition_from_parse_identifier(),
            NumericLiteral(num_lit_state) => {
                self.transition_from_parse_numeric_literal(num_lit_state)
            }
            StringLiteral(str_lit_state) => {
                self.transition_from_parse_string_literal(str_lit_state)
            }
            Symbol => Ok((Complete(Token::Symbol), EmitToken)),
            Whitespace => self.transition_from_parse_whitespace(),
        }
    }

    fn transition_from_parse_identifier(&mut self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Alphabetic | CharClass::Numeric | CharClass::SymbolIdentifier => {
                Ok((Parse(Identifier), Append))
            }
            CharClass::SymbolNumericLiteral
            | CharClass::Symbol
            | CharClass::Whitespace
            | CharClass::None => Ok((Complete(Token::Identifier), EmitToken)),
            CharClass::Invalid | CharClass::SymbolStringLiteral => {
                Err((Parse(Identifier), ErrorKind::Invalid))
            }
        }
    }

    fn transition_from_parse_numeric_literal(
        &mut self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match (self.scanner.cursor().curr(), self.scanner.cursor().next()) {
            (_, Some(DOT)) => num_lit_state.apply_dot().map_or_else(
                |next| Err((Parse(NumericLiteral(next)), ErrorKind::Invalid)),
                |next| Ok((Parse(NumericLiteral(next)), Append)),
            ),
            (_, Some(UPPER_E) | Some(LOWER_E)) => num_lit_state.apply_exp().map_or_else(
                |next| Err((Parse(NumericLiteral(next)), ErrorKind::Invalid)),
                |next| Ok((Parse(NumericLiteral(next)), Append)),
            ),
            (Some(UPPER_E) | Some(LOWER_E), Some(PLUS) | Some(HYPHEN)) => {
                let next = mem::take(num_lit_state);
                Ok((Parse(NumericLiteral(next)), Append))
            }
            (curr, next) => match (CharClass::classify(curr), CharClass::classify(next)) {
                (CharClass::Numeric, CharClass::Whitespace | CharClass::None) => {
                    Ok((Complete(Token::NumericLiteral), EmitToken))
                }
                (_, CharClass::Numeric | CharClass::SymbolNumericLiteral) => {
                    let next = mem::take(num_lit_state);
                    Ok((Parse(NumericLiteral(next)), Append))
                }
                _ => Err((
                    Parse(NumericLiteral(mem::take(num_lit_state))),
                    ErrorKind::Invalid,
                )),
            },
        }
    }

    fn transition_from_parse_string_literal(
        &self,
        str_lit_state: &mut StringLiteralState,
    ) -> Result<(State, Action), (State, ErrorKind)> {
        match self.scanner.cursor().next() {
            Some(DOUBLE_QUOTE) => Ok((Complete(Token::StringLiteral), Pop)),
            c => match CharClass::classify(c) {
                CharClass::Invalid => Err((
                    Parse(StringLiteral(mem::take(str_lit_state))),
                    ErrorKind::Invalid,
                )),
                _ => Ok((Parse(StringLiteral(mem::take(str_lit_state))), Append)),
            },
        }
    }

    fn transition_from_parse_whitespace(&self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Whitespace => Ok((Parse(Whitespace), Append)),
            _ => Ok((Complete(Token::Whitespace), EmitToken)),
        }
    }

    fn transition_from_ready(&mut self) -> Result<(State, Action), (State, ErrorKind)> {
        match CharClass::classify(self.scanner.cursor().next()) {
            CharClass::Alphabetic | CharClass::SymbolIdentifier => Ok((Parse(Identifier), Append)),
            CharClass::Numeric => Ok((
                Parse(NumericLiteral(NumericLiteralState::default())),
                Append,
            )),
            CharClass::SymbolStringLiteral => {
                Ok((Parse(StringLiteral(StringLiteralState::default())), Push))
            }
            CharClass::Symbol | CharClass::SymbolNumericLiteral => Ok((Parse(Symbol), Append)),
            CharClass::Whitespace => Ok((Parse(Whitespace), Append)),
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
                Complete(_) => Ok((Ready, Noop)),
                End => return None,
            };

            match result {
                Ok((next_state, Append)) => {
                    self.state = next_state;
                    self.scanner
                        .next()
                        .and_then(|cursor| cursor.curr())
                        .map(|c| self.string_buffer.push(c));
                }
                Ok((next_state, EmitToken)) => {
                    if let Complete(emitter) = next_state {
                        return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
                    }
                }
                Ok((next_state, Noop)) => {
                    self.state = next_state;
                }
                Ok((next_state, Pop)) => {
                    self.scanner.next();
                    if let Complete(emitter) = next_state {
                        return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
                    }
                }
                Ok((next_state, Push)) => {
                    self.scanner.next();
                    self.state = next_state;
                }
                Err((state, kind)) => {
                    self.scanner.next();

                    return Some(Err(Error::new(
                        kind,
                        state,
                        mem::take(&mut self.string_buffer),
                        self.scanner.cursor().clone(),
                    )));
                }
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
    }
}
