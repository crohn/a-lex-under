use crate::scanner::Scanner;
use crate::tokenization::{self, Action, CharClass, NumericLiteralState, ParseState, State, Token};
use std::iter::Iterator;
use std::mem;

use Action::*;
use ParseState::*;
use State::*;

const DOT: char = '.';
const HYPHEN: char = '-';
const LOWER_E: char = 'e';
const PLUS: char = '+';
const UNDERSCORE: char = '_';
const UPPER_E: char = 'E';

pub struct Tokenizer<'a> {
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn classify_char(c: Option<char>) -> CharClass {
        match c {
            Some(UNDERSCORE) => CharClass::SymbolIdentifier,
            Some(DOT) | Some(PLUS) | Some(HYPHEN) => CharClass::SymbolNumericLiteral,
            Some(c) if c.is_numeric() => CharClass::Numeric,
            Some(c) if c.is_alphabetic() => CharClass::Alphabetic,
            Some(c) if c.is_whitespace() => CharClass::Whitespace,
            Some(c) if !c.is_control() => CharClass::Symbol,
            Some(_) => CharClass::Invalid,
            None => CharClass::None,
        }
    }

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
    ) -> Result<(State, Action), tokenization::Error> {
        match parse_state {
            Identifier => self.transition_from_parse_identifier(),
            NumericLiteral(num_lit_state) => {
                self.transition_from_parse_numeric_literal(num_lit_state)
            }
            Whitespace => self.transition_from_parse_whitespace(),
            Symbol => Ok((Complete(Token::Symbol), EmitToken)),
        }
    }

    fn transition_from_parse_identifier(&mut self) -> Result<(State, Action), tokenization::Error> {
        match Self::classify_char(self.scanner.cursor().next()) {
            CharClass::Alphabetic | CharClass::Numeric | CharClass::SymbolIdentifier => {
                Ok((Parse(Identifier), Append))
            }
            CharClass::SymbolNumericLiteral
            | CharClass::Symbol
            | CharClass::Whitespace
            | CharClass::None => Ok((Complete(Token::Identifier), EmitToken)),
            CharClass::Invalid => Err(self.error(Parse(Identifier))),
        }
    }

    fn transition_from_parse_numeric_literal(
        &mut self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), tokenization::Error> {
        match (self.scanner.cursor().curr(), self.scanner.cursor().next()) {
            (_, Some(DOT)) => num_lit_state.apply_dot().map_or_else(
                |next| Err(self.error(Parse(NumericLiteral(next)))),
                |next| Ok((Parse(NumericLiteral(next)), Append)),
            ),
            (_, Some(UPPER_E) | Some(LOWER_E)) => num_lit_state.apply_exp().map_or_else(
                |next| Err(self.error(Parse(NumericLiteral(next)))),
                |next| Ok((Parse(NumericLiteral(next)), Append)),
            ),
            (Some(UPPER_E) | Some(LOWER_E), Some(PLUS) | Some(HYPHEN)) => {
                let next = mem::take(num_lit_state);
                Ok((Parse(NumericLiteral(next)), Append))
            }
            (curr, next) => match (Self::classify_char(curr), Self::classify_char(next)) {
                (CharClass::Numeric, CharClass::Whitespace | CharClass::None) => {
                    Ok((Complete(Token::NumericLiteral), EmitToken))
                }
                (_, CharClass::Numeric | CharClass::SymbolNumericLiteral) => {
                    let next = mem::take(num_lit_state);
                    Ok((Parse(NumericLiteral(next)), Append))
                }
                _ => Err(self.error(Parse(NumericLiteral(mem::take(num_lit_state))))),
            },
        }
    }

    fn transition_from_parse_whitespace(&self) -> Result<(State, Action), tokenization::Error> {
        match Self::classify_char(self.scanner.cursor().next()) {
            CharClass::Whitespace => Ok((Parse(Whitespace), Append)),
            _ => Ok((Complete(Token::Whitespace), EmitToken)),
        }
    }

    fn transition_from_ready(&mut self) -> Result<(State, Action), tokenization::Error> {
        match Self::classify_char(self.scanner.cursor().next()) {
            CharClass::Alphabetic | CharClass::SymbolIdentifier => Ok((Parse(Identifier), Append)),
            CharClass::Numeric => Ok((
                Parse(NumericLiteral(NumericLiteralState::default())),
                Append,
            )),
            CharClass::Symbol | CharClass::SymbolNumericLiteral => Ok((Parse(Symbol), Append)),
            CharClass::Whitespace => Ok((Parse(Whitespace), Append)),
            CharClass::None => Ok((End, Noop)),
            CharClass::Invalid => Err(self.error(Ready)),
        }
    }

    fn error(&mut self, state: State) -> tokenization::Error {
        self.scanner.next();
        tokenization::Error {
            buffer: mem::take(&mut self.string_buffer),
            cursor: self.scanner.cursor().clone(),
            state,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, tokenization::Error>;

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

                    if let Some(cursor) = self.scanner.next() {
                        if let Some(c) = cursor.curr() {
                            self.string_buffer.push(c);
                        }
                    }
                }
                Ok((next_state, EmitToken)) => {
                    // we can skip updating state here to save one cycle,
                    // because of mem::take at the beginning of the loop
                    if let Complete(emitter) = next_state {
                        return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
                    }
                }
                Ok((next_state, Noop)) => {
                    self.state = next_state;
                    continue;
                }
                Err(error) => {
                    // we can skip updating state here to save one cycle,
                    // because of mem::take at the beginning of the loop
                    return Some(Err(error));
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::cursor::CursorBuilder;

    use super::*;

    fn identifier(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Identifier(String::from(s)))
    }

    fn numeric_literal(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::NumericLiteral(String::from(s)))
    }

    fn symbol(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Symbol(String::from(s)))
    }

    fn whitespace(s: &str) -> Result<Token, tokenization::Error> {
        Ok(Token::Whitespace(String::from(s)))
    }

    fn tokenize(input: &str) -> Vec<Result<Token, tokenization::Error>> {
        let tokenizer = Tokenizer::new(Scanner::new(input));
        tokenizer.collect()
    }

    #[test]
    fn empty() {
        assert_eq!(tokenize(""), vec![]);
        assert_eq!(
            tokenize("\u{18}"),
            vec![Err(tokenization::Error {
                buffer: String::new(),
                cursor: CursorBuilder::new().col(1).curr('\u{18}').build(),
                state: Ready
            })]
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
            vec![Err(tokenization::Error {
                buffer: String::from("a"),
                cursor: CursorBuilder::new().col(2).prev('a').curr('\u{18}').build(),
                state: Parse(Identifier),
            })]
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
                Err(tokenization::Error {
                    buffer: String::from("1"),
                    cursor: CursorBuilder::new()
                        .col(2)
                        .prev('1')
                        .curr('\u{18}')
                        .next('2')
                        .build(),
                    state: Parse(NumericLiteral(NumericLiteralState {
                        has_dot: false,
                        has_exp: false
                    })),
                }),
                numeric_literal("2")
            ]
        );
        assert_eq!(
            tokenize("12\u{18}"),
            vec![Err(tokenization::Error {
                buffer: String::from("12"),
                cursor: CursorBuilder::new().col(3).prev('2').curr('\u{18}').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: false,
                    has_exp: false
                })),
            })]
        );
        assert_eq!(
            tokenize("1."),
            vec![Err(tokenization::Error {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(2).prev('.').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: true,
                    has_exp: false
                })),
            })]
        );
        assert_eq!(
            tokenize("1. "),
            vec![Err(tokenization::Error {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr(' ').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: true,
                    has_exp: false
                })),
            })]
        );
        assert_eq!(
            tokenize("1.\u{18}"),
            vec![Err(tokenization::Error {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr('\u{18}').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: true,
                    has_exp: false
                })),
            })],
        );
        assert_eq!(
            tokenize("1.2."),
            vec![Err(tokenization::Error {
                buffer: String::from("1.2"),
                cursor: CursorBuilder::new().col(4).prev('2').curr('.').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: true,
                    has_exp: false
                })),
            })]
        );
        assert_eq!(
            tokenize("1e"),
            vec![Err(tokenization::Error {
                buffer: String::from("1e"),
                cursor: CursorBuilder::new().col(2).prev('e').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: false,
                    has_exp: true
                })),
            })]
        );
        assert_eq!(
            tokenize("1E"),
            vec![Err(tokenization::Error {
                buffer: String::from("1E"),
                cursor: CursorBuilder::new().col(2).prev('E').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: false,
                    has_exp: true
                })),
            })]
        );
        assert_eq!(
            tokenize("1a"),
            vec![Err(tokenization::Error {
                buffer: String::from("1"),
                cursor: CursorBuilder::new().col(2).prev('1').curr('a').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: false,
                    has_exp: false
                })),
            })]
        );
        assert_eq!(
            tokenize("11a"),
            vec![Err(tokenization::Error {
                buffer: String::from("11"),
                cursor: CursorBuilder::new().col(3).prev('1').curr('a').build(),
                state: Parse(NumericLiteral(NumericLiteralState {
                    has_dot: false,
                    has_exp: false
                })),
            })]
        );
    }
}
