use crate::scanner::Scanner;
use crate::tokenization;
use std::iter::Iterator;
use std::mem;

use Action::*;
use State::*;

const DOT: char = '.';
const HYPHEN: char = '-';
const LOWER_E: char = 'e';
const PLUS: char = '+';
const UNDERSCORE: char = '_';
const UPPER_E: char = 'E';

#[derive(Debug)]
enum Action {
    Append,
    EmitToken,
    Noop,
}

#[derive(Debug)]
enum State {
    Ready,
    Parse(ParseState),
    Complete(fn(String) -> Token),
    End,
}

impl Default for State {
    fn default() -> State {
        State::Ready
    }
}

#[derive(Debug)]
enum ParseState {
    Identifier,
    NumericLiteral(NumericLiteralState),
    Symbol,
    Whitespace,
}

#[derive(Debug)]
struct NumericLiteralState {
    has_dot: bool,
    has_exp: bool,
}

impl Default for NumericLiteralState {
    fn default() -> NumericLiteralState {
        NumericLiteralState {
            has_dot: false,
            has_exp: false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    NumericLiteral(String),
    Symbol(String),
    Whitespace(String),
}

pub struct Tokenizer<'a> {
    scanner: Scanner<'a>,
    state: State,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn is_char_delimiter(c: char) -> bool {
        c.is_whitespace()
    }

    pub fn is_char_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphanumeric()
    }

    pub fn is_char_start_identifier(c: char) -> bool {
        c == UNDERSCORE || c.is_alphabetic()
    }

    pub fn is_char_numeric_literal(c: char) -> bool {
        c == DOT || c == PLUS || c == HYPHEN || c == UPPER_E || c == LOWER_E || c.is_numeric()
    }

    pub fn is_char_start_numeric_literal(c: char) -> bool {
        c.is_numeric()
    }

    pub fn is_char_symbol(c: char) -> bool {
        !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
    }

    pub fn new(scanner: Scanner<'a>) -> Tokenizer<'a> {
        Tokenizer {
            scanner,
            state: State::default(),
            string_buffer: String::new(),
        }
    }

    fn emit_numeric_literal_or_error(
        &self,
        num_lit_state: &NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        if num_lit_state.has_dot || num_lit_state.has_exp {
            if let Some(curr) = self.scanner.cursor().curr {
                if curr.is_numeric() {
                    Ok((Complete(Token::NumericLiteral), EmitToken))
                } else {
                    Err(())
                }
            } else {
                Err(())
            }
        } else {
            Ok((Complete(Token::NumericLiteral), EmitToken))
        }
    }

    fn transition_from_parse(&self, parse_state: &mut ParseState) -> Result<(State, Action), ()> {
        match parse_state {
            ParseState::Identifier => self.transition_from_parse_identifier(),
            ParseState::NumericLiteral(num_lit_state) => {
                self.transition_from_parse_numeric_literal(num_lit_state)
            }
            ParseState::Whitespace => self.transition_from_parse_whitespace(),
            ParseState::Symbol => Ok((Complete(Token::Symbol), EmitToken)),
        }
    }

    fn transition_from_parse_identifier(&self) -> Result<(State, Action), ()> {
        let Some(c) = self.scanner.cursor().next else {
            return Ok((Complete(Token::Identifier), EmitToken));
        };

        if Self::is_char_identifier(c) {
            Ok((Parse(ParseState::Identifier), Append))
        } else if Self::is_char_delimiter(c) {
            Ok((Complete(Token::Identifier), EmitToken))
        } else if Self::is_char_symbol(c) {
            Ok((Complete(Token::Identifier), EmitToken))
        } else {
            Err(())
        }
    }

    fn transition_from_parse_numeric_literal(
        &self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        match self.scanner.cursor().next {
            Some(DOT) => self.transition_from_parse_numeric_literal_dot(num_lit_state),
            Some(UPPER_E) | Some(LOWER_E) => {
                self.transition_from_parse_numeric_literal_exp(num_lit_state)
            }
            Some(PLUS) | Some(HYPHEN) => {
                self.transition_from_parse_numeric_literal_sign(num_lit_state)
            }

            Some(c) => self.transition_from_parse_numeric_literal_some(c, num_lit_state),
            None => self.emit_numeric_literal_or_error(num_lit_state),
        }
    }

    fn transition_from_parse_numeric_literal_dot(
        &self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        if num_lit_state.has_dot || num_lit_state.has_exp {
            Err(())
        } else {
            let mut next = mem::take(num_lit_state);
            next.has_dot = true;
            Ok((Parse(ParseState::NumericLiteral(next)), Append))
        }
    }

    fn transition_from_parse_numeric_literal_exp(
        &self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        if num_lit_state.has_exp {
            Err(())
        } else {
            let mut next = mem::take(num_lit_state);
            next.has_exp = true;
            Ok((Parse(ParseState::NumericLiteral(next)), Append))
        }
    }

    fn transition_from_parse_numeric_literal_sign(
        &self,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        if num_lit_state.has_exp
            && matches!(self.scanner.cursor().curr, Some(UPPER_E) | Some(LOWER_E))
        {
            let next = mem::take(num_lit_state);
            Ok((Parse(ParseState::NumericLiteral(next)), Append))
        } else {
            Err(())
        }
    }

    fn transition_from_parse_numeric_literal_some(
        &self,
        c: char,
        num_lit_state: &mut NumericLiteralState,
    ) -> Result<(State, Action), ()> {
        if Self::is_char_numeric_literal(c) {
            let next = mem::take(num_lit_state);
            Ok((Parse(ParseState::NumericLiteral(next)), Append))
        } else if Self::is_char_delimiter(c) {
            self.emit_numeric_literal_or_error(num_lit_state)
        } else {
            Err(())
        }
    }

    fn transition_from_parse_whitespace(&self) -> Result<(State, Action), ()> {
        let Some(c) = self.scanner.cursor().next else {
            return Ok((Complete(Token::Whitespace), EmitToken));
        };

        if Self::is_char_delimiter(c) {
            Ok((Parse(ParseState::Whitespace), Append))
        } else {
            Ok((Complete(Token::Whitespace), EmitToken))
        }
    }

    fn transition_from_ready(&self) -> Result<(State, Action), ()> {
        let Some(c) = self.scanner.cursor().next else {
            return Ok((End, Noop));
        };

        if Self::is_char_start_identifier(c) {
            Ok((Parse(ParseState::Identifier), Append))
        } else if Self::is_char_start_numeric_literal(c) {
            Ok((
                Parse(ParseState::NumericLiteral(NumericLiteralState::default())),
                Append,
            ))
        } else if Self::is_char_symbol(c) {
            Ok((Parse(ParseState::Symbol), Append))
        } else if Self::is_char_delimiter(c) {
            Ok((Parse(ParseState::Whitespace), Append))
        } else {
            Err(())
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

            if let Ok((next_state, next_action)) = result {
                self.state = next_state;

                match next_action {
                    Append => {
                        if let Some(cursor) = self.scanner.next() {
                            if let Some(c) = cursor.curr {
                                self.string_buffer.push(c);
                            }
                        }
                    }
                    EmitToken => {
                        // do not emit on empty buffer
                        if let Complete(emitter) = self.state {
                            return Some(Ok(emitter(mem::take(&mut self.string_buffer))));
                        }
                    }
                    Noop => continue,
                }
            } else {
                self.state = Ready;
                self.scanner.next();

                let buffer = mem::take(&mut self.string_buffer);
                let error = Some(Err(tokenization::Error::InvalidCharacter {
                    buffer,
                    cursor: self.scanner.cursor().clone(),
                }));
                return error;
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
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::new(),
                cursor: CursorBuilder::new().col(1).curr('\u{18}').build(),
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
        assert_eq!(
            tokenize("a\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("a"),
                cursor: CursorBuilder::new().col(2).prev('a').curr('\u{18}').build()
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
                Err(tokenization::Error::InvalidCharacter {
                    buffer: String::from("1"),
                    cursor: CursorBuilder::new()
                        .col(2)
                        .prev('1')
                        .curr('\u{18}')
                        .next('2')
                        .build()
                }),
                numeric_literal("2")
            ]
        );
        assert_eq!(
            tokenize("12\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("12"),
                cursor: CursorBuilder::new().col(3).prev('2').curr('\u{18}').build()
            })]
        );
        assert_eq!(
            tokenize("1."),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(2).prev('.').build()
            })]
        );
        assert_eq!(
            tokenize("1. "),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr(' ').build()
            })]
        );
        assert_eq!(
            tokenize("1.\u{18}"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1."),
                cursor: CursorBuilder::new().col(3).prev('.').curr('\u{18}').build()
            })],
        );
        assert_eq!(
            tokenize("1.2."),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1.2"),
                cursor: CursorBuilder::new().col(4).prev('2').curr('.').build()
            })]
        );
        assert_eq!(
            tokenize("1e"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1e"),
                cursor: CursorBuilder::new().col(2).prev('e').build()
            })]
        );
        assert_eq!(
            tokenize("1E"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1E"),
                cursor: CursorBuilder::new().col(2).prev('E').build()
            })]
        );
        assert_eq!(
            tokenize("1a"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("1"),
                cursor: CursorBuilder::new().col(2).prev('1').curr('a').build()
            })]
        );
        assert_eq!(
            tokenize("11a"),
            vec![Err(tokenization::Error::InvalidCharacter {
                buffer: String::from("11"),
                cursor: CursorBuilder::new().col(3).prev('1').curr('a').build()
            })]
        );
    }
}
