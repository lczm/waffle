use logos::{Logos, SpannedIter};
use miette::{NamedSource, SourceSpan};
use std::iter::Peekable;

use crate::{
    errors::{AppError, LexError, ParseError},
    lexer::Token,
};

#[derive(Debug)]
pub enum Expr {
    Symbol(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    List(Vec<Expr>),
}

// 'a, data lives in source elsewhere, not within parser
pub(crate) struct Parser<'a> {
    // SpannedIter gives Token together with line and column height (Span)
    // and Peekable lets you look ahead at the next token without consuming
    // we can get this from the lexer
    iter: Peekable<SpannedIter<'a, Token>>,
    // reference to the source
    src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            // create the lexer within the parser itself, so it does 2 in 1
            iter: Token::lexer(source).spanned().peekable(),
            src: source,
        }
    }

    pub fn has_more(&mut self) -> bool {
        self.iter.peek().is_some()
    }

    fn make_error(&self, span: SourceSpan, expected: &str) -> ParseError {
        ParseError::UnexpectedToken {
            src: NamedSource::new("input", self.src.to_string()),
            span,
            expected: expected.to_string(),
        }
    }

    fn make_unexpected_closing_paren_error(&self, span: SourceSpan) -> ParseError {
        ParseError::UnexpectedClosingParen {
            src: NamedSource::new("input", self.src.to_string()),
            span,
        }
    }

    fn make_eof_error(&self) -> ParseError {
        let len = self.src.len();
        ParseError::UnexpectedEOF {
            src: NamedSource::new("input", self.src.to_string()),
            span: (len.saturating_sub(1)..len).into(),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, AppError> {
        // get next token
        let (token_res, _) = match self.iter.peek() {
            Some(pair) => pair,
            None => return Err(self.make_eof_error().into()),
        };

        // check if lexer is ok
        if token_res.is_err() {
            let (_, span) = self.iter.next().unwrap();
            return Err(LexError {
                src: NamedSource::new("input", self.src.to_string()),
                bad_token_span: span.into(),
            }
            .into());
        }

        let token = token_res.clone().unwrap();
        match token {
            Token::LParen => self.parse_list(),
            Token::Integer(i) => {
                self.iter.next();
                Ok(Expr::Integer(i))
            }
            Token::Float(i) => {
                self.iter.next();
                Ok(Expr::Float(i))
            }
            Token::String(i) => {
                self.iter.next();
                Ok(Expr::String(i))
            }
            Token::Symbol(i) => {
                self.iter.next();
                Ok(Expr::Symbol(i))
            }
            Token::True => {
                self.iter.next();
                Ok(Expr::Boolean(true))
            }
            Token::False => {
                self.iter.next();
                Ok(Expr::Boolean(false))
            }

            Token::RParen => {
                let (_, span) = self.iter.next().unwrap();
                Err(self.make_unexpected_closing_paren_error(span.into()).into())
            }
        }
    }

    fn parse_list(&mut self) -> Result<Expr, AppError> {
        // got to parse_list, so the first token is the opening token, we can just move past that
        self.iter.next();

        let mut items = Vec::new();

        // loop until we see a ) or a EOF and break
        loop {
            match self.iter.peek() {
                None => {
                    return Err(self.make_eof_error().into());
                }
                Some((Ok(t), _)) if t == &Token::RParen => {
                    // found the end, move past it and end the loop
                    self.iter.next();
                    break;
                }
                _ => {
                    items.push(self.parse_expr()?);
                }
            }
        }

        Ok(Expr::List(items))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn error_unmatched_closing_parenthesis() {
        let mut parser = Parser::new(")");

        assert!(matches!(
            parser.parse_expr(),
            Err(AppError::Parser(ParseError::UnexpectedClosingParen { .. }))
        ));
    }
}
