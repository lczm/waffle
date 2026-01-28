use std::iter::Peekable;

use logos::{Logos, SpannedIter};
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
pub enum AppError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(#[from] LexError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Parser(#[from] ParseError),
}

#[derive(Error, Debug, Diagnostic)]
#[error("Invalid token found!")]
#[diagnostic(
    code(waffle::lex_error),
    help("Check if this is a character that you intended to type or if it is supported")
)]
pub struct LexError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("This character is not recognized")]
    pub bad_token_span: SourceSpan,
}

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected token")]
    #[diagnostic(code(waffle::unexpected_token))]
    UnexpectedToken {
        #[source_code]
        src: NamedSource<String>,

        #[label("Expected {expected}")]
        span: SourceSpan,
        expected: String,
    },

    #[error("Unexpected end of file")]
    #[diagnostic(code(waffle::unexpected_token))]
    UnexpectedEOF {
        #[source_code]
        src: NamedSource<String>,

        #[label("Unexpected end of file")]
        span: SourceSpan,
    },
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    // maybe for the future, keep them out for now for simplicity
    // #[token("[")]
    // LBracket,
    // #[token("]")]
    // RBracket,
    // #[token("{")]
    // LBrace,
    // #[token("}")]
    // RBrace,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // give higher priority to integers than symbols
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok(), priority=3)]
    Integer(i64),

    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r"[a-zA-Z!$%&*+-./:<=>?@^_~][a-zA-Z0-9!$%&*+-./:<=>?@^_~]*", |lex| lex.slice().to_string())]
    Symbol(String),

    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),
}

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
struct Parser<'a> {
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

    fn make_error(&self, span: SourceSpan, expected: &str) -> ParseError {
        ParseError::UnexpectedToken {
            src: NamedSource::new("input", self.src.to_string()),
            span,
            expected: expected.to_string(),
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
                Err(self
                    .make_error(
                        span.into(),
                        "Unexpected closing brace ')' without the opening brace",
                    )
                    .into())
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

fn main() -> miette::Result<()> {
    let source = "(defun add (a b) (+ a b))".to_string();
    let invalid_source = "(defun add (a b) (+ a; b))".to_string();

    let mut parser = Parser::new(&source);
    while parser.iter.peek().is_some() {
        match parser.parse_expr() {
            Ok(expr) => println!("Parsed: {:#?}", expr),
            // first error instant break
            // TODO: maybe in the future can do some sort of recovery
            Err(e) => {
                println!("{:?}", miette::Report::new(e));
                break;
            }
        }
    }

    Ok(())
}
