use logos::Logos;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

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

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

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

fn main() -> miette::Result<()> {
    let source = "(defun add (a b) (+ a; b))".to_string();
    let mut lexer = Token::lexer(&source);

    while let Some(token) = lexer.next() {
        match token {
            Ok(t) => println!("Token: {:?}", t),
            Err(_) => {
                let span = lexer.span();
                let err = LexError {
                    src: NamedSource::new("error", source.clone()),
                    bad_token_span: (span.start, span.end - span.start).into(),
                };

                return Err(err.into());
            }
        }
    }

    Ok(())
}
