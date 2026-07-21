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
