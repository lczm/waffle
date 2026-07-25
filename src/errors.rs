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

    #[error(transparent)]
    #[diagnostic(transparent)]
    Compiler(#[from] CompileError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    VM(#[from] RuntimeError),
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

    #[error("Unexpected closing parenthesis")]
    #[diagnostic(
        code(waffle::unexpected_closing_parenthesis),
        help("Remove this ')' or add a matching '(' before it")
    )]
    UnexpectedClosingParen {
        #[source_code]
        src: NamedSource<String>,

        #[label("This ')' has no matching '('")]
        span: SourceSpan,
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

#[derive(Error, Debug, Diagnostic)]
pub enum CompileError {
    #[error("List is empty")]
    EmptyList,

    #[error("Unknown symbol")]
    UnknownSymbol,

    #[error("operator {operator} expected {expected_count} arguments, but received {parsed_count}")]
    #[diagnostic(
        code(waffle::incorrect_argument_count),
        help("Provide exactly {expected_count} arguments to `{operator}`")
    )]
    IncorrectArgumentCount {
        operator: String,
        expected_count: usize,
        parsed_count: usize,
    },
}

#[derive(Error, Debug, Diagnostic)]
pub enum RuntimeError {
    #[error("Indexing into a chunk constants with an invalid index")]
    InvalidConstantIndex,

    #[error("Attempted to pop off the VM stack when stack is empty")]
    StackPopEmpty,
}
