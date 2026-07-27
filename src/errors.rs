use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::bytecode::Value;

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

#[derive(Error, Debug, Diagnostic, PartialEq)]
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

#[derive(Error, Debug, Diagnostic, PartialEq)]
pub enum CompileError {
    #[error("List is empty")]
    EmptyList,

    #[error("Unknown symbol")]
    UnknownSymbol,

    #[error("Operator {operator} expected {expected_count} arguments, but received {parsed_count}")]
    #[diagnostic(
        code(waffle::incorrect_argument_count),
        help("Provide exactly {expected_count} arguments to `{operator}`")
    )]
    IncorrectArgumentCount {
        operator: String,
        expected_count: usize,
        parsed_count: usize,
    },

    #[error("The first expression in a function call must be a symbol")]
    InvalidCallTarget,

    #[error("`define` expected a symbol as its name, but received {found}")]
    InvalidDefinitionName { found: String },
}

#[derive(Error, Debug, Diagnostic, PartialEq)]
pub enum RuntimeError {
    #[error("Indexing into a chunk constants with an invalid index")]
    InvalidConstantIndex,

    #[error("Attempted to pop off the VM stack when stack is empty")]
    StackPopEmpty,

    #[error(
        "operator {operator} requires two integers or two floats, but received {received_type_lhs} {operator} {received_type_rhs}"
    )]
    TypeError {
        operator: String,
        received_type_lhs: String,
        received_type_rhs: String,
    },

    #[error("Attempted to divide {numerator} by zero")]
    DivideByZero { numerator: Value },

    #[error("Integer overflow while evaluating {lhs} {operator} {rhs}")]
    IntegerOverflow {
        operator: String,
        lhs: i64,
        rhs: i64,
    },
}
