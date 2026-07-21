use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub(crate) enum Token {
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
