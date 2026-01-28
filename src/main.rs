use logos::Logos;

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

fn main() {
    let source = "(defun add (a b) (+ a b))";
    let mut lexer = Token::lexer(source);

    while let Some(token) = lexer.next() {
        match token {
            Ok(t) => println!("Token: {:?}", t),
            Err(_) => eprintln!("Unknown token at {:?}", lexer.span()),
        }
    }
}
