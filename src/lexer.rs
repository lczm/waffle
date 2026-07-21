use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r\n\f]+")]
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

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Result<Vec<Token>, ()> {
        Token::lexer(source).collect()
    }

    #[test]
    fn skips_each_supported_whitespace_character() {
        for space in [" ", "\t", "\r", "\n", "\x0c"] {
            // left{space}{right} -> left right
            let test_source = format!("left{space}right");

            assert_eq!(
                lex(&test_source),
                Ok(vec![
                    Token::Symbol("left".into()),
                    Token::Symbol("right".into()),
                ]),
                "failed to skip whitespace {space:?}",
            );
        }
    }

    #[test]
    fn skips_windows_crlf_line_endings() {
        let source = "(\r\nadd\r\n\t1\r\n2\r\n)\r\n";

        assert_eq!(
            lex(source),
            Ok(vec![
                Token::LParen,
                Token::Symbol("add".into()),
                Token::Integer(1),
                Token::Integer(2),
                Token::RParen,
            ])
        );
    }

    #[test]
    fn preserves_whitespace_inside_strings() {
        assert_eq!(
            lex("\"first line\r\nsecond line\""),
            Ok(vec![Token::String("first line\r\nsecond line".into())])
        );
    }
}
