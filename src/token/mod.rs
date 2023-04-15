#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Token {
    Let,
    Ident(String),
    Equal,
    Int(i64),
    Assign,
    Plus,
    Minus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
}

pub fn token_from_literal(lieral: String) -> Token {
    match lieral.as_ref() {
        "let" => Token::Let,
        "fn" => Token::Function,
        "=" => Token::Assign,
        "==" => Token::Equal,
        "+" => Token::Plus,
        "(" => Token::LParen,
        ")" => Token::RParen,
        "{" => Token::LBrace,
        "}" => Token::RBrace,
        "-" => Token::Minus,
        "," => Token::Comma,
        ";" => Token::Semicolon,
        _ => Token::Ident(lieral),
    }
}