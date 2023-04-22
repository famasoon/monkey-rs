#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Token {
    Illigal,
    Eof,
    Ident(String),
    Int(i64),
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Equal,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    If,
    Else,
    Boolean(bool),
    Return,
    NotEqual,
    String(String),
    LBacket,
    RBacket,
    Colon,
    Macro,
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
        "!" => Token::Bang,
        "*" => Token::Asterisk,
        "/" => Token::Slash,
        "<" => Token::Lt,
        ">" => Token::Gt,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(lieral),
    }
}