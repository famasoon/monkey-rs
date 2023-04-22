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
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        "=" => Token::Assign,
        "+" => Token::Plus,
        "-" => Token::Minus,
        "!" => Token::Bang,
        "*" => Token::Asterisk,
        "/" => Token::Slash,
        "<" => Token::LT,
        ">" => Token::GT,
        "," => Token::Comma,
        ";" => Token::Semicolon,
        "(" => Token::LParen,
        ")" => Token::RParen,
        "{" => Token::LBrace,
        "}" => Token::RBrace,
        "==" => Token::Equal,
        "!=" => Token::NotEqual,
        "[" => Token::RBracket,
        "]" => Token::LBracket,
        ":" => Token::Colon,
        "macro" => Token::Macro,
        _ => Token::Ident(lieral),
    }

    pub fn string_from_token(token: Token) -> String {
        match token {
            Token::Let => String::from("let"),
            Token::Function => String::from("fn"),
            Token::Boolean(true) => String::from("true"),
            Token::Boolean(false) => String::from("false"),
            Token::Return => String::from("return"),
            Token::If => String::from("if"),
            Token::Else => String::from("else"),
            Token::Assign => String::from("="),
            Token::Plus => String::from("+"),
            Token::Minus => String::from("-"),
            Token::Bang => String::from("!"),
            Token::Asterisk => String::from("*"),
            Token::Slash => String::from("/"),
            Token::Lt => String::from("<"),
            Token::Gt => String::from(">"),
            Token::Comma => String::from(","),
            Token::Semicolon => String::from(";"),
            Token::LParen => String::from("("),
            Token::RParen => String::from(")"),
            Token::LBrace => String::from("{"),
            Token::RBrace => String::from("}"),
            Token::Equal => String::from("=="),
            Token::NotEqual => String::from("!="),
            Token::Int(d) => format!("{}", d),
            Token::String(s) => s,
            Token::LBacket => String::from("["),
            Token::RBacket => String::from("]"),
            Token::Colon => String::from(":"),
            Token::Macro => String::from("macro"),
            _ => String::from(""),
        }
    }
}