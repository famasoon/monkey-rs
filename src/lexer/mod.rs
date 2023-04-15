use crate::token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }
    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let tok = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch.unwrap());
                    token::Token::new(token::EQ, literal)
                } else {
                    token::Token::new(token::ASSIGN, self.ch.unwrap().to_string())
                }
            }
            Some('+') => token::Token::new(token::PLUS, self.ch.unwrap().to_string()),
            Some('-') => token::Token::new(token::MINUS, self.ch.unwrap().to_string()),
            Some('!') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch.unwrap());
                    token::Token::new(token::NOT_EQ, literal)
                } else {
                    token::Token::new(token::BANG, self.ch.unwrap().to_string())
                }
            }
            Some('/') => token::Token::new(token::SLASH, self.ch.unwrap().to_string()),
            Some('*') => token::Token::new(token::ASTERISK, self.ch.unwrap().to_string()),
            Some('<') => token::Token::new(token::LT, self.ch.unwrap().to_string()),
            Some('>') => token::Token::new(token::GT, self.ch.unwrap().to_string()),
            Some(';') => token::Token::new(token::SEMICOLON, self.ch.unwrap().to_string()),
            Some(',') => token::Token::new(token::COMMA, self.ch.unwrap().to_string()),
            Some('(') => token::Token::new(token::LPAREN, self.ch.unwrap().to_string()),
            Some(')') => token::Token::new(token::RPAREN, self.ch.unwrap().to_string()),
            Some('{') => token::Token::new(token::LBRACE, self.ch.unwrap().to_string()),
            Some('}') => token::Token::new(token::RBRACE, self.ch.unwrap().to_string()),
            Some('[') => token::Token::new(token::LBRACKET, self.ch.unwrap().to_string()),
            Some(']') => token::Token::new(token::RBRACKET, self.ch.unwrap().to_string()),
            Some(':') => token::Token::new(token::COLLON, self.ch.unwrap().to_string()),
            Some('"') => token::Token::new(token::STRING, self.read_string()),
            Some(ch) => {
                if ch.is_alphabetic() || ch == '_' {
                    let literal = self.read_identifier();
                    let tok_type = token::lookup_ident(&literal);
                    return token::Token::new(tok_type, literal);
                } else if ch.is_digit(10) {
                    return token::Token::new(token::INT, self.read_number());
                } else {
                    token::Token::new(token::ILLEGAL, self.ch.unwrap().to_string())
                }
            }
        };
        self.read_char();
        tok
    }
    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_some() && (self.ch.unwrap().is_alphabetic() || self.ch.unwrap() == '_') {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position)
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_some() && self.ch.unwrap().is_whitespace() {
            self.read_char();
        }
    }
    fn read_number(&mut self) -> token::Token {
        let position = self.position;
        while self.ch.is_some() && self.ch.unwrap().is_digit(10) {
            self.read_char();
        }
        token::Token::INT()
    }
}