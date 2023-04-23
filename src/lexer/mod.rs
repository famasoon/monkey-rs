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
        return l;
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let ch = self.ch;
        self.read_char();
        match ch {
            Some('=') => {
                return match self.peak_char() {
                    Some('=') => {
                        self.read_char();
                        token::Token::Equal
                    }
                    Some(_) => token::Token::Assign,
                    None => token::Token::Assign,
                }
            }
            Some(';') => token::Token::Semicolon,
            Some('(') => token::Token::LParen,
            Some(')') => token::Token::RParen,
            Some(',') => token::Token::Comma,
            Some('+') => token::Token::Plus,
            Some('{') => token::Token::LBrace,
            Some('}') => token::Token::RBrace,
            Some('-') => token::Token::Minus,
            Some('!') => {
                return match self.peak_char() {
                    Some('=') => {
                        self.read_char();
                        token::Token::NotEqual
                    }
                    Some(_) => token::Token::Bang,
                    None => token::Token::Bang,
                };
            }
            Some('*') => token::Token::Asterisk,
            Some('/') => token::Token::Slash,
            Some('<') => token::Token::Lt,
            Some('>') => token::Token::Gt,
            None => token::Token::Eof,
            Some('"') => token::Token::String(self.read_string()),
            Some('[') => token::Token::LBracket,
            Some(']') => token::Token::RBracket,
            Some(':') => token::Token::Colon,
            Some(c) => {
                if Lexer::is_letter(c) {
                    self.read_identifier()
                } else if Lexer::is_digit(c) {
                    self.read_number()
                } else {
                    token::Token::Illigal
                }
            }
        }
    }

    fn read_identifier(&mut self) -> token::Token {
        let pos = self.position;
        loop {
            if let Some(s) = self.ch {
                if Lexer::is_letter(s) {
                    self.read_char();
                } else {
                    break;
                }
            }
        }

        let mut str1 = "".to_string();
        for (i, c) in self.input.chars().enumerate() {
            if i >= pos - 1 && i < self.position {
                str1.push(c);
            }
        }

        token::token_from_literal(str1)
    }

    fn read_number(&mut self) -> token::Token {
        let pos = self.position;
        loop {
            if let Some(s) = self.ch {
                if Lexer::is_digit(s) {
                    self.read_char();
                } else {
                    break;
                }
            }
        }

        let mut str1 = "".to_string();
        for (i, c) in self.input.chars().enumerate() {
            if i >= pos - 1 && i < self.position {
                str1.push(c);
            }
        }

        let num: i64 = str1.parse().unwrap();
        token::Token::Int(num)
    }

    fn read_string(&mut self) -> String {
        let mut str1 = "".to_string();

        if self.ch.unwrap() == '"' {
            self.read_char();
            return str1;
        }

        str1.push(self.ch.unwrap());
        loop {
            self.read_char();
            if self.ch.unwrap() == '"' || self.ch.unwrap() == '\0' {
                self.read_char();
                break;
            }
            str1.push(self.ch.unwrap());
        }

        str1
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.ch {
                if c.is_whitespace() {
                    self.read_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peak_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            return None;
        }
        self.input.chars().nth(self.read_position - 1)
    }

    fn is_letter(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_digit(c: char) -> bool {
        c.is_digit(10)
    }
}
