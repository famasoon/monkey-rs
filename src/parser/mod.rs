use crate::ast;
use crate::lexer;
use crate::token;

use ast::*;

#[derive(PartialEq, PartialOrd)]
enum Priority {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(l: lexer::Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: token::Token::Illigal,
            peek_token: token::Token::Illigal,
            errors: Vec::new(),
        };
        p.next_token();
        p.next_token();

        return p;
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut p = Program::new();

        while !self.cur_token_is(token::Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                p.statements.push(Box::new(s));
            }
            self.next_token();
        }
        Some(p)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            token::Token::Let => {
                let ps = self.parse_let_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
            token::Token::Return => {
                let ps = self.parse_return_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
            _ => {
                let ps = self.parse_expression_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(token::Token::Ident(String::new())) {
            return None;
        }

        let ident = match self.cur_token.clone() {
            token::Token::Ident(s) => Expression::Ident(s.clone()),
            _ => Expression::Ident(String::from("")),
        };

        if !self.expect_peek(token::Token::Assign) {
            return None;
        }

        self.next_token();

        let exp = match self.parse_expression(Priority::Lowest) {
            Some(s) => s,
            None => Expression::Literal(Literal::Uint),
        };

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(ident, exp))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let value = self.parse_expression(Priority::Lowest).unwrap();

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let exp = match self.parse_expression(Priority::Lowest) {
            Some(s) => s,
            None => return None,
        };

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::ExpStatement(exp))
    }

    fn parse_expression(&mut self, p: Priority) -> Option<Expression> {
        let mut leftexp = match self.prefix_parse() {
            Some(s) => s,
            None => return None,
        };

        while !self.peek_token_is(token::Token::Semicolon) && p < self.peek_priority() {
            self.next_token();
            leftexp = self.parse_infix_expression(leftexp.clone());
        }
        Some(leftexp)
    }

    fn prefix_parse(&mut self) -> Option<Expression> {
        match &self.cur_token {
            token::Token::Ident(d) => Some(Expression::Ident(d.clone())),
            token::Token::Int(d) => Some(Expression::Literal(Literal::Int(*d))),
            token::Token::String(s) => Some(Expression::Literal(Literal::String(s.clone()))),
            token::Token::Bang => self.prefix_parse_ops(),
            token::Token::Minus => self.prefix_parse_ops(),
            token::Token::Boolean(d) => Some(Expression::Literal(Literal::Boolean(d.clone()))),
            token::Token::LParen => {
                self.next_token();
                let exp = self.parse_expression(Priority::Lowest);
                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }
                exp
            }
            token::Token::If => {
                let _ = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                self.next_token();
                let condition = self.parse_expression(Priority::Lowest).unwrap();

                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                let consequence = self.parse_block_statement().unwrap();
                let mut alternative: Option<Box<Expression>> = None;
                if self.peek_token_is(token::Token::Else) {
                    self.next_token();

                    if !self.expect_peek(token::Token::LBrace) {
                        return None;
                    }

                    alternative = match self.parse_block_statement() {
                        Some(s) => Some(Box::new(s)),
                        None => None,
                    }
                }

                Some(Expression::If(
                    Box::new(condition),
                    Box::new(consequence),
                    alternative,
                ))
            }
            token::Token::Function => {
                let _ = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                let parameters = self.parse_function_parameters();

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                let body = self.parse_block_statement().unwrap();

                Some(Expression::Function(parameters, Box::new(body)))
            }
            token::Token::LBracket => Some(Expression::Array(
                self.parse_expression_arguments(token::Token::RBracket),
            )),
            token::Token::LBrace => self.parse_hash(),
            token::Token::Macro => {
                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                let parameters = self.parse_function_parameters();

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                let body = self.parse_block_statement().unwrap();
                Some(Expression::Macro(parameters, Box::new(body)))
            }
            _ => None,
        }
    }

    fn prefix_parse_ops(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        self.next_token();
        let exp = self.parse_expression(Priority::Prefix).unwrap();
        Some(Expression::Prefix(token.clone(), Box::new(exp)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        match self.cur_token {
            token::Token::LParen => {
                let function = Box::new(left);
                let _ = self.cur_token.clone();
                let arguments = self.parse_expression_arguments(token::Token::RParen);
                Expression::Call(function, arguments)
            }
            token::Token::LBracket => {
                self.next_token();
                let exp = self.parse_expression(Priority::Lowest).unwrap();

                if !self.expect_peek(token::Token::RBracket) {
                    return Expression::Index(
                        Box::new(left),
                        Box::new(Expression::Literal(Literal::Int(0))),
                    );
                }
                Expression::Index(Box::new(left), Box::new(exp))
            }
            _ => {
                let _ = self.cur_token.clone();
                let operator = self.cur_token.clone();

                let priority = self.cur_priority();
                self.next_token();
                let right = self.parse_expression(priority).unwrap();

                Expression::Infix(operator.clone(), Box::new(left), Box::new(right))
            }
        }
    }

    fn parse_block_statement(&mut self) -> Option<Expression> {
        let mut statements: Vec<Box<Statement>> = Vec::new();

        self.next_token();

        while !self.cur_token_is(token::Token::RBrace) && !self.cur_token_is(token::Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                statements.push(Box::new(s.clone()));
            }
            self.next_token();
        }

        Some(Expression::Block(statements))
    }

    fn parse_function_parameters(&mut self) -> Vec<Box<Expression>> {
        let mut ret: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(token::Token::RParen) {
            self.next_token();
            return ret;
        }
        self.next_token();

        let ident_name = match self.cur_token.clone() {
            token::Token::Ident(s) => s.clone(),
            _ => String::from(""),
        };
        let ident = Expression::Ident(ident_name);

        ret.push(Box::new(ident));

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();

            let ident_name = match self.cur_token.clone() {
                token::Token::Ident(s) => s.clone(),
                _ => String::from(""),
            };
            let ident = Expression::Ident(ident_name);
            ret.push(Box::new(ident));
        }

        if self.expect_peek(token::Token::RParen) {
            return ret;
        }

        ret
    }

    fn parse_expression_arguments(&mut self, end_token: token::Token) -> Vec<Box<Expression>> {
        let mut args: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(end_token.clone()) {
            self.next_token();
            return args;
        }

        self.next_token();

        match self.parse_expression(Priority::Lowest) {
            Some(s) => args.push(Box::new(s.clone())),
            None => {}
        };

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_expression(Priority::Lowest) {
                Some(s) => args.push(Box::new(s.clone())),
                None => {}
            };
        }

        if !self.expect_peek(end_token) {
            return args;
        }

        args
    }

    fn parse_hash(&mut self) -> Option<Expression> {
        let mut args: Vec<HashItem> = Vec::new();
        while !self.peek_token_is(token::Token::RBrace) {
            self.next_token();
            let key = match self.parse_expression(Priority::Lowest) {
                Some(s) => s,
                None => Expression::Literal(Literal::Uint),
            };

            if !self.expect_peek(token::Token::Colon) {
                return None;
            }

            self.next_token();

            let value = match self.parse_expression(Priority::Lowest) {
                Some(s) => s,
                None => Expression::Literal(Literal::Uint),
            };

            if !self.expect_peek(token::Token::Comma) && !self.peek_token_is(token::Token::RBrace) {
                return None;
            };

            if let Expression::Literal(Literal::Uint) = key {
                continue;
            };

            if let Expression::Literal(Literal::Uint) = value {
                continue;
            };

            let hashitem = HashItem { key, value };
            args.push(hashitem);
        }
        if !self.expect_peek(token::Token::RBrace) {
            return None;
        }
        Some(Expression::Hashmap(args))
    }

    fn peek_priority(&self) -> Priority {
        self.get_priority(&self.peek_token)
    }

    fn cur_priority(&self) -> Priority {
        self.get_priority(&self.cur_token)
    }

    fn get_priority(&self, tok: &token::Token) -> Priority {
        match *tok {
            token::Token::Equal => Priority::Equals,
            token::Token::NotEqual => Priority::Equals,
            token::Token::Lt => Priority::LessGreater,
            token::Token::Gt => Priority::LessGreater,
            token::Token::Plus => Priority::Sum,
            token::Token::Minus => Priority::Sum,
            token::Token::Slash => Priority::Product,
            token::Token::Asterisk => Priority::Product,
            token::Token::LParen => Priority::Call,
            token::Token::LBracket => Priority::Index,
            _ => Priority::Lowest,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn cur_token_is(&self, token: token::Token) -> bool {
        match token {
            token::Token::Ident(_) => match self.cur_token {
                token::Token::Ident(_) => true,
                _ => false,
            },
            _ => self.cur_token == token,
        }
    }

    fn expect_peek(&mut self, t: token::Token) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t.clone());
            false
        }
    }

    fn peek_token_is(&self, token: token::Token) -> bool {
        match token {
            token::Token::Ident(_) => match self.peek_token {
                token::Token::Ident(_) => true,
                _ => false,
            },
            _ => self.peek_token == token,
        }
    }

    fn peek_error(&mut self, t: token::Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token
        ));
    }
}
