use crate::token;
use std::fmt;
pub mod modify;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Literal {
    Int(i64),
    Boolean(bool),
    String(String),
    Uint,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Literal::Int(i) => format!("{}", i),
            Literal::Boolean(b) => format!("{}", b),
            Literal::String(s) => s.clone(),
            Literal::Uint => String::from("()"),
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct HashItem {
    pub value: Expression,
    pub key: Expression,
}

impl fmt::Display for HashItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = format!("{}: {}", self.key, self.value);
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Literal(Literal),
    Block(Vec<Box<Statement>>),
    Prefix(token::Token, Box<Expression>),
    Infix(token::Token, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Function(Vec<Box<Expression>>, Box<Expression>),
    Call(Box<Expression>, Vec<Box<Expression>>),
    Ident(String),
    Array(Vec<Box<Expression>>),
    Index(Box<Expression>, Box<Expression>),
    Hashmap(Vec<HashItem>),
    Macro(Vec<Box<Expression>>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Expression::Literal(l) => format!("{}", l),
            Expression::Block(b) => {
                let mut string = String::new();
                for s in b.iter() {
                    string.push_str(&format!("{}", &s));
                }
                string
            }
            Expression::Prefix(t, e) => format!("({}{})", token::string_from_token(t.clone()), e),
            Expression::Infix(t, e1, e2) => {
                format!("({} {} {})", token::string_from_token(t.clone()), e1, e2)
            }
            Expression::If(e, s1, s2) => {
                let mut string = format!("if {} {{{}}}", e, s1);
                let string2 = match s2 {
                    Some(s) => format!(" else {{{}}}", s),
                    None => String::from(""),
                };
                format!("{}{}", string, string2)
            }
            Expression::Function(p, b) => {
                let mut string = String::from("fn(");
                for p in p {
                    string.push_str(&format!("{}, ", p));
                }
                string.push_str(&format!(") {}", b));
                string
            }
            Expression::Call(f, p) => {
                let mut string: Vec<String> = Vec::new();
                for p in p.iter() {
                    string.push(format!("{}, ", p));
                }
                format!("{}({})", f, string.join(", "))
            }
            Expression::Ident(i) => format!("{}", i),
            Expression::Array(a) => {
                let mut string: Vec<String> = Vec::new();
                for e in a {
                    string.push(format!("{}, ", e));
                }
                format!("[{}]", string.join(", "))
            }
            Expression::Index(e1, e2) => format!("({}[{}])", e1, e2),
            Expression::Hashmap(h) => {
                let mut string: Vec<String> = Vec::new();
                for h in h.iter() {
                    string.push(format!("{}", h));
                }
                format!("{{{}}}", string.join(","))
            }
            Expression::Macro(p, b) => {
                let mut strings: Vec<String> = Vec::new();
                for p in p.iter() {
                    strings.push(format!("{}, ", p));
                }
                format!("macro({}) {}", strings.join(", "), b)
            }
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    Let(Expression, Expression),
    Return(Expression),
    ExpStatement(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Statement::Let(e1, e2) => format!("let {} = {};", e1, e2),
            Statement::Return(e1) => format!("return {};", e1),
            Statement::ExpStatement(e1) => format!("{}", e1),
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        for s in self.statements.iter() {
            string.push_str(&format!("{}", s));
        }
        write!(f, "{}", string)
    }
}
