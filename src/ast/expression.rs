use std::fmt;
use std::fmt::{Display, Formatter, Error, Binary};
use std::convert::TryFrom;
use crate::lex::tokens::{Token, TokenType};

#[derive(Debug)]
pub enum Expression {
    Variable(String),
    Statement(Box<Expression>),
    Block(Vec<Expression>),
    Print(Box<Expression>),
    Literal(Literal),
    Unary{
        kind: UnaryOperation,
        expr: Box<Expression>
    },
    Binary{
        kind: BinaryOperation,
        operands: (Box<Expression>, Box<Expression>)
    },
    Grouping(Box<Expression>),
    NonLocalAssign(String, Box<Expression>),
    Assign(String, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    LogicOr(Box<Expression>, Box<Expression>),
    LogicAnd(Box<Expression>, Box<Expression>),
    While(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Function(Vec<Expression>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Expression::Variable(s) => write!(f, "{}", s),
            Expression::Statement(expr) => write!(f, "{}; ", expr),
            Expression::Block(e) => {
                write!(f, "{{")?;
                for expr in e {
                    write!(f, "{}", expr)?;
                }
                write!(f, "}}")
            },
            Expression::Print(expr) => write!(f, "print {}", expr),
            Expression::Literal(l) => {
                write!(f, "{}", l)
            },
            Expression::Unary{kind,expr} => write!(f, "{}{}", kind, expr),
            Expression::Binary{kind,operands} => write!(f, "({} {} {})", kind, operands.0, operands.1),
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::NonLocalAssign(name, expr) => write!(f, "nonlocal {}={}", name, expr),
            Expression::Assign(name, expr) => write!(f, "{}={}", name, expr),
            Expression::If(cond, yes, no) => {
                if no.is_some() {
                    write!(f, "if ({}) {} else {}", cond, yes, no.as_ref().unwrap())
                } else {
                    write!(f, "if ({}) {}", cond, yes)
                }
            },
            Expression::LogicOr(a, b) => write!(f, "{} || {}", a, b),
            Expression::LogicAnd(a, b) => write!(f, "{} && {}", a, b),
            Expression::While(cond, body) => write!(f, "while ({}) {}", cond, body),
            Expression::Call(callee, args) => {
                write!(f, "{}(", callee);
                for arg in args {
                    write!(f, "{}", arg);
                }
                write!(f, ")")
            },
            Expression:: Function(params, _) => {
                write!(f, "Fn(");
                for param in params {
                    write!(f, "{},", param);
                }
                write!(f, ")")
            }
        }
    }
}

impl Expression {
    pub fn accept<T>(&self, visitor: impl ExpressionVisitor<Passthrough=T>, passthrough: T) {
        visitor.visit(self, passthrough);
    }
}

pub trait ExpressionVisitor {
    type Item;
    type Passthrough;
    fn visit(&self, expr: &Expression, passthrough: Self::Passthrough) -> Self::Item;
}

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    True,
    False
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false")
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperation {
    Not,
    Minus
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl TryFrom<&Token> for UnaryOperation {
    type Error = &'static str;
    fn try_from(value: &Token) -> Result<UnaryOperation, Self::Error> {
        let kind = &value.token_type;
        match kind {
            TokenType::Minus => Ok(UnaryOperation::Minus),
            TokenType::Bang => Ok(UnaryOperation::Not),
            _ => Err("The token passed didn't represent a unary operation")
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperation {
    Equals,
    NotEquals,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Plus,
    Minus,
    Multiply,
    Divide
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            BinaryOperation::Equals => write!(f, "=="),
            BinaryOperation::NotEquals=> write!(f, "!="),
            BinaryOperation::LessThan=> write!(f, "<"),
            BinaryOperation::LessOrEqual=> write!(f, "<="),
            BinaryOperation::GreaterThan=> write!(f, ">"),
            BinaryOperation::GreaterOrEqual=> write!(f, ">="),
            BinaryOperation::Plus=> write!(f, "+"),
            BinaryOperation::Minus=> write!(f, "-"),
            BinaryOperation::Multiply=> write!(f, "*"),
            BinaryOperation::Divide=> write!(f, "/")
        }
    }
}

impl TryFrom<&Token> for BinaryOperation {
    type Error = String;
    fn try_from(value: &Token) -> Result<BinaryOperation, Self::Error> {
        let kind = &value.token_type;
        match kind {
            TokenType::Minus => Ok(BinaryOperation::Minus),
            TokenType::Plus => Ok(BinaryOperation::Plus),
            TokenType::Slash => Ok(BinaryOperation::Divide),
            TokenType::Star => Ok(BinaryOperation::Multiply),
            TokenType::BangEqual => Ok(BinaryOperation::NotEquals),
            TokenType::EqualEqual => Ok(BinaryOperation::Equals),
            TokenType::Greater => Ok(BinaryOperation::GreaterThan),
            TokenType::GreaterEqual => Ok(BinaryOperation::GreaterOrEqual),
            TokenType::Lesser => Ok(BinaryOperation::LessThan),
            TokenType::LesserEqual => Ok(BinaryOperation::LessOrEqual),
            x => Err(format!("The token passed didn't represent a binary operation: {:?}", x))
        }
    }
}
