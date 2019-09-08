use crate::lex::tokens::{Token, TokenType};
use std::convert::TryFrom;
use std::fmt;
use std::fmt::{Binary, Display, Error, Formatter};

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Statement(Box<Expression>),
    Block(Vec<Expression>),
    Print(Box<Expression>),
    Literal(Literal),
    Unary {
        kind: UnaryOperation,
        expr: Box<Expression>,
    },
    Binary {
        kind: BinaryOperation,
        operands: (Box<Expression>, Box<Expression>),
    },
    ObjectNew(Box<Expression>, Vec<(FieldIdent, Expression)>),
    FieldAccess(Box<Expression>, String),
    FieldSet(Box<Expression>, String, Box<Expression>),
    Grouping(Box<Expression>),
    NonLocalAssign(String, Box<Expression>),
    Assign(String, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    LogicOr(Box<Expression>, Box<Expression>),
    LogicAnd(Box<Expression>, Box<Expression>),
    While(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Method(Box<Expression>, String, Vec<Expression>),
    Function(Vec<Param>, Option<Return>, Box<Expression>),
    Return(Option<Box<Expression>>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Expression::Variable(s) => write!(f, "VAR({})", s),
            Expression::Statement(expr) => write!(f, "{}; ", expr),
            Expression::Block(e) => {
                write!(f, "{{")?;
                for expr in e {
                    write!(f, "{}", expr)?;
                }
                write!(f, "}}")
            }
            Expression::Print(expr) => write!(f, "print {}", expr),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Unary { kind, expr } => write!(f, "{}{}", kind, expr),
            Expression::Binary { kind, operands } => {
                write!(f, "({} {} {})", kind, operands.0, operands.1)
            }
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::NonLocalAssign(name, expr) => write!(f, "nonlocal {}={}", name, expr),
            Expression::Assign(name, expr) => write!(f, "{}={}", name, expr),
            Expression::ObjectNew(parent, fields) => write!(f, "{} ~ {{{:?}}}", parent, fields),
            Expression::FieldAccess(obj, field) => write!(f, "{}.{}", obj, field),
            Expression::FieldSet(obj, field, rhs) => write!(f, "{}.{} = {}", obj, field, rhs),
            Expression::If(cond, yes, no) => {
                if no.is_some() {
                    write!(f, "if ({}) {} else {}", cond, yes, no.as_ref().unwrap())
                } else {
                    write!(f, "if ({}) {}", cond, yes)
                }
            }
            Expression::LogicOr(a, b) => write!(f, "{} || {}", a, b),
            Expression::LogicAnd(a, b) => write!(f, "{} && {}", a, b),
            Expression::While(cond, body) => write!(f, "while ({}) {}", cond, body),
            Expression::Call(callee, args) => {
                write!(f, "{}(", callee);
                for arg in args {
                    write!(f, "{},", arg);
                }
                write!(f, ")")
            },
            Expression::Method(callee, field, args) => {
                write!(f, "{}:{}", callee, field);
                for arg in args {
                    write!(f, "{},", arg);
                }
                write!(f, ")")
            },
            Expression::Function(params, ret, _) => {
                write!(f, "fn(");
                for param in params {
                    write!(f, "{},", param);
                }
                write!(f, ")");
                if let Some(r) = ret {
                    write!(f, " {}", r);
                }
                write!(f, " _")
            },
            Expression::Return(Some(expr)) => write!(f, "return {}", expr),
            Expression::Return(None) => write!(f, "return _"),
        }
    }
}

impl Expression {
    pub fn accept<T>(&self, visitor: impl ExpressionVisitor<Passthrough = T>, passthrough: T) {
        visitor.visit(self, passthrough);
    }
}

pub trait ExpressionVisitor {
    type Item;
    type Passthrough;
    fn visit(&self, expr: &Expression, passthrough: Self::Passthrough) -> Self::Item;
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    True,
    False,
    Object, // The mother-of-all-objects (literally)
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Object => write!(f, "Object"),
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum FieldIdent {
    Normal(String),
    Proto(String),
}

impl Display for FieldIdent {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            FieldIdent::Normal(s) => write!(f, "{}", s),
            FieldIdent::Proto(s) => write!(f, "proto {}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperation {
    Not,
    Minus,
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", match self {
            UnaryOperation::Not   => "!",
            UnaryOperation::Minus => "-"
        })
    }
}

impl TryFrom<&Token> for UnaryOperation {
    type Error = &'static str;
    fn try_from(value: &Token) -> Result<UnaryOperation, Self::Error> {
        let kind = &value.token_type;
        match kind {
            TokenType::Minus => Ok(UnaryOperation::Minus),
            TokenType::Bang => Ok(UnaryOperation::Not),
            _ => Err("The token passed didn't represent a unary operation"),
        }
    }
}

impl From<&str> for UnaryOperation {
    fn from(value: &str) -> UnaryOperation {
        match value {
            "-" => UnaryOperation::Minus,
            "!" => UnaryOperation::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
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
    Divide,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            BinaryOperation::Equals => write!(f, "=="),
            BinaryOperation::NotEquals => write!(f, "!="),
            BinaryOperation::LessThan => write!(f, "<"),
            BinaryOperation::LessOrEqual => write!(f, "<="),
            BinaryOperation::GreaterThan => write!(f, ">"),
            BinaryOperation::GreaterOrEqual => write!(f, ">="),
            BinaryOperation::Plus => write!(f, "+"),
            BinaryOperation::Minus => write!(f, "-"),
            BinaryOperation::Multiply => write!(f, "*"),
            BinaryOperation::Divide => write!(f, "/"),
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
            x => Err(format!(
                "The token passed didn't represent a binary operation: {:?}",
                x
            )),
        }
    }
}

impl From<&str> for BinaryOperation {
    fn from(value: &str) -> BinaryOperation {
        match value {
            "-" => BinaryOperation::Minus,
            "+" => BinaryOperation::Plus,
            "/" => BinaryOperation::Divide,
            "*" => BinaryOperation::Multiply,
            "!=" => BinaryOperation::NotEquals,
            "==" => BinaryOperation::Equals,
            ">" => BinaryOperation::GreaterThan,
            ">=" => BinaryOperation::GreaterOrEqual,
            "<" => BinaryOperation::LessThan,
            "<=" => BinaryOperation::LessOrEqual,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub ident: String,
    pub lifetime: Option<String>,
    pub access_type: AccessType,
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match &self.access_type {
            AccessType::Shared => {},
            x => {write!(f, "{} ", x)?},
        };
        write!(f, "{}", self.ident)?;
        if let Some(x) = &self.lifetime {
            write!(f, "'{}", x)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub lifetime: Option<String>,
    pub access_type: AccessType
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} ", self.access_type);
        write!(f, "return");
        if let Some(l) = &self.lifetime {
            write!(f, "'{}", l);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum AccessType {
    Move,
    Excl,
    Shared,
}

impl Display for AccessType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            AccessType::Move => write!(f, "move"),
            AccessType::Excl => write!(f, "excl"),
            _ => Ok(()),
        }
    }
}
