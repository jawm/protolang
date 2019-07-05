use crate::ast::expression::{ExpressionVisitor, Expression, Literal, UnaryOperation, BinaryOperation};
use crate::errors::{Error, ErrorBuilder, ErrorType};
use std::fmt::{Binary, Formatter, Display};
use std::ops::{Add, Sub, Mul, Div};
use std::cmp::Ordering;
use std::cell::RefCell;
use core::borrow::Borrow;
//use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),

}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Value::Integer(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Bool(x) => write!(f, "{}", x),
        }
    }
}

pub struct Interpreter<'a> {
    pub err_build: &'a ErrorBuilder,
    pub environment: RefCell<std::collections::HashMap<String, Value>>,
}

impl<'a> ExpressionVisitor for Interpreter<'a> {
    type Item = Result<Value, Error>;
    type Passthrough = &'a mut std::io::Write;
    fn visit(&self, expr: &Expression, passthrough: Self::Passthrough) -> Self::Item {
        match expr {
            Expression::Statement(x) => self.statement_expression(x, passthrough),
            Expression::Block(exprs) => self.block_expression(exprs, passthrough),
            Expression::Print(x) => self.print_expression(x, passthrough),
            Expression::Literal(x) => Ok(self.literal_value(x)),
            Expression::Unary {kind, expr} => self.unary_value(kind, expr, passthrough),
            Expression::Binary {kind, operands} => self.binary_value(kind, operands, passthrough),
            Expression::Grouping(e) => self.visit(e, passthrough),
            Expression::Variable(s) => self.get_var(s),
            Expression::Assign(s, expr) => {
                let val = self.visit(expr, passthrough)?;
                self.set_var(s, val)
            },
        }
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(err_build: &'a ErrorBuilder) -> Interpreter {
        Interpreter {
            err_build,
            environment: RefCell::new(std::collections::HashMap::new())
        }
    }

    pub fn interpret(&self, exprs: Vec<Expression>, out: &mut std::io::Write) -> Option<Error> {
        for expr in exprs {
            if let Err(e) = self.visit(&expr, out) {
                return Some(e);
            }
        }
        None
    }

    fn literal_value(&self, x: &Literal) -> Value {
        match x {
            Literal::Integer(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::String(s) => Value::String(s.to_string()),
            Literal::True => Value::Bool(true),
            Literal::False => Value::Bool(false),
        }
    }

    fn statement_expression(&self, expr: &Box<Expression>, out: &mut std::io::Write) -> Result<Value, Error> {
        self.visit(expr, out)
    }

    fn block_expression(&self, exprs: &Vec<Expression>, out: &mut std::io::Write) -> Result<Value, Error> {
        let mut last = Value::String("None type from block expression".to_string());
        for expr in exprs {
            last = self.visit(expr, out)?
        }
        return Ok(last)
    }

    fn print_expression(&self, expr: &Box<Expression>, out: &mut std::io::Write) -> Result<Value, Error> {
        let v = self.visit(expr, out)?;
        writeln!(out, "{}", v);
        Ok(v)
    }

    fn unary_value(&self, kind: &UnaryOperation, expr: &Box<Expression>, out: &mut std::io::Write) -> Result<Value, Error> {
        let v = self.visit(expr, out)?;
        match kind {
            UnaryOperation::Not => match v {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                v => Err(self.err_build.create(0, 0, ErrorType::InterpretBooleanNotWrongType))
            },
            UnaryOperation::Minus => match v {
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                v => Err(self.err_build.create(0, 0, ErrorType::InterpretUnaryMinus)),
            }
        }
    }

    fn binary_value(&self, kind: &BinaryOperation, operands: &(Box<Expression>, Box<Expression>), out: &mut std::io::Write) -> Result<Value, Error> {
        let left = self.visit(&operands.0, out)?;
        let right = self.visit(&operands.1, out)?;
        match kind {
            BinaryOperation::Plus => left + right,
            BinaryOperation::Equals => Ok(Value::Bool(left == right)),
            BinaryOperation::NotEquals => Ok(Value::Bool(left != right)),
            BinaryOperation::LessThan => Ok(Value::Bool(left < right)),
            BinaryOperation::LessOrEqual => Ok(Value::Bool(left <= right)),
            BinaryOperation::GreaterThan => Ok(Value::Bool(left > right)),
            BinaryOperation::GreaterOrEqual => Ok(Value::Bool(left >= right)),
            BinaryOperation::Minus => left - right,
            BinaryOperation::Multiply => left * right,
            BinaryOperation::Divide => left / right,
        }.map_err(|e|self.err_build.create(0, 0, e))
    }

    fn get_var(&self, s: &str) -> Result<Value, Error> {
        self.environment.borrow().get(s).map(Clone::clone).ok_or(self.err_build.create(0, 0, ErrorType::NonExistantVariable))
    }

    fn set_var(&self, s: &str, v: Value) -> Result<Value, Error> {
        self.environment.borrow_mut().insert(s.to_string(), v);
        Ok(Value::String("THIS IS A NONE VALUE FROM SETTING VARIABLE".to_string()))
    }
}

impl Add<Value> for Value {
    type Output = Result<Value, ErrorType>;
    fn add(self, rhs: Value) -> Self::Output {
        match self {
            Value::Integer(i) => match rhs {
                Value::Integer(i2) => Ok(Value::Integer(i + i2)),
                Value::Float(f) => Ok(Value::Float(i as f64 + f)),
                Value::String(s) => Ok(Value::String(i.to_string() + &s)),
                Value::Bool(_) => Err(ErrorType::AddBool),
            }
            Value::Float(f) => match rhs {
                Value::Integer(i) => Ok(Value::Float(f + i as f64)),
                Value::Float(f2) => Ok(Value::Float(f + f2)),
                Value::String(s) => Ok(Value::String(f.to_string() + &s)),
                Value::Bool(_) => Err(ErrorType::AddBool),
            }
            Value::String(s) => match rhs {
                Value::Integer(i) => Ok(Value::String(s + &i.to_string())),
                Value::Float(f) => Ok(Value::String(s + &f.to_string())),
                Value::String(s2) => Ok(Value::String(s + &s2)),
                Value::Bool(b) => Ok(Value::String(s + &b.to_string())),
            }
            Value::Bool(b) => match rhs {
                Value::String(s) => Ok(Value::String(b.to_string() + &s)),
                _ => Err(ErrorType::AddBool),
            }
        }
    }
}

impl Sub<Value> for Value {
    type Output = Result<Value, ErrorType>;
    fn sub(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l - r as f64)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
            _ => Err(ErrorType::SubtractWrongTypes),
        }
    }
}

impl PartialEq<Value> for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Integer(i), Value::Integer(i2)) => i == i2,
            (Value::Integer(f), Value::Integer(f2)) => f == f2,
            (Value::String(s), Value::String(s2)) => s == s2,
            (Value::Bool(b), Value::Bool(b2)) => b == b2,
            _ => false,
        }
    }
}

impl PartialOrd<Value> for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => l.partial_cmp(r),
            (Value::Integer(l), Value::Float(r)) => (*l as f64).partial_cmp(r),
            (Value::Float(l), Value::Integer(r)) => l.partial_cmp(&(*r as f64)),
            (Value::Float(l), Value::Float(r)) => l.partial_cmp(r),
            (Value::String(l), Value::String(r)) => l.partial_cmp(r),
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Mul<Value> for Value {
    type Output = Result<Value, ErrorType>;
    fn mul(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l * r as f64)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
            (Value::Integer(l), Value::String(r)) => Ok(Value::String(r.repeat(l as usize))),
            (Value::String(l), Value::Integer(r)) => Ok(Value::String(l.repeat(r as usize))),
            _ => Err(ErrorType::MultiplyWrongTypes),
        }
    }
}

impl Div<Value> for Value {
    type Output = Result<Value, ErrorType>;

    fn div(self, rhs: Value) -> Self::Output {
        match (self, rhs) {
            (Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l / r)),
            (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(l as f64 / r)),
            (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l / r as f64)),
            (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l / r)),
            _ => Err(ErrorType::DivideWrongTypes),
        }
    }
}
