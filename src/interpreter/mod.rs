use crate::ast::expression::{
    BinaryOperation, Expression, ExpressionVisitor, Literal, UnaryOperation,
};
use crate::errors::{Error, ErrorBuilder, ErrorType};
use crate::External;
use core::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Binary, Debug, Display, Formatter};
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
//use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Callable(Rc<Callable>),
}

trait Callable: Debug {
    fn arity(&self) -> usize;
    fn call<'a, 'pt>(
        &self,
        interpreter: &'a Interpreter<'a>,
        out: &'a mut External<'pt>,
        args: Vec<Value>,
    ) -> Result<Value, Error>;
}

struct NativeFn {
    arity: usize,
    body: fn(Vec<Value>, interpreter: &Interpreter, out: &mut External) -> Result<Value, Error>,
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "NativeFunction")
    }
}

impl Callable for NativeFn {
    fn arity(&self) -> usize {
        return self.arity;
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        out: &mut External,
        args: Vec<Value>,
    ) -> Result<Value, Error> {
        (self.body)(args, interpreter, out)
    }
}

#[derive(Debug)]
struct RuntimeFn {
    params: Vec<Expression>,
    body: Box<Expression>,
}

impl Callable for RuntimeFn {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call(
        &self,
        interpreter: &Interpreter,
        out: &mut External,
        args: Vec<Value>,
    ) -> Result<Value, Error> {
        interpreter.environment.borrow_mut().wrap();
        for (param, arg) in self.params.iter().zip(args) {
            // this should always be true
            if let Expression::Variable(s) = param {
                interpreter.environment.borrow_mut().scopes[0].insert(s.to_string(), arg);
            }
        }
        let result = interpreter.visit(&self.body, out);
        interpreter.environment.borrow_mut().unwrap();
        return result;
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Value::Integer(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Callable { .. } => write!(f, "FunctionObject"),
        }
    }
}

pub struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Environment {
    pub fn new() -> Environment {
        let mut globals = HashMap::new();
        let clock = Value::Callable(Rc::new(NativeFn {
            arity: 0,
            body: |_, _, ext| {
                let time = (ext.clock)();
                Ok(Value::Float(time))
            },
        }));
        let print = Value::Callable(Rc::new(NativeFn {
            arity: 1,
            body: |mut args, _, out| {
                write!(out.output, "{}\n", args[0]);
                Ok(args.remove(0))
            },
        }));
        globals.insert("clock".to_string(), clock);
        globals.insert("puts".to_string(), print);
        Environment {
            scopes: vec![globals],
        }
    }

    pub fn wrap(&mut self) {
        self.scopes.insert(0, HashMap::new());
    }

    pub fn unwrap(&mut self) {
        self.scopes.remove(0);
    }
}

pub struct Interpreter<'a> {
    pub err_build: &'a ErrorBuilder,
    pub environment: RefCell<Environment>,
}

//
//impl<'a, 's> ExpressionVisitor<'s> for Interpreter<'a> {
//    type Item = Result<Value, Error>;
//    type Passthrough = &'s mut External<'s>;
//    fn visit(&'s self, expr: &Expression, passthrough: Self::Passthrough) -> Self::Item {
//        match expr {
//            Expression::Statement(x) => self.statement_expression(x, passthrough),
//            Expression::Block(exprs) => self.block_expression(exprs, passthrough),
//            Expression::Print(x) => self.print_expression(x, passthrough),
//            Expression::Literal(x) => Ok(self.literal_value(x)),
//            Expression::Unary { kind, expr } => self.unary_value(kind, expr, passthrough),
//            Expression::Binary { kind, operands } => self.binary_value(kind, operands, passthrough),
//            Expression::Grouping(e) => self.visit(e, passthrough),
//            Expression::Variable(s) => self.get_var(s),
//            Expression::Assign(s, expr) => {
//                let val = self.visit(expr, passthrough)?;
//                self.set_var(s, val)
//            }
//            Expression::NonLocalAssign(s, expr) => {
//                let val = self.visit(expr, passthrough)?;
//                self.set_nonlocal(s, val)
//            }
//            Expression::If(cond, yes, no) => self.if_cond(cond, yes, no, passthrough),
//            Expression::LogicOr(a, b) => self.logic_or(a, b, passthrough),
//            Expression::LogicAnd(a, b) => self.logic_and(a, b, passthrough),
//            Expression::While(cond, body) => self.while_loop(cond, body, passthrough),
//            Expression::Call(callee, args) => self.call(callee, args, passthrough),
//            Expression::Function(params, body) => self.function(params, body, passthrough),
//        }
//    }
//}

impl<'a> Interpreter<'a> {
    fn visit<'s, 'x>(&'s self, expr: &Expression, passthrough: &'s mut External<'x>) -> Result<Value, Error> {
        match expr {
            Expression::Statement(x) => self.statement_expression(x, passthrough),
            Expression::Block(exprs) => self.block_expression(exprs, passthrough),
            Expression::Print(x) => self.print_expression(x, passthrough),
            Expression::Literal(x) => Ok(self.literal_value(x)),
            Expression::Unary { kind, expr } => self.unary_value(kind, expr, passthrough),
            Expression::Binary { kind, operands } => self.binary_value(kind, operands, passthrough),
            Expression::Grouping(e) => self.visit(e, passthrough),
            Expression::Variable(s) => self.get_var(s),
            Expression::Assign(s, expr) => {
                let val = self.visit(expr, passthrough)?;
                self.set_var(s, val)
            }
            Expression::NonLocalAssign(s, expr) => {
                let val = self.visit(expr, passthrough)?;
                self.set_nonlocal(s, val)
            }
            Expression::If(cond, yes, no) => self.if_cond(cond, yes, no, passthrough),
            Expression::LogicOr(a, b) => self.logic_or(a, b, passthrough),
            Expression::LogicAnd(a, b) => self.logic_and(a, b, passthrough),
            Expression::While(cond, body) => self.while_loop(cond, body, passthrough),
            Expression::Call(callee, args) => self.call(callee, args, passthrough),
            Expression::Function(params, body) => self.function(params, body, passthrough),
        }
    }


    pub fn new(err_build: &'a ErrorBuilder) -> Interpreter {
        Interpreter {
            err_build,
            environment: RefCell::new(Environment::new()),
        }
    }

    pub fn interpret<'x, 'pt>(
        &'x self,
        exprs: Vec<Expression>,
        out: &'x mut External<'pt>,
    ) -> Option<Error> {
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

    fn statement_expression<'s, 'x>(
        &'s self,
        expr: &Box<Expression>,
        out: &'s mut External<'x>,
    ) -> Result<Value, Error> {
        self.visit(expr, out)
    }

    fn block_expression<'pt, 'x>(
        &'pt self,
        exprs: &Vec<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        self.environment.borrow_mut().wrap();
        let mut last = Value::String("None type from block expression".to_string());
        for expr in exprs {
            last = self.visit(expr, out)?
        }
        self.environment.borrow_mut().unwrap();
        return Ok(last);
    }

    fn print_expression<'pt, 'x>(
        &'pt self,
        expr: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let v = self.visit(expr, out)?;
        writeln!(out.output, "{}", v);
        Ok(v)
    }

    fn unary_value<'pt, 'x>(
        &'pt self,
        kind: &UnaryOperation,
        expr: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let v = self.visit(expr, out)?;
        match kind {
            UnaryOperation::Not => match v {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                v => Err(self
                    .err_build
                    .create(0, 0, ErrorType::InterpretBooleanNotWrongType)),
            },
            UnaryOperation::Minus => match v {
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                v => Err(self.err_build.create(0, 0, ErrorType::InterpretUnaryMinus)),
            },
        }
    }

    fn binary_value<'pt, 'x>(
        &'pt self,
        kind: &BinaryOperation,
        operands: &(Box<Expression>, Box<Expression>),
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
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
        }
        .map_err(|e| self.err_build.create(0, 0, e))
    }

    fn get_var(&self, s: &str) -> Result<Value, Error> {
        let scopes = &self.environment.borrow().scopes;
        for env in scopes {
            if env.contains_key(s) {
                return Ok(env.get(s).unwrap().clone());
            }
        }
        Err(self.err_build.create(0, 0, ErrorType::NonExistantVariable))
    }

    fn set_nonlocal(&self, s: &str, v: Value) -> Result<Value, Error> {
        let scopes = &mut self.environment.borrow_mut().scopes;
        for env in scopes {
            if env.contains_key(s) {
                env.insert(s.to_string(), v);
                return Ok(Value::String(
                    "THIS IS A NONE VALUE FROM SETTING NON LOCAL".to_string(),
                ));
            }
        }
        Err(self.err_build.create(0, 0, ErrorType::NonExistantVariable))
    }

    fn set_var(&self, s: &str, v: Value) -> Result<Value, Error> {
        self.environment.borrow_mut().scopes[0].insert(s.to_string(), v);
        Ok(Value::String(
            "THIS IS A NONE VALUE FROM SETTING VARIABLE".to_string(),
        ))
    }

    fn if_cond<'pt, 'x>(
        &'pt self,
        cond: &Box<Expression>,
        yes: &Box<Expression>,
        no: &Option<Box<Expression>>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let cond_eval = self.visit(cond, out)?;
        if let Value::Bool(true) = cond_eval {
            self.visit(yes, out)
        } else {
            no.as_ref().map_or(
                Ok(Value::String(
                    "PLACE HOLDER NONE VALUE FROM IF ELSE BRANCH".to_string(),
                )),
                |v| self.visit(&v, out),
            )
        }
    }

    fn while_loop<'pt, 'x>(
        &'pt self,
        cond: &Box<Expression>,
        body: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let mut result = Value::String("NONE VALUE FROM WHILE LOOP".to_string());
        loop {
            let eval = self.visit(cond, out)?;
            if let Value::Bool(true) = eval {
                result = self.visit(body, out)?;
            } else {
                return Ok(result);
            }
        }
    }

    fn call<'pt, 'x>(
        &'pt self,
        callee: &Box<Expression>,
        args: &Vec<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let callable = self.visit(callee, out)?;
        let mut arguments_result = vec![];
        for arg in args {
            arguments_result.push(self.visit(arg, out)?);
        }
        let arguments_result = Ok(arguments_result);

        //        let arguments_result: Result<Vec<Value>, Error> = args.iter().map(|arg|self.visit(arg, out)).collect();
        match arguments_result {
            Ok(args) => {
                if let Value::Callable(call) = callable {
                    if call.arity() == args.len() {
                        call.call(self, out, args)
                    } else {
                        Err(self.err_build.create(0, 0, ErrorType::WrongNumberArgs))
                    }
                } else {
                    Err(self.err_build.create(0, 0, ErrorType::CallNonFunction))
                }
            }
            Err(e) => Err(e),
        }
    }

    fn function<'pt, 'x>(
        &self,
        params: &Vec<Expression>,
        body: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        Ok(Value::Callable(Rc::new(RuntimeFn {
            params: params.to_vec(),
            body: body.clone(),
        })))
    }

    fn logic_or<'pt, 'x>(
        &'pt self,
        a: &Box<Expression>,
        b: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let a_res = self.visit(a, out);
        if let Ok(Value::Bool(true)) = a_res {
            a_res
        } else {
            self.visit(b, out)
        }
    }

    fn logic_and<'pt, 'x>(
        &'pt self,
        a: &Box<Expression>,
        b: &Box<Expression>,
        out: &'pt mut External<'x>,
    ) -> Result<Value, Error> {
        let a_res = self.visit(a, out);
        if let Ok(Value::Bool(true)) = a_res {
            self.visit(b, out)
        } else {
            Ok(Value::Bool(false))
        }
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
                _ => Err(ErrorType::AddIncompatible),
            },
            Value::Float(f) => match rhs {
                Value::Integer(i) => Ok(Value::Float(f + i as f64)),
                Value::Float(f2) => Ok(Value::Float(f + f2)),
                Value::String(s) => Ok(Value::String(f.to_string() + &s)),
                Value::Bool(_) => Err(ErrorType::AddBool),
                _ => Err(ErrorType::AddIncompatible),
            },
            Value::String(s) => match rhs {
                Value::Integer(i) => Ok(Value::String(s + &i.to_string())),
                Value::Float(f) => Ok(Value::String(s + &f.to_string())),
                Value::String(s2) => Ok(Value::String(s + &s2)),
                Value::Bool(b) => Ok(Value::String(s + &b.to_string())),
                _ => Err(ErrorType::AddIncompatible),
            },
            Value::Bool(b) => match rhs {
                Value::String(s) => Ok(Value::String(b.to_string() + &s)),
                _ => Err(ErrorType::AddBool),
            },
            _ => Err(ErrorType::AddIncompatible),
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
