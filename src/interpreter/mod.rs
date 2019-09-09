use crate::ast::expression::{
    BinaryOperation, Expression, ExpressionVisitor, Literal, Param, UnaryOperation, FieldIdent
};
use crate::errors::{Error, ErrorBuilder, ErrorType};
use crate::External;
use core::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Binary, Debug, Display, Formatter};
use std::ops::{Add, Div, Mul, Sub, Try};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug)]
pub enum ExprResult<'a> {
    Value(ValRef<'a>),
    ControlFlow(ControlFlowConstruct<'a>),
    Err(Error),
}

pub enum ExprError<'a> {
    ControlFlow(ControlFlowConstruct<'a>),
    Err(Error)
}

#[derive(Debug)]
pub enum ControlFlowConstruct<'a> {
    Return(Option<ValRef<'a>>)
}

#[derive(Debug)]
pub enum ValRef<'a> {
    Val(Value<'a>),
    Ref(Ref<'a, ValRef<'a>>)
}

impl<'a> Try for ExprResult<'a> {
    type Ok = ValRef<'a>;
    type Error = ExprError<'a>;

    fn into_result(self) -> Result<Self::Ok, Self::Error> {
        match self {
            ExprResult::Value(v) => Ok(v),
            ExprResult::ControlFlow(c) => Err(ExprError::ControlFlow(c)),
            ExprResult::Err(e) => Err(ExprError::Err(e))
        }
    }

    fn from_error(v: Self::Error) -> Self {
        match v {
            ExprError::ControlFlow(c) => ExprResult::ControlFlow(c),
            ExprError::Err(e) => ExprResult::Err(e)
        }
    }

    fn from_ok(v: Self::Ok) -> Self {
        ExprResult::Value(v)
    }
}

#[derive(Debug)]
pub enum Value<'a> {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Callable(Rc<Callable<'a>>),
    Object(HashMap<FieldIdent, RefCell<ValRef<'a>>>)
}

impl<'a> ValRef<'a> {
    fn field_access(&'a self, field: &str) -> Option<Ref<'a, ValRef>> {
        match self {
            ValRef::Val(x) => x.field_access(field),
            ValRef::Ref(x) => x.field_access(field),
        }
    }
    fn field_set<'b>(&'b mut self, field: &str, value: ValRef<'a>) {
        match self {
            ValRef::Val(x) => x.field_set(field, value),
            ValRef::Ref(x) => x.field_set(field, value)
        }
    }
    fn proto_fields(&'a self) -> HashMap<FieldIdent, Ref<ValRef>> {
        match self {
            ValRef::Val(x) => x.proto_fields(),
            ValRef::Ref(x) => x.proto_fields(),
        }
    }
    fn drill(&'a self) -> &'a Value<'a> {
        match self {
            ValRef::Val(x) => x,
            ValRef::Ref(x) => x.drill()
        }
    }
}

impl Display for ValRef<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            ValRef::Val(x) => write!(f, "{}", x),
            ValRef::Ref(x) => write!(f, "{}", x),
        }
    }
}

impl<'b> Value<'b> {
    fn field_access<'a>(&'a self, field: &str) -> Option<Ref<'a, ValRef<'b>>> {
        match self {
            Value::Object(x) => {
                x.get(&FieldIdent::Normal(field.to_string()))
                    .map(|rc|rc.borrow())
            },
            _ => None
        }
    }

    fn field_set<'a>(&'a mut self, field: &str, value: ValRef<'b>) {
        match self {
            Value::Object(x) => {
                x.insert(FieldIdent::Normal(field.to_string()), RefCell::new(value));
            },
            _ => {}
        }
    }

    fn proto_fields(&self) -> HashMap<FieldIdent, Ref<ValRef>> {
        // TODO fix this pls
        HashMap::new()
//        match self {
//            Value::Object(x) => {
//                let mut h = HashMap::new();
//                for (k, v) in x {
//                    if let FieldIdent::Proto(_) = k {
//                        h.insert(k.clone(), v.clone());
//                    }
//                }
//                h
//            },
//            _ => HashMap::new()
//        }
    }
}

pub trait Callable<'res>: Debug {
    fn arity(&self) -> usize;
    fn call<'a, 'pt>(
        &'res self,
        interpreter: &'res Interpreter<'res>,
        out: &'a mut External<'pt>,
        args: Vec<ValRef<'res>>,
    ) -> ExprResult<'res>;
}

struct NativeFn<'n> {
    arity: usize,
    body: fn(Vec<ValRef<'n>>, interpreter: &Interpreter, out: &mut External) -> ExprResult<'n>,
}

impl Debug for NativeFn<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "NativeFunction")
    }
}

impl<'nf> Callable<'nf> for NativeFn<'nf> {
    fn arity(&self) -> usize {
        return self.arity;
    }

    fn call<'a, 'pt>(
        &'nf self,
        interpreter: &'nf Interpreter<'nf>,
        out: &'a mut External<'pt>,
        args: Vec<ValRef<'nf>>,
    ) -> ExprResult<'nf> {
        (self.body)(args, interpreter, out)
    }
}

#[derive(Debug)]
struct RuntimeFn {
    params: Vec<Param>,
    body: Box<Expression>,
}

impl<'x> Callable<'x> for RuntimeFn {
    fn arity(&self) -> usize {
        self.params.len()
    }
    fn call<'a, 'pt>(
        &'x self,
        interpreter: &'x Interpreter<'x>,
        out: &'a mut External<'pt>,
        args: Vec<ValRef<'x>>,
    ) -> ExprResult<'x> {
        interpreter.environment.borrow_mut().wrap();
        for (param, arg) in self.params.iter().zip(args) {
            interpreter.environment.borrow_mut().scopes[0].insert(param.ident.to_string(), RefCell::new(arg));
        }
        let result = interpreter.visit(&self.body, out);
        interpreter.environment.borrow_mut().unwrap();
        return result;
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Value::Integer(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Callable { .. } => write!(f, "FunctionObject"),
            Value::Object(_) => write!(f, "Object"),
        }
    }
}

pub struct Environment<'a> {
    scopes: Vec<HashMap<String, RefCell<ValRef<'a>>>>,
}

impl Environment<'_> {
    pub fn new<'a>() -> Environment<'a> {
        let mut globals = HashMap::new();
        let clock = Value::Callable(Rc::new(NativeFn {
            arity: 0,
            body: |_, _, ext| {
                let time = (ext.clock)();
                ExprResult::Value(ValRef::Val(Value::Float(time)))
            },
        }));
        let print = Value::Callable(Rc::new(NativeFn {
            arity: 1,
            body: |mut args, _, out| {
                write!(out.output, "{}\n", args[0]);
                ExprResult::Value(args.remove(0))
            },
        }));
        //globals.insert("clock".to_string(), RefCell::new(ValRef::Val(clock)));
        //globals.insert("print".to_string(), RefCell::new(ValRef::Val(print)));
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
    pub environment: RefCell<Environment<'a>>,
}


impl<'a> Interpreter<'a> {
    fn visit<'x>(&'a self, expr: &Expression, passthrough: &mut External<'x>) -> ExprResult<'a> {
        match expr {
            Expression::Statement(x) => self.statement_expression(x, passthrough),
            Expression::Block(exprs) => self.block_expression(exprs, passthrough),
            Expression::Print(x) => self.print_expression(x, passthrough),
            Expression::Literal(x) => ExprResult::Value(ValRef::Val(self.literal_value(x))),
            Expression::Unary { kind, expr } => self.unary_value(kind, expr, passthrough),
            Expression::Binary { kind, operands } => self.binary_value(kind, operands, passthrough),
            Expression::Grouping(e) => self.visit(e, passthrough),
            Expression::Variable(s) => self.get_var(s),
            Expression::Assign(s, expr) => {
                let val = self.visit(expr, passthrough)?;
                self.set_var(s, val)
            },
            Expression::ObjectNew(parent, fields) => {
                let l = self.visit(parent, passthrough)?;
                let mut proto: HashMap<FieldIdent, RefCell<ValRef>> = l
                    .proto_fields()
                    .into_iter()
                    .map(|(k, v)|{
                        (match k {
                            FieldIdent::Proto(s) => FieldIdent::Normal(s),
                            x => x,
                        },
                        RefCell::new(ValRef::Ref(v)))
                    }).collect();
                let x: Result<Vec<(FieldIdent, RefCell<ValRef>)>, ExprError> = fields
                    .into_iter()
                    .map(|(k, v)|self.visit(v, passthrough)
                        .into_result()
                        .map(|value|(k.clone(), RefCell::new(value))))
                    .collect();
                let obj: HashMap<FieldIdent, RefCell<ValRef>> = x?.into_iter().collect();
                proto.extend(obj);
                ExprResult::Value(ValRef::Val(Value::Object(proto)))
            },
            Expression::FieldAccess(obj, field) => {
                let l = self.visit(obj, passthrough)?;
                l.field_access(field)
                    .map(|z|ExprResult::Value(ValRef::Ref(z)))
                    .unwrap_or(ExprResult::Err(self.err_build.create(0, 0, ErrorType::NonExistantField)))
            },
            Expression::FieldSet(obj, field, rhs) => {
                let mut l = self.visit(obj, passthrough)?;
                let r = self.visit(rhs, passthrough)?;
                l.field_set(field, r);
                ExprResult::Value(ValRef::Val(Value::String(
                    "THIS IS A NONE VALUE FROM SETTING FIELD".to_string(),
                )))
            },
            Expression::NonLocalAssign(s, expr) => {
                let val = self.visit(expr, passthrough)?;
                self.set_nonlocal(s, val)
            },
            Expression::Method(callee, field, args) => {
                let l = self.visit(callee, passthrough)?;
                let method = l.field_access(field);
                if let None = method {
                    return ExprResult::Err(self.err_build.create(0, 0, ErrorType::NonExistantField));
                }
                let method = method.unwrap();
                let mut arg_vals = Vec::with_capacity(1 + args.len());
                arg_vals.push(l);
                for arg in args {
                    arg_vals.push(self.visit(arg, passthrough)?);
                }
                self.run_callable(ValRef::Ref(method), arg_vals, passthrough)
            }
            Expression::If(cond, yes, no) => self.if_cond(cond, yes, no, passthrough),
            Expression::LogicOr(a, b) => self.logic_or(a, b, passthrough),
            Expression::LogicAnd(a, b) => self.logic_and(a, b, passthrough),
            Expression::While(cond, body) => self.while_loop(cond, body, passthrough),
            Expression::Call(callee, args) => self.call(callee, args, passthrough),
            Expression::Function(params, ret, body) => self.function(params, body, passthrough),
            Expression::Return(e) => self.retn(e, passthrough),
        }
    }


    pub fn new(err_build: &'a ErrorBuilder) -> Interpreter<'a> {
        Interpreter {
            err_build,
            environment: RefCell::new(Environment::new()),
        }
    }

    pub fn interpret<'x, 'pt>(
        &'a self,
        exprs: Vec<Expression>,
        out: &'x mut External<'pt>,
    ) -> Option<Error> {
        for expr in exprs {
            if let ExprResult::Err(e) = self.visit(&expr, out) {
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
            Literal::Object => Value::Object(HashMap::new()),
        }
    }

    fn statement_expression<'s, 'x>(
        &'a self,
        expr: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        self.visit(expr, out)
    }

    fn block_expression<'pt, 'x>(
        &'a self,
        exprs: &Vec<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        self.environment.borrow_mut().wrap();
        let mut last = ValRef::Val(Value::String("None type from block expression".to_string()));
        for expr in exprs {
            match self.visit(expr, out){
                ExprResult::ControlFlow(x) => {
                    self.environment.borrow_mut().unwrap();
                    return ExprResult::ControlFlow(x)
                },
                x => last = x?
            };
        }
        self.environment.borrow_mut().unwrap();
        return ExprResult::Value(last);
    }

    fn print_expression<'pt, 'x>(
        &'a self,
        expr: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let v = self.visit(expr, out)?;
        writeln!(out.output, "{}", v);
        ExprResult::Value(v)
    }

    fn unary_value<'pt, 'x>(
        &'a self,
        kind: &UnaryOperation,
        expr: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let v = self.visit(expr, out)?;
        let x = v.drill();
        match kind {
            UnaryOperation::Not => match x {
                Value::Bool(b) => ExprResult::Value(ValRef::Val(Value::Bool(!b))),
                v => ExprResult::Err(self
                    .err_build
                    .create(0, 0, ErrorType::InterpretBooleanNotWrongType)),
            },
            UnaryOperation::Minus => match x {
                Value::Integer(i) => ExprResult::Value(ValRef::Val(Value::Integer(-i))),
                Value::Float(f) => ExprResult::Value(ValRef::Val(Value::Float(-f))),
                v => ExprResult::Err(self.err_build.create(0, 0, ErrorType::InterpretUnaryMinus)),
            },
        }
    }

    fn binary_value<'pt, 'x>(
        &'a self,
        kind: &BinaryOperation,
        operands: &(Box<Expression>, Box<Expression>),
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let left = self.visit(&operands.0, out)?;
        let left = *left.drill();
        let right = self.visit(&operands.1, out)?;
        let right = *right.drill();
        match match kind {
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
        } {
            Ok(v) => ExprResult::Value(ValRef::Val(v)),
            Err(e) => ExprResult::Err(self.err_build.create(0, 0, e)),
        }
    }

    fn get_var(&'a self, s: &str) -> ExprResult<'a> {
        let scopes = &self.environment.borrow().scopes;
        for env in scopes {
            if env.contains_key(s) {
                return ExprResult::Value(ValRef::Ref(env.get(s).unwrap().borrow()));
            }
        }
        ExprResult::Err(self.err_build.create(0, 0, ErrorType::NonExistantVariable))
    }

    fn set_nonlocal(&self, s: &str, v: ValRef<'a>) -> ExprResult {
        let scopes = &mut self.environment.borrow_mut().scopes;
        for env in scopes {
            if env.contains_key(s) {
                env.insert(s.to_string(), RefCell::new(v));
                return ExprResult::Value(ValRef::Val(Value::String(
                    "THIS IS A NONE VALUE FROM SETTING NON LOCAL".to_string(),
                )));
            }
        }
        ExprResult::Err(self.err_build.create(0, 0, ErrorType::NonExistantVariable))
    }

    fn set_var(&'a self, s: &str, v: ValRef<'a>) -> ExprResult<'a> {
        self.environment.borrow_mut().scopes[0].insert(s.to_string(), RefCell::new(v));
        ExprResult::Value(ValRef::Val(Value::String(
            "THIS IS A NONE VALUE FROM SETTING VARIABLE".to_string(),
        )))
    }

    fn if_cond<'pt, 'x>(
        &'a self,
        cond: &Box<Expression>,
        yes: &Box<Expression>,
        no: &Option<Box<Expression>>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let cond_eval = self.visit(cond, out)?.drill();
        if let Value::Bool(true) = cond_eval {
            self.visit(yes, out)
        } else {
            no.as_ref().map_or(
                ExprResult::Value(ValRef::Val(Value::String(
                    "PLACE HOLDER NONE VALUE FROM IF ELSE BRANCH".to_string(),
                ))),
                |v| self.visit(&v, out),
            )
        }
    }

    fn while_loop<'pt, 'x>(
        &'a self,
        cond: &Box<Expression>,
        body: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let mut result = ValRef::Val(Value::String("NONE VALUE FROM WHILE LOOP".to_string()));
        loop {
            let eval = self.visit(cond, out)?.drill();
            if let Value::Bool(true) = eval {
                result = self.visit(body, out)?;
            } else {
                return ExprResult::Value(result);
            }
        }
    }

    fn call<'pt, 'x>(
        &'a self,
        callee: &Box<Expression>,
        args: &Vec<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let callable = self.visit(callee, out)?;
        let mut arguments_result = vec![];
        for arg in args {
            arguments_result.push(self.visit(arg, out)?);
        }
        let arguments_result = Ok(arguments_result);

        // TODO Rearrange this arguments thing

        //        let arguments_result: Result<Vec<Value>, Error> = args.iter().map(|arg|self.visit(arg, out)).collect();
        match arguments_result {
            Ok(args) => {
                self.run_callable(callable, args, out)
            }
            Err(e) => ExprResult::Err(e),
        }
    }

    fn run_callable<'pt, 'x>(&'a self, callable: ValRef<'a>, args: Vec<ValRef<'a>>, out: &mut External<'x>,) -> ExprResult<'a> {
        if let Value::Callable(call) = callable.drill() {
            if call.arity() == args.len() {
                match call.call(self, out, args) {
                    ExprResult::ControlFlow(ControlFlowConstruct::Return(x)) => ExprResult::Value(x.unwrap_or(ValRef::Val(Value::String("NONE RETURN FROM FUNCTION".to_string())))),
                    x => x,
                }
            } else {
                ExprResult::Err(self.err_build.create(0, 0, ErrorType::WrongNumberArgs))
            }
        } else {
            ExprResult::Err(self.err_build.create(0, 0, ErrorType::CallNonFunction))
        }
    }

    fn function<'pt, 'x>(
        &'a self,
        params: &Vec<Param>,
        body: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        // TODO the function needs to do closure stuff, which it doesn't right now :/
        // Try to capture the environment in which the closure is created, and store alongside (betting lifetime issues arise)
        ExprResult::Value(ValRef::Val(Value::Callable(Rc::new(RuntimeFn {
            params: params.to_vec(),
            body: body.clone(),
        }))))
    }

    fn retn<'pt, 'x>(
        &'a self,
        expr: &Option<Box<Expression>>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        expr.as_ref()
            .map(|e| match self.visit(e, out){
                ExprResult::Value(v) => ExprResult::ControlFlow(ControlFlowConstruct::Return(Some(v))),
                x => x,
            })
            .unwrap_or(ExprResult::ControlFlow(ControlFlowConstruct::Return(None)))
    }

    fn logic_or<'pt, 'x>(
        &'a self,
        a: &Box<Expression>,
        b: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let a_res = self.visit(a, out)?.drill();
        if let Value::Bool(true) = a_res {
            ExprResult::Value(ValRef::Val(Value::Bool(true)))
        } else {
            self.visit(b, out)
        }
    }

    fn logic_and<'pt, 'x>(
        &'a self,
        a: &Box<Expression>,
        b: &Box<Expression>,
        out: &mut External<'x>,
    ) -> ExprResult<'a> {
        let a_res = self.visit(a, out)?.drill();
        if let Value::Bool(true) = a_res {
            self.visit(b, out)
        } else {
            ExprResult::Value(ValRef::Val(Value::Bool(false)))
        }
    }
}

impl<'a> Add<Value<'a>> for Value<'a> {
    type Output = Result<Value<'a>, ErrorType>;
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

impl<'a> Sub<Value<'a>> for Value<'a> {
    type Output = Result<Value<'a>, ErrorType>;
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

impl PartialEq<Value<'_>> for Value<'_> {
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

impl PartialOrd<Value<'_>> for Value<'_> {
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

impl<'a> Mul<Value<'a>> for Value<'a> {
    type Output = Result<Value<'a>, ErrorType>;
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

impl<'a> Div<Value<'a>> for Value<'a> {
    type Output = Result<Value<'a>, ErrorType>;

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
