use super::expression::{Expression, ExpressionVisitor};
use crate::ast::expression::UnaryOperation;

pub struct Printer {}

impl ExpressionVisitor for Printer {
    type Item = String;
    fn visit(&self, expr: &Expression) -> String {
        expr.to_string()
    }
}
