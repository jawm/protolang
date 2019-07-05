use super::expression::{Expression, ExpressionVisitor};
use crate::ast::expression::UnaryOperation;

pub struct Printer {}

impl ExpressionVisitor for Printer {
    type Item = String;
    type Passthrough = ();
    fn visit(&self, expr: &Expression, _: Self::Passthrough) -> String {
        expr.to_string()
    }
}
