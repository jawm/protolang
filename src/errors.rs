use crate::lex::tokens::{Token, TokenType};
use crate::interpreter::Value;

#[derive(Debug)]
pub enum ErrorType {
    // Lexing errors
    NumberWithTrailingDot,
    UnexpectedCharacter(char),
    UnterminatedString,

    // Parsing errors
    UnclosedParen,
    UnexpectedToken(TokenType),
    UnexpectedEOF,
    MissingSemiColon,
    InvalidAssignment,

    // Interpreting errors
    InterpretUnaryMinus,
    InterpretBooleanNotWrongType,
    AddBool,
    SubtractWrongTypes,
    MultiplyWrongTypes,
    DivideWrongTypes,
    NonExistantVariable,
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrorType,
    location: usize,
    length: usize,
    input_name: String,
    input_string: String
}

pub struct ErrorBuilder {
    input_name: String,
    input_string: String,
}

impl ErrorBuilder {
    pub fn new(input_name: String, input_string: String) -> ErrorBuilder {
        ErrorBuilder {
            input_name,
            input_string
        }
    }

    pub fn create(&self, location: usize, length: usize, error_type: ErrorType) -> Error {
        Error {
            error_type,
            location,
            length,
            input_string: self.input_string.clone(),
            input_name: self.input_name.clone(),
        }
    }
}