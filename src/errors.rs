use crate::interpreter::Value;
use crate::lex::tokens::{Token, TokenType};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ErrorType {
    ParseError(String),

    // Lexing errors
    NumberWithTrailingDot,
    UnexpectedCharacter(char),
    UnterminatedString,

    // Parsing errors
    UnclosedParen,
    UnexpectedToken(TokenType),
    UnexpectedEOF,
    MissingSemiColon,
    MissingRightBrace,
    InvalidAssignment,
    ConditionGrouping, // condition in if statement must be surrounded by brackets
    MissingRightParen,
    ParamFollowup, // Parameter must be followed either by comma or right paren
    ParamIdent,    // Parameter should be an identifier
    MissingParams, // Function didn't have parameters

    // Interpreting errors
    InterpretUnaryMinus,
    InterpretBooleanNotWrongType,
    AddBool,
    AddIncompatible,
    SubtractWrongTypes,
    MultiplyWrongTypes,
    DivideWrongTypes,
    NonExistantVariable,
    NonExistantField,
    CallNonFunction,
    WrongNumberArgs,
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrorType,
    location: usize,
    length: usize,
    input_name: String,
    input_string: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match &self.error_type {
            ErrorType::ParseError(s) => write!(f, "{}", s),
            _ => write!(f, "Error type {:?}\nLocation: {}", self.error_type, self.location)
        }
    }
}

pub struct ErrorBuilder {
    input_name: String,
    input_string: String,
}

impl ErrorBuilder {
    pub fn new(input_name: String, input_string: String) -> ErrorBuilder {
        ErrorBuilder {
            input_name,
            input_string,
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
