use crate::errors::Error;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub location: usize,
    pub lexeme: String,
}

impl Token {
    pub fn new(location: usize, token_type: TokenType) -> Option<Result<Token, Error>> {
        Some(Ok(Token {
            token_type: token_type,
            location: location,
            lexeme: "".to_string(),
        }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    SemiColon,

    LeftParen,
    RightParen,

    LeftBrace,
    RightBrace,

    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Apostrophe,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,

    Identifier(String),
    String(String),
    Integer(i64),
    Float(f64),

    Import,
    And,
    Else,
    False,
    True,
    Fn,
    For,
    If,
    While,
    Or,
    Return,
}
