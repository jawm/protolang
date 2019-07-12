use super::tokens::Token;
use std::str::Chars;
use core::borrow::Borrow;
use crate::lex::tokens::TokenType;
use std::iter::{Enumerate, Peekable};
use crate::errors::{ErrorBuilder, Error, ErrorType};

pub struct Lexer<'a> {
    chars: Peekable<Enumerate<Chars<'a>>>,
    err_build: &'a ErrorBuilder,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, err_build: &'a ErrorBuilder) -> Lexer<'a> {
        Lexer {
            chars: input.chars().enumerate().peekable(),
            err_build
        }
    }

    fn peek_match(&mut self, idx: usize, matcher: char, positive: TokenType, negative: TokenType) -> Option<Result<Token, Error>> {
        match self.chars.peek() {
            Some((_, x)) if *x == matcher => {
                self.chars.next();
                Token::new(idx, positive)
            },
            _ => Token::new(idx, negative)
        }
    }

    fn match_string(&mut self, idx: usize) -> Option<Result<Token, Error>> {
        let mut s = "".to_string();
        loop {
            match self.chars.next() {
                Some((_, '"')) => {
                    return Token::new(idx, TokenType::String(s));
                },
                Some((_, c)) => s.push(c),
                None => return Some(Err(self.err_build.create(idx, 1, ErrorType::UnterminatedString)))
            }
        }
    }

    fn swallow_comment(&mut self, idx: usize) -> Option<Result<Token, Error>> {
        loop {
            if let Some((idx, c)) = self.chars.next() {
                if c == '\n' {
                    return self.next();
                }
            } else {
                return None;
            }
        }
    }

    fn read_ident(&mut self, idx: usize, u: char) -> Option<Result<Token, Error>> {
        let mut n = u.to_string();
        loop {
            match self.chars.peek() {
                Some((_, c)) if c.is_ascii_alphanumeric() => n.push(*c),
                _ => return keyword_matcher(idx, &n)
            }
            self.chars.next();
        }
    }

    fn read_number(&mut self, idx: usize, u: char) -> Option<Result<Token, Error>> {
        // Read a number
        let mut n = u.to_string();
        let mut is_float = false;
        let token = loop {
            match self.chars.peek() {
                Some((_, digit)) if digit.is_numeric() => n.push(*digit),
                Some((_, '.')) => {
                    if is_float {
                        break Some(Err(self.err_build.create(idx, 1, ErrorType::NumberWithTrailingDot)))
                    }
                    is_float = true;
                    n.push('.');
                    self.chars.next();
                    match self.chars.peek() {
                        Some((_, digit)) if digit.is_numeric() => continue,
                        _ => break Some(Err(self.err_build.create(idx, 1, ErrorType::NumberWithTrailingDot)))
                    }
                }
                _ => break if is_float {
                    Token::new(idx, TokenType::Float(n.parse::<f64>().unwrap()))
                } else {
                    Token::new(idx, TokenType::Integer(n.parse::<i64>().unwrap()))
                }
            }
            self.chars.next();
        };
        token
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().and_then(|(idx, c)| match c {
            '#' => self.swallow_comment(idx),
            ' ' | '\r' | '\n' => self.next(), // ignore whitespace
            ';' => Token::new(idx, TokenType::SemiColon),
            '(' => Token::new(idx, TokenType::LeftParen),
            ')' => Token::new(idx, TokenType::RightParen),
            '{' => Token::new(idx, TokenType::LeftBrace),
            '}' => Token::new(idx, TokenType::RightBrace),
            ',' => Token::new(idx, TokenType::Comma),
            '.' => Token::new(idx, TokenType::Dot),
            '-' => Token::new(idx, TokenType::Minus),
            '+' => Token::new(idx, TokenType::Plus),
            '/' => Token::new(idx, TokenType::Slash),
            '*' => Token::new(idx, TokenType::Star),
            '\''=> Token::new(idx, TokenType::Apostrophe),
            '!' => self.peek_match(idx, '=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.peek_match(idx, '=', TokenType::EqualEqual, TokenType::Equal),
            '>' => self.peek_match(idx, '=', TokenType::GreaterEqual, TokenType::Greater),
            '<' => self.peek_match(idx, '=', TokenType::LesserEqual, TokenType::Lesser),
            '"' => self.match_string(idx),
            u if u.is_ascii_alphabetic() => self.read_ident(idx, u),
            u if u.is_numeric() => self.read_number(idx, u),
            x => Some(Err(self.err_build.create(idx, 1, ErrorType::UnexpectedCharacter(x))))
        })
    }
}

fn keyword_matcher(idx: usize, s: &str) -> Option<Result<Token,Error>> {
    Token::new(idx, match s {
        "import" => TokenType::Import,
        "nonlocal" => TokenType::NonLocal,
        "and" => TokenType::And,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "true" => TokenType::True,
        "fn" => TokenType::Fn,
        "for" => TokenType::For,
        "if" => TokenType::If,
        "while" => TokenType::While,
        "or" => TokenType::Or,
        "return" => TokenType::Return,
        _ => TokenType::Identifier(s.to_string())
    })
}