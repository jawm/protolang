use crate::ast::expression;
use crate::ast::expression::{Expression, AccessType, Return};
use crate::ast::expression::Param;
use crate::errors::{Error, ErrorBuilder, ErrorType};
use crate::lex::tokens::{Token, TokenType};
use std::convert::TryInto;
use std::iter::Peekable;
use pest::Parser;
use pest::iterators::{Pairs, Pair};
use wasm_bindgen::__rt::core::hint::unreachable_unchecked;
use wasm_bindgen::__rt::std::process::exit;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct WaveParser;

pub fn parse(input: &str) -> Result<Vec<Expression>, Error>{
    let parsed = WaveParser::parse(Rule::file, input).expect("Syntax should be valid")
        .next().unwrap(); // we can unwrap here, since if the result is Ok, then it must contain the rule we asked for.
    match_multiple_exprs(parsed.into_inner())
}

fn match_multiple_exprs(pairs: Pairs<Rule>) -> Result<Vec<Expression>, Error> {
    let mut exprs = vec![];
    for record in pairs {
        if let Rule::expression = record.as_rule() {
            exprs.push(match_expression(record)?);
        } else {
            unreachable!();
        }
    }
    Ok(exprs)
}

fn match_expression(pair: Pair<Rule>) -> Result<Expression, Error> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::block => Ok(Expression::Block(match_multiple_exprs(pair.into_inner().next().unwrap().into_inner())?)),
        Rule::assign => match_assign(pair.into_inner()),
        Rule::function => match_function(pair.into_inner()),
        Rule::body_ret => match_return(pair.into_inner()),
        Rule::if_expr => match_if(pair.into_inner()),
        x => unreachable!(),
    }
}

fn match_assign(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let ident = pairs.next().unwrap();
    let expr = match_expression(pairs.next().unwrap())?;
    Ok(Expression::Assign(ident.as_str().to_string(), Box::new(expr)))
}

fn match_function(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let mut params = vec![];
    let mut n = pairs.next().unwrap();
    if let Rule::params = n.as_rule() {

        for p in n.into_inner() {
            let param = p.into_inner().next().unwrap();
            let access_type = match param.as_rule() {
                Rule::shared_param => AccessType::Shared,
                Rule::excl_param => AccessType::Excl,
                Rule::move_param => AccessType::Move,
                _ => unreachable!()
            };
            let mut inner = param.into_inner();
            let name = inner.next().unwrap();
            let mut lifetime = None;
            if let Some(lt) = inner.next() {
                lifetime = Some(lt.into_inner().next().unwrap().as_str().to_string());
            }
            params.push(Param {
                ident: name.as_str().to_string(),
                lifetime: lifetime,
                access_type: access_type,
            })
        }

        n = pairs.next().unwrap();
    }
    // At this point, n *must* be a signature_ret
    let ret = n.into_inner().next().unwrap();
    let return_type = match ret.as_rule() {
        Rule::no_return => None,
        Rule::shared_return => {
            let mut lifetime_param = None;
            if let Some(lifetime) = ret.into_inner().next() {
                lifetime_param = Some(lifetime.into_inner().next().unwrap().as_str().to_string());
            }
            Some(Return{
                lifetime: lifetime_param,
                access_type: AccessType::Shared,
            })
        },
        Rule::excl_return => {
            let mut lifetime_param = None;
            if let Some(lifetime) = ret.into_inner().next() {
                lifetime_param = Some(lifetime.into_inner().next().unwrap().as_str().to_string());
            }
            Some(Return{
                lifetime: lifetime_param,
                access_type: AccessType::Excl,
            })
        },
        Rule::move_return => Some(Return{
            lifetime: None,
            access_type: AccessType::Move
        }),
        _ => unreachable!()
    };

    // Now we need the body of the function:
    let body = match_expression(pairs.next().unwrap())?;

    Ok(Expression::Function(params, return_type, Box::new(body)))
}

fn match_return(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    Ok(Expression::Return(
        if let Some(e) = pairs.next() {
            Some(Box::new(match_expression(e)?))
        } else {
            None
        }
    ))
}

fn match_if(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let n = pairs.next().unwrap();
    if let Rule::if_help = n.as_rule() {
        let mut if_expr = n.into_inner();
        let condition = match_expression(if_expr.next().unwrap())?;
        let body = match_expression(if_expr.next().unwrap())?;
        let mut else_body = None;
        if let Some(el) = if_expr.next() {
            else_body = Some(Box::new(match_expression(el)?));
        }
        Ok(Expression::If(Box::new(condition), Box::new(body), else_body))

    } else {
        match_while(n.into_inner())
    }
}

fn match_while(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let n = pairs.next().unwrap();
    if let Rule::while_help = n.as_rule() {
        let mut if_expr = n.into_inner();
        let condition = match_expression(if_expr.next().unwrap())?;
        let body = match_expression(if_expr.next().unwrap())?;
        Ok(Expression::While(Box::new(condition), Box::new(body)))
    } else {
        match_or(n.into_inner())
    }
}

fn binary_helper(mut pairs: Pairs<Rule>, next: fn(Pairs<Rule>)->Result<Expression, Error>) -> Result<Expression, Error> {
    let mut result = next(pairs.next().unwrap().into_inner())?;
    while let Some(x) = pairs.next() {
        let mut x = x.into_inner();
        let operator = x.next().unwrap().as_str().into();
        let right = next(x.next().unwrap().into_inner())?;
        result = Expression::Binary {
            kind: operator,
            operands: (Box::new(result), Box::new(right))
        }
    }
    Ok(result)
}

fn match_or(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let mut result = match_and(pairs.next().unwrap().into_inner())?;
    while let Some(x) = pairs.next() {
        let mut x = x.into_inner();
        x.next();
        let right = match_and(x.next().unwrap().into_inner())?;
        result = Expression::LogicOr(Box::new(result), Box::new(right));
    }
    Ok(result)
}

fn match_and(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let mut result = match_equality(pairs.next().unwrap().into_inner())?;
    while let Some(x) = pairs.next() {
        let mut x = x.into_inner();
        x.next();
        let right = match_equality(x.next().unwrap().into_inner())?;
        result = Expression::LogicAnd(Box::new(result), Box::new(right));
    }
    Ok(result)
}

fn match_equality(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    binary_helper(pairs, match_comparison)
}

fn match_comparison(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    binary_helper(pairs, match_addition)
}

fn match_addition(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    println!("{:#?}", pairs);
    binary_helper(pairs, match_multiplication)
}

fn match_multiplication(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    binary_helper(pairs, match_unary)
}

fn match_unary(mut pairs: Pairs<Rule>) -> Result<Expression, Error> {
    let next = pairs.next().unwrap();
    match next.as_rule() {
        Rule::unary_op => {
            let operator = next.as_str().into();
            let expr = match_unary(pairs)?;
            Ok(Expression::Unary {
                kind: operator,
                expr: Box::new(expr)
            })
        },
        Rule::call => Ok(Expression::Literal(expression::Literal::True)),
        x => {
            println!("{:?}", x);
            unreachable!()
        }
    }
}

/*



pub struct Parser<'a, T: Iterator<Item = &'a Token>> {
    tokens: Peekable<T>,
    err_build: &'a ErrorBuilder,
}

impl<'a, T: Iterator<Item = &'a Token>> Parser<'a, T> {
    pub fn new(tokens: T, err_build: &'a ErrorBuilder) -> Parser<'a, T> {
        Parser {
            tokens: tokens.peekable(),
            err_build,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<expression::Expression>, Error> {
        let exprs = self.read_multiple_exprs()?;
        Ok(exprs)
    }

    fn read_multiple_exprs(&mut self) -> Result<Vec<expression::Expression>, Error> {
        let mut exprs = vec![];
        while let Some(_) = self.tokens.peek() {
            let expr = self.statement()?;
            if let expression::Expression::Statement(ex) = &expr {
                exprs.push(expr);
                continue;
            } else if self.tokens.peek().is_some() {
                let next = self.tokens.next().unwrap();
                return Err(self.err_build.create(
                    next.location,
                    0,
                    ErrorType::UnexpectedToken(next.token_type.clone()),
                ));
            } else {
                exprs.push(expr);
                break;
            }
        }
        Ok(exprs)
    }

    fn expression(&mut self) -> Result<expression::Expression, Error> {
        self.block()
    }

    fn statement(&mut self) -> Result<expression::Expression, Error> {
        let mut expr = self.expression()?;
        if let Some(Token {
            token_type: TokenType::SemiColon,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            Ok(expression::Expression::Statement(Box::new(expr)))
        } else {
            Ok(expr)
        }
    }

    fn block(&mut self) -> Result<expression::Expression, Error> {
        if let Some(Token {
            token_type: TokenType::LeftBrace,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            let mut exprs = vec![];
            loop {
                match self.tokens.peek() {
                    None => return Err(self.err_build.create(0, 0, ErrorType::MissingRightBrace)),
                    Some(Token {
                        token_type: TokenType::RightBrace,
                        ..
                    }) => break,
                    _ => {
                        let expr = self.statement()?;
                        if let Expression::Statement(_) = expr {
                            exprs.push(expr);
                        } else {
                            exprs.push(expr);
                            break;
                        }
                    }
                }
            }
            self.tokens.next(); // consume the right brace
            return Ok(Expression::Block(exprs));
        }
        return self.assign();
    }

    fn function(&mut self) -> Result<expression::Expression, Error> {
        if let Some(Token {
            token_type: TokenType::Fn,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            if let Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) = self.tokens.next()
            {
                // parse arguments
                let mut params = vec![];
                loop {
                    match self.tokens.next() {
                        Some(Token {
                            token_type: TokenType::Identifier(s),
                            ..
                        }) => {
                            params.push(Expression::Variable(s.to_string()));
                            match self.tokens.next() {
                                Some(Token {
                                    token_type: TokenType::Comma,
                                    ..
                                }) => continue,
                                Some(Token {
                                    token_type: TokenType::RightParen,
                                    ..
                                }) => break,
                                n => {
                                    return Err(self.err_build.create(
                                        0,
                                        1,
                                        ErrorType::ParamFollowup,
                                    ))
                                }
                            }
                        }
                        Some(Token {
                            token_type: TokenType::RightParen,
                            ..
                        }) => break,
                        Some(n) => {
                            return Err(self.err_build.create(n.location, 1, ErrorType::ParamIdent))
                        }
                        None => return Err(self.err_build.create(0, 0, ErrorType::UnexpectedEOF)),
                    }
                }
                let block = self.block()?;
                return Ok(Expression::Function(params, Box::new(block)));
            } else {
                return Err(self.err_build.create(0, 0, ErrorType::MissingParams));
            }
            self.print()
        } else {
            self.print()
        }
    }

    fn assign(&mut self) -> Result<expression::Expression, Error> {
        let nonlocal = if let Some(Token {
            token_type: TokenType::NonLocal,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            true
        } else {
            false
        };
        let mut l = self.function()?;
        if let Some(Token {
            token_type: TokenType::Equal,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            let right = self.assign()?;
            if let Expression::Variable(name) = l {
                if nonlocal {
                    return Ok(Expression::NonLocalAssign(name, Box::new(right)));
                }
                return Ok(Expression::Assign(name, Box::new(right)));
            } else {
                return Err(self.err_build.create(0, 0, ErrorType::InvalidAssignment));
            }
        } else if nonlocal {
            return Err(self.err_build.create(
                0,
                0,
                ErrorType::UnexpectedToken(TokenType::NonLocal),
            ));
        }
        Ok(l)
    }

    // TODO remove print statement in favour of a function for various reasons
    fn print(&mut self) -> Result<Expression, Error> {
        if let Some(Token {
            token_type: TokenType::Identifier(ident),
            ..
        }) = self.tokens.peek()
        {
            if ident == "print" {
                self.tokens.next();
                return Ok(Expression::Print(Box::new(self.retn()?)));
            }
        }
        self.retn()
    }

    fn retn(&mut self) -> Result<Expression, Error> {
        if let Some(Token {
                        token_type: TokenType::Return,
                        ..
                    }) = self.tokens.peek()
        {
            self.tokens.next();
            if let Some(Token {token_type: TokenType::SemiColon, ..}) = self.tokens.peek() {
                return Ok(Expression::Return(None))
            } else {
                return Ok(Expression::Return(Some(Box::new(self.if_cond()?))));
            }
        }
        self.if_cond()
    }

    fn if_cond(&mut self) -> Result<Expression, Error> {
        if let Some(Token {
            token_type: TokenType::If,
            ..
        }) = self.tokens.peek()
        {
            let token = self.tokens.next().unwrap();
            if let Ok(Expression::Grouping(expr)) = self.primary() {
                let yes = self.expression()?;
                if let Some(Token {
                    token_type: TokenType::Else,
                    ..
                }) = self.tokens.peek()
                {
                    self.tokens.next();
                    let no = self.expression()?;
                    return Ok(Expression::If(expr, Box::new(yes), Some(Box::new(no))));
                }
                return Ok(Expression::If(expr, Box::new(yes), None));
            } else {
                return Err(self.err_build.create(
                    token.location + 1,
                    1,
                    ErrorType::ConditionGrouping,
                ));
            }
        }
        self.while_loop()
    }

    fn while_loop(&mut self) -> Result<Expression, Error> {
        if let Some(Token {
            token_type: TokenType::While,
            ..
        }) = self.tokens.peek()
        {
            let token = self.tokens.next().unwrap();
            if let Ok(Expression::Grouping(expr)) = self.primary() {
                let body = self.expression()?;
                return Ok(Expression::While(expr, Box::new(body)));
            } else {
                return Err(self.err_build.create(
                    token.location + 1,
                    1,
                    ErrorType::ConditionGrouping,
                ));
            }
        }
        self.logic_or()
    }

    fn logic_or(&mut self) -> Result<Expression, Error> {
        let mut expr = self.logic_and()?;
        while let Some(Token {
            token_type: TokenType::Or,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            let right = self.logic_and()?;
            expr = Expression::LogicOr(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expression, Error> {
        let mut expr = self.equality()?;
        while let Some(Token {
            token_type: TokenType::And,
            ..
        }) = self.tokens.peek()
        {
            self.tokens.next();
            let right = self.equality()?;
            expr = Expression::LogicAnd(Box::new(expr), Box::new(right));
        }
        Ok(expr)
    }

    fn binary_helper(
        &mut self,
        matcher: impl Fn(&TokenType) -> bool,
        left: expression::Expression,
        right: fn(&mut Parser<'a, T>) -> Result<expression::Expression, Error>,
    ) -> Result<expression::Expression, Error> {
        let mut expr = left;
        loop {
            match self.tokens.peek() {
                Some(Token { token_type: x, .. }) if matcher(x) => {
                    let token = self.tokens.next().unwrap().try_into().unwrap();
                    let expr_right = right(self);
                    expr = expression::Expression::Binary {
                        kind: token,
                        operands: (Box::new(expr), Box::new(expr_right?)),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<expression::Expression, Error> {
        let expr = self.comparison()?;
        self.binary_helper(
            |x| *x == TokenType::BangEqual || *x == TokenType::EqualEqual,
            expr,
            Parser::comparison,
        )
    }

    fn comparison(&mut self) -> Result<expression::Expression, Error> {
        let expr = self.addition()?;
        self.binary_helper(
            |x| {
                *x == TokenType::Greater
                    || *x == TokenType::GreaterEqual
                    || *x == TokenType::Lesser
                    || *x == TokenType::LesserEqual
            },
            expr,
            Parser::addition,
        )
    }

    fn addition(&mut self) -> Result<expression::Expression, Error> {
        let expr = self.multiplication()?;
        self.binary_helper(
            |x| *x == TokenType::Minus || *x == TokenType::Plus,
            expr,
            Parser::multiplication,
        )
    }

    fn multiplication(&mut self) -> Result<expression::Expression, Error> {
        let expr = self.unary()?;
        self.binary_helper(
            |x| *x == TokenType::Slash || *x == TokenType::Star,
            expr,
            Parser::unary,
        )
    }

    fn unary(&mut self) -> Result<expression::Expression, Error> {
        match self.tokens.peek() {
            Some(Token { token_type: x, .. })
                if *x == TokenType::Bang || *x == TokenType::Minus =>
            {
                Ok(expression::Expression::Unary {
                    kind: self.tokens.next().unwrap().try_into().unwrap(),
                    expr: Box::new(self.unary()?),
                })
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<Expression, Error> {
        let mut expr = self.primary()?;
        loop {
            if let Some(Token {
                token_type: TokenType::LeftParen,
                ..
            }) = self.tokens.peek()
            {
                self.tokens.next();
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expression) -> Result<Expression, Error> {
        let mut args = vec![];
        if let Some(Token {
            token_type: TokenType::RightParen,
            ..
        }) = self.tokens.peek()
        {
        } else {
            loop {
                args.push(self.expression()?);
                if let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = self.tokens.peek()
                {
                    self.tokens.next();
                } else {
                    break;
                }
            }
        }
        match self.tokens.next() {
            Some(Token {
                token_type: TokenType::RightParen,
                ..
            }) => Ok(Expression::Call(Box::new(expr), args)),
            Some(tok) => Err(self
                .err_build
                .create(tok.location, 0, ErrorType::MissingRightParen)),
            None => Err(self.err_build.create(0, 0, ErrorType::UnexpectedEOF)),
        }
    }

    fn primary(&mut self) -> Result<expression::Expression, Error> {
        if let Some(token) = self.tokens.next() {
            match &token.token_type {
                TokenType::True => Ok(expression::Expression::Literal(expression::Literal::True)),
                TokenType::False => Ok(expression::Expression::Literal(expression::Literal::False)),
                TokenType::Integer(i) => Ok(expression::Expression::Literal(
                    expression::Literal::Integer(*i),
                )),
                TokenType::Float(f) => Ok(expression::Expression::Literal(
                    expression::Literal::Float(*f),
                )),
                TokenType::String(s) => Ok(expression::Expression::Literal(
                    expression::Literal::String(s.to_string()),
                )),
                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(Token {
                        token_type: TokenType::RightParen,
                        ..
                    }) = self.tokens.next()
                    {
                        Ok(expression::Expression::Grouping(Box::new(expr)))
                    } else {
                        Err(self
                            .err_build
                            .create(token.location, 1, ErrorType::UnclosedParen))
                    }
                }
                TokenType::Identifier(s) => Ok(Expression::Variable(s.to_string())),
                x => Err(self.err_build.create(
                    token.location,
                    1,
                    ErrorType::UnexpectedToken(x.clone()),
                )),
            }
        } else {
            Err(self.err_build.create(0, 0, ErrorType::UnexpectedEOF))
        }
    }
}
*/