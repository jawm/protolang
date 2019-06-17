#![feature(exclusive_range_pattern)]

extern crate clap;
use clap::{App, Arg, SubCommand};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::{fs, io};

mod errors;
use errors::ErrorBuilder;
mod ast;

mod lex;
use lex::tokens::Token;
use lex::Lexer;

mod parse;
use crate::errors::Error;
use parse::Parser;

use crate::ast::expression::ExpressionVisitor;

mod interpreter;

fn main() {
    let matches = App::new("Interpreter for unnamed language")
        .version("0.0.1")
        .author("James Morrison")
        .arg(
            Arg::with_name("INPUT")
                .help("The file to be executed")
                .required(false)
                .index(1),
        )
        .get_matches();

    if let Some(file) = matches.value_of("INPUT") {
        run_file(file);
    } else {
        run_prompt();
    }
}

fn run_file(file: &str) {
    let contents = fs::read_to_string(file).expect("Something went wrong reading the file");
    run(
        contents,
        &ErrorBuilder::new(file.to_string(), format!("Error in file: {}", file)),
    );
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut iterator = stdin.lock().lines();
    loop {
        print!("> ");
        io::stdout().flush();
        let line = iterator.next().unwrap().unwrap();
        run(
            line,
            &ErrorBuilder::new("input".to_string(), format!("Error in input")),
        );
    }
}

fn run(input: String, err_build: &ErrorBuilder) {
    let (tokens, errors): (Vec<Result<Token, Error>>, Vec<Result<Token, Error>>) =
        Lexer::new(&input, err_build).partition(Result::is_ok);
    if errors.len() > 0 {
        errors.iter().for_each(|e| println!("{:?}", e));
        return;
    }
    let tokens = tokens.into_iter().map(Result::unwrap).collect::<Vec<_>>();
//    println!("{:?}", tokens);
    let mut parser = Parser::new(tokens.iter(), err_build);
    match parser.parse() {
        Ok(x) => {
            println!("{}", x);
//            println!("{:?}", interpreter::Interpreter {err_build}.visit(&x));
            println!("{:?}", interpreter::Interpreter {err_build}.visit(&x));
        },
        Err(e) => println!("{:?}", e)
    }
}
