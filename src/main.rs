#![feature(exclusive_range_pattern)]

extern crate clap;
extern crate wasm_bindgen;
extern crate js_sys;
use clap::{App, Arg, SubCommand};
use wasm_bindgen::prelude::*;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::{fs, io};
use std::str;

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
use crate::interpreter::Interpreter;

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

#[wasm_bindgen]
pub fn run_wasm(input: &str, output: &js_sys::Function) {
    struct Writer<'a> {output: &'a js_sys::Function}
    impl<'a> std::io::Write for Writer<'a> {
        fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error> {
            self.output.call1(&JsValue::NULL, &JsValue::from(str::from_utf8(buf).unwrap()));
            return Ok(buf.len())
        }

        fn flush(&mut self) -> Result<(), std::io::Error> {
            Ok(())
        }
    }
    let mut w = Writer{output};
    let err_build = &ErrorBuilder::new("repl".to_string(), format!("Error in input"));
    run(input.to_string(), err_build, &mut Interpreter::new(err_build), &mut w);
}

fn run_file(file: &str) {
    let contents = fs::read_to_string(file).expect("Something went wrong reading the file");
    let x = &ErrorBuilder::new(file.to_string(), format!("Error in file: {}", file));
    run(
        contents,
        x,
        &Interpreter::new(x),
        &mut std::io::stdout()
    );
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut iterator = stdin.lock().lines();

    let err_build = &ErrorBuilder::new("input".to_string(), format!("Error in input"));
    let interpreter = &interpreter::Interpreter::new(err_build);

    loop {
        print!("> ");
        io::stdout().flush();
        let line = iterator.next().unwrap().unwrap();
        run(
            line,
            err_build,
            interpreter,
            &mut std::io::stdout()
        );
    }
}

fn run<'a, T: std::io::Write>(input: String, err_build: &ErrorBuilder, interpreter: &'a Interpreter<'a>, out: &'a mut T) {
    let (tokens, errors): (Vec<Result<Token, Error>>, Vec<Result<Token, Error>>) =
        Lexer::new(&input, err_build).partition(Result::is_ok);
    if errors.len() > 0 {
        errors.iter().for_each(|e| writeln!(out, "{:?}", e).unwrap_or(()));
        return;
    }
    let tokens = tokens.into_iter().map(Result::unwrap).collect::<Vec<_>>();
    let mut parser = Parser::new(tokens.iter(), err_build);
    match parser.parse() {
        Ok(x) => {
            if let Some(e) = (interpreter.interpret(x, out)) {
                writeln!(out, "{:?}", e);
            }
        },
        Err(e) => {
            writeln!(out, "{:?}", e);
        },
    }
}
