#![feature(exclusive_range_pattern)]
#![feature(duration_float)]
#![feature(in_band_lifetimes)]
#![feature(try_trait)]

extern crate clap;
extern crate js_sys;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate wasm_bindgen;
use clap::{App, Arg, SubCommand};
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::str;
use std::time::{SystemTime, UNIX_EPOCH};
use std::{fs, io};
use wasm_bindgen::prelude::*;

mod errors;
use errors::ErrorBuilder;
mod ast;

mod lex;
use lex::tokens::Token;
use lex::Lexer;

mod parse;
use crate::errors::Error;
//use parse::Parser;

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

fn pest_parse(input: &str) {
    let unparsed_file = fs::read_to_string(input).expect("Input should be a valid filename");
    println!("{:?}", parse::parse(unparsed_file.as_ref()));
}

#[wasm_bindgen]
pub fn run_wasm(input: &str, output: &js_sys::Function) {
    struct Writer<'a> {
        output: &'a js_sys::Function,
    }
    impl<'a> std::io::Write for Writer<'a> {
        fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error> {
            self.output
                .call1(&JsValue::NULL, &JsValue::from(str::from_utf8(buf).unwrap()));
            return Ok(buf.len());
        }

        fn flush(&mut self) -> Result<(), std::io::Error> {
            Ok(())
        }
    }
    let mut w = Writer { output };

    let err_build = &ErrorBuilder::new("repl".to_string(), format!("Error in input"));

    let mut ext = External {
        output: &mut w,
        clock: js_sys::Date::now,
    };
    run(
        input.to_string(),
        err_build,
        &mut Interpreter::new(err_build),
        &mut ext,
    );
}

fn run_file(file: &str) {
    pest_parse(file);
//    let contents = fs::read_to_string(file).expect("Something went wrong reading the file");
//    let x = &ErrorBuilder::new(file.to_string(), format!("Error in file: {}", file));
//    run(
//        contents,
//        x,
//        &Interpreter::new(x),
//        &mut External {
//            output: &mut std::io::stdout(),
//            clock: || {
//                SystemTime::now()
//                    .duration_since(UNIX_EPOCH)
//                    .unwrap()
//                    .as_secs_f64()
//            },
//        },
//    );
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
            &mut External {
                output: &mut std::io::stdout(),
                clock: || {
                    SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64()
                },
            },
        );
    }
}

struct External<'a> {
    output: &'a mut std::io::Write,
    clock: fn() -> f64,
}

fn run<'a, 'x, 's>(
    input: String,
    err_build: &ErrorBuilder,
    interpreter: &'a Interpreter<'s>,
    ext: &'a mut External<'a>,
) {
//    let (tokens, errors): (Vec<Result<Token, Error>>, Vec<Result<Token, Error>>) =
//        Lexer::new(&input, err_build).partition(Result::is_ok);
//    if errors.len() > 0 {
//        errors
//            .iter()
//            .for_each(|e| writeln!(ext.output, "{:?}", e).unwrap_or(()));
//        return;
//    }
//    let tokens = tokens.into_iter().map(Result::unwrap).collect::<Vec<_>>();
//    let mut parser = Parser::new(tokens.iter(), err_build);
//    match parser.parse() {
//        Ok(x) => {
////            println!("{:?}", x);
//            if let Some(e) = interpreter.interpret(x, ext) {
//                writeln!(ext.output, "{:?}", e);
//            }
//        }
//        Err(e) => {
//            writeln!(ext.output, "{:?}", e);
//        }
//    }
}
