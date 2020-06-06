extern crate c3;
use c3::codegen::{CodeGenerator};
use c3::lexer::Lexer;
use c3::parser::{Parser, Tokens};
use c3::token::Token;
use std::env;
use std::rc::Rc;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }
    let tokens = Tokens::new(
        0,
        Rc::new(Lexer::new(&args[1]).into_iter().collect::<Vec<Token>>()),
    );
    let mut parser = Parser::new();
    let mut codegen = CodeGenerator::new();
    std::process::exit(match parser.parse(tokens) {
        Ok(program) => {
            codegen.gen(program);
            0
        }
        Err(err) => {
            eprintln!("error: {:?}", err);
            1
        }
    });
}
