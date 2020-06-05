extern crate c3;
use c3::codegen::gen;
use c3::lexer::Lexer;
use c3::parser::{parse, Parser};
use c3::token::Token;
use std::env;
use std::rc::Rc;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }
    let x = Lexer::new(&args[1]).into_iter().collect::<Vec<Token>>();
    let p = Parser::new(0, Rc::new(x));
    std::process::exit(match parse(p) {
        Ok(program) => {
            gen(program);
            0
        }
        Err(err) => {
            eprintln!("error: {:?}", err);
            1
        }
    });
}
