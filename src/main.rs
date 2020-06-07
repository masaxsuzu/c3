extern crate c3;
use c3::codegen::CodeGenerator;
use c3::error::Error;
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
        args[1].len(),
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
        Err(Error::ParseError(msg, token)) => {
            let pos = match token {
                Token::Eof(p) => p,
                Token::Identifier(_, p) => p,
                Token::Illegal(_, p) => p,
                Token::Number(_, p) => p,
                Token::Reserved(_, p) => p,
            };
            eprintln!(
                "{}",
                Error::DisplayError("line".to_string(), args[1].to_string(), pos, msg)
            );
            1
        }
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    });
}
