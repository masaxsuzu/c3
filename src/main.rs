extern crate c3;

use c3::codegen::gen;
use c3::parser::parse;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    if let Ok(expr) = parse(&args[1]) {
        gen(expr);
    } else {
        panic!("error");
    }
}
