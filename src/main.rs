extern crate c3;

use c3::codegen::gen;
use c3::parser::parse;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("{}: invalid number of arguments", args[0]);
    }

    print!(".intel_syntax noprefix\n");
    print!(".globl main\n");
    print!("main:\n");

    // Save callee-saved registers.
    print!("  push r12\n");
    print!("  push r13\n");
    print!("  push r14\n");
    print!("  push r15\n");

    if let Ok(expr) = parse(&args[1]) {
        gen(expr);
    } else {
        panic!("error");
    }

    print!("  pop r12\n");
    print!("  pop r13\n");
    print!("  pop r14\n");
    print!("  pop r15\n");

    print!("  ret\n");
}
