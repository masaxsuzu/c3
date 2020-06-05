use crate::ast::{Node, Operator};

const REG: [&'static str; 6] = ["r10", "r11", "r12", "r13", "r14", "r15"];

pub fn gen(expr: Node) {
    print!(".intel_syntax noprefix\n");
    print!(".globl main\n");
    print!("main:\n");

    // Save callee-saved registers.
    print!("  push r12\n");
    print!("  push r13\n");
    print!("  push r14\n");
    print!("  push r15\n");

    let top = gen_expr(expr, 0);
    print!("  mov rax, {}\n", REG[top - 1]);

    print!("  pop r12\n");
    print!("  pop r13\n");
    print!("  pop r14\n");
    print!("  pop r15\n");

    print!("  ret\n");
}

fn gen_expr(expr: Node, top: usize) -> usize {
    match expr {
        Node::Number(i) => {
            print!("  mov {}, {}\n", REG[top], i);
            return top + 1;
        }
        Node::Binary(node, op) => {
            let top = gen_expr(node.left, top);
            let top = gen_expr(node.right, top);

            let rd = REG[top - 2];
            let rs = REG[top - 1];
            match op {
                Operator::Add => print!("  add {}, {}\n", rd, rs),
                Operator::Sub => print!("  sub {}, {}\n", rd, rs),
                Operator::Mul => print!("  imul {}, {}\n", rd, rs),
                Operator::Div => {
                    print!("  mov rax, {}\n", rd);
                    print!("  cqo\n");
                    print!("  idiv {}\n", rs);
                    print!("  mov {}, rax\n", rs);
                }
            }
            return top - 1;
        }
    }
}
