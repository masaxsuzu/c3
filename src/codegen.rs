use crate::ast::{Node, Operator, Program};

const REG: [&'static str; 6] = ["r10", "r11", "r12", "r13", "r14", "r15"];

pub fn gen(program: Program) {
    print!(".intel_syntax noprefix\n");
    print!(".globl main\n");
    print!("main:\n");

    // Prologue. r12-15 are callee-saved registers.
    print!("  push rbp\n");
    print!("  mov rbp, rsp\n");
    print!("  sub rsp, 240\n");
    print!("  mov [rbp-8], r12\n");
    print!("  mov [rbp-16], r13\n");
    print!("  mov [rbp-24], r14\n");
    print!("  mov [rbp-32], r15\n");

    for stmt in program.nodes {
        gen_stmt(stmt, 0);
    }

    // Epilogue
    print!(".L.return:\n");
    print!("  mov r12, [rbp-8]\n");
    print!("  mov r13, [rbp-16]\n");
    print!("  mov r14, [rbp-24]\n");
    print!("  mov r15, [rbp-32]\n");
    print!("  mov rsp, rbp\n");
    print!("  pop rbp\n");

    print!("  ret\n");
}

fn gen_stmt(node: Node, top: usize) -> usize {
    match node {
        Node::ExprStmt(stmt) => {
            let top = gen_expr(stmt.left, top) - 1;
            return top;
        }
        Node::Return(expr) => {
            let top = gen_expr(expr.left, top) - 1;
            print!("  mov rax, {}\n", REG[top]);
            print!("  jmp .L.return\n\n");
            return top;
        }
        _ => {
            unreachable!("must be statement\n");
        }
    }
}

fn gen_expr(expr: Node, top: usize) -> usize {
    match expr {
        Node::Number(i) => {
            print!("  mov {}, {}\n", REG[top], i);
            return top + 1;
        }
        Node::Variable(name) => {
            let top = gen_addr(Node::Variable(name), top);
            return load(top);
        }
        Node::Assign(node) => {
            let top = gen_expr(node.right, top);
            let top = gen_addr(node.left, top);
            return store(top);
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
                Operator::Eq => {
                    print!("  cmp {}, {}\n", rd, rs);
                    print!("  sete al\n");
                    print!("  movzb {}, al\n", rd);
                }
                Operator::Ne => {
                    print!("  cmp {}, {}\n", rd, rs);
                    print!("  setne al\n");
                    print!("  movzb {}, al\n", rd);
                }
                Operator::Lt => {
                    print!("  cmp {}, {}\n", rd, rs);
                    print!("  setl al\n");
                    print!("  movzb {}, al\n", rd);
                }
                Operator::Le => {
                    print!("  cmp {}, {}\n", rd, rs);
                    print!("  setle al\n");
                    print!("  movzb {}, al\n", rd);
                }
            }
            return top - 1;
        }
        _ => top,
    }
}

fn gen_addr(var: Node, top: usize) -> usize {
    if let Node::Variable(name) = var {
        let x = name.chars().next().unwrap() as u8;
        let base = 'a' as u8;
        let offset = ((x - base) + 1) * 8 + 32;
        print!("  lea {}, [rbp-{}]\n", REG[top], offset);
        return top + 1;
    }
    return top;
}

fn load(top: usize) -> usize {
    print!("  mov {}, [{}]\n", REG[top - 1], REG[top - 1]);
    top
}

fn store(top: usize) -> usize {
    print!("  mov [{}], {}\n", REG[top - 1], REG[top - 2]);
    top - 1
}
