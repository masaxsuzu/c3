use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Node, Operator, Program, Variable};

const REG: [&'static str; 6] = ["r10", "r11", "r12", "r13", "r14", "r15"];

pub struct CodeGenerator {
    label_seq: i64,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { label_seq: 0 }
    }

    pub fn gen(&mut self, mut program: Program) {
        compute_offset(&mut program);

        // eprintln!("{:?}\n",program);

        print!(".intel_syntax noprefix\n");
        print!(".globl main\n");
        print!("main:\n");

        // Prologue. r12-15 are callee-saved registers.
        print!("  push rbp\n");
        print!("  mov rbp, rsp\n");
        print!("  sub rsp, {}\n", program.stack_size);
        print!("  mov [rbp-8], r12\n");
        print!("  mov [rbp-16], r13\n");
        print!("  mov [rbp-24], r14\n");
        print!("  mov [rbp-32], r15\n");

        // eprintln!("{:?}", program.stmt);
        self.gen_stmt(&program.stmt, 0);

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

    fn gen_stmt(&mut self, node: &Node, mut top: usize) -> usize {
        match node {
            Node::BlockStmt(block) => {
                for stmt in block.nodes.iter() {
                    top = self.gen_stmt(stmt, top);
                }
                return top;
            }
            Node::ExprStmt(stmt) => {
                let top = self.gen_expr(&stmt.left, top) - 1;
                return top;
            }
            Node::Return(expr) => {
                let top = self.gen_expr(&expr.left, top) - 1;
                print!("  mov rax, {}\n", REG[top]);
                print!("  jmp .L.return\n");
                return top;
            }
            Node::If(_if) => {
                self.label_seq += 1;
                let seq = self.label_seq;

                let top = if _if.otherwise != Node::Null {
                    let top = self.gen_expr(&_if.cond, top) - 1;
                    print!("  cmp %s, {}\n", REG[top]);
                    print!("  je  .L.else.{}\n", seq);
                    let top = self.gen_stmt(&_if.then, top);
                    print!("  jmp .L.end.{}\n", seq);
                    print!(".L.else.{}:\n", seq);
                    let top = self.gen_stmt(&_if.otherwise, top);
                    print!(".L.end.{}:\n", seq);
                    top
                } else {
                    let top = self.gen_expr(&_if.cond, top) - 1;
                    print!("  cmp {}, 0\n", REG[top]);
                    print!("  je  .L.end.{}\n", seq);
                    let top = self.gen_stmt(&_if.then, top);
                    print!(".L.end.{}:\n", seq);
                    top
                };
                return top;
            }
            Node::Loop(_for) => {
                self.label_seq += 1;
                let seq = self.label_seq;

                let top = if _for.init != Node::Null {
                    self.gen_stmt(&_for.init, top)
                } else {
                    top
                };

                print!(".L.begin.{}:\n", seq);

                let top = if _for.cond != Node::Null {
                    let top = self.gen_expr(&_for.cond, top) - 1;
                    print!("  cmp {}, 0\n", REG[top]);
                    print!("  je  .L.end.{}\n", seq);
                    top
                } else {
                    top
                };

                let top = self.gen_stmt(&_for.then, top);

                let top = if _for.inc != Node::Null {
                    self.gen_stmt(&_for.inc, top)
                } else {
                    top
                };

                print!("  jmp  .L.begin.{}\n", seq);
                print!(".L.end.{}:\n", seq);

                return top;
            }
            Node::Null => {
                return top;
            }
            _ => {
                unreachable!("must be statement {:?} \n", node);
            }
        }
    }

    fn gen_expr(&self, expr: &Node, top: usize) -> usize {
        match expr {
            Node::Number(i) => {
                print!("  mov {}, {}\n", REG[top], i);
                return top + 1;
            }
            Node::Variable(var) => {
                let top = self.gen_addr_var(var, top);
                return load(top);
            }
            Node::Assign(node) => {
                let top = self.gen_expr(&node.right, top);
                let top = self.gen_addr(&node.left, top);
                return store(top);
            }
            Node::Binary(node, op) => {
                let top = self.gen_expr(&node.left, top);
                let top = self.gen_expr(&node.right, top);

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

    fn gen_addr(&self, node: &Node, top: usize) -> usize {
        if let Node::Variable(var) = node {
            return self.gen_addr_var(var, top);
        }
        return top;
    }

    fn gen_addr_var(&self, var: &Rc<RefCell<Variable>>, top: usize) -> usize {
        let v = var.borrow();
        print!("  lea {}, [rbp-{}]\n", REG[top], v.offset);
        return top + 1;
    }
}

fn compute_offset(program: &mut Program) {
    let mut offset = 0;
    for local in program.locals.iter() {
        offset += 8;
        local.borrow_mut().offset = offset;
    }
    program.stack_size = offset;
}

fn load(top: usize) -> usize {
    print!("  mov {}, [{}]\n", REG[top - 1], REG[top - 1]);
    top
}

fn store(top: usize) -> usize {
    print!("  mov [{}], {}\n", REG[top - 1], REG[top - 2]);
    top - 1
}
