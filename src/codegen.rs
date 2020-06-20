use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{Node, Operator1, Operator2, Program, Function, Variable};

const REG: [&'static str; 6] = ["r10", "r11", "r12", "r13", "r14", "r15"];
const ARGREG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

pub struct CodeGenerator {
    label_seq: i64,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { label_seq: 0 }
    }

    pub fn gen(&mut self, mut prog: Program) {
        for function in prog.functions.iter_mut() {
            compute_offset(&mut function.borrow_mut());
        }
        // eprintln!("{:?}\n",program);

        print!(".intel_syntax noprefix\n");

        for f in prog.functions.iter().as_ref() {
            let function = f.borrow();

            print!(".globl {}\n", function.name);
            print!("{}:\n", function.name);

            // Prologue. r12-15 are callee-saved registers.
            print!("  push rbp\n");
            print!("  mov rbp, rsp\n");
            print!("  sub rsp, {}\n", function.stack_size);
            print!("  mov [rbp-8], r12\n");
            print!("  mov [rbp-16], r13\n");
            print!("  mov [rbp-24], r14\n");
            print!("  mov [rbp-32], r15\n");

            #[cfg(debug_assertions)]
            eprintln!("{:?}", function.stmt);

            self.gen_stmt(&function.stmt, 0, f);

            // Epilogue
            print!(".L.return.{}:\n", function.name);
            print!("  mov r12, [rbp-8]\n");
            print!("  mov r13, [rbp-16]\n");
            print!("  mov r14, [rbp-24]\n");
            print!("  mov r15, [rbp-32]\n");
            print!("  mov rsp, rbp\n");
            print!("  pop rbp\n");

            print!("  ret\n");
        }
    }

    fn gen_stmt(&mut self, node: &Node, mut top: usize, function: &Rc<RefCell<Function>>) -> usize {
        match node {
            Node::BlockStmt(block, _) => {
                for stmt in block.nodes.iter() {
                    top = self.gen_stmt(stmt, top, function);
                }
                return top;
            }
            Node::ExprStmt(stmt, _) => {
                let top = self.gen_expr(&stmt.left, top) - 1;
                return top;
            }
            Node::Return(expr, _) => {
                let top = self.gen_expr(&expr.left, top) - 1;
                print!("  mov rax, {}\n", REG[top]);
                print!("  jmp .L.return.{}\n", function.borrow().name);
                return top;
            }
            Node::If(_if, _) => {
                self.label_seq += 1;
                let seq = self.label_seq;

                let top = if !is_null(&_if.otherwise) {
                    let top = self.gen_expr(&_if.cond, top) - 1;
                    print!("  cmp %s, {}\n", REG[top]);
                    print!("  je  .L.else.{}\n", seq);
                    let top = self.gen_stmt(&_if.then, top, function);
                    print!("  jmp .L.end.{}\n", seq);
                    print!(".L.else.{}:\n", seq);
                    let top = self.gen_stmt(&_if.otherwise, top, function);
                    print!(".L.end.{}:\n", seq);
                    top
                } else {
                    let top = self.gen_expr(&_if.cond, top) - 1;
                    print!("  cmp {}, 0\n", REG[top]);
                    print!("  je  .L.end.{}\n", seq);
                    let top = self.gen_stmt(&_if.then, top, function);
                    print!(".L.end.{}:\n", seq);
                    top
                };
                return top;
            }
            Node::Loop(_for, _) => {
                self.label_seq += 1;
                let seq = self.label_seq;

                let top = if !is_null(&_for.init) {
                    self.gen_stmt(&_for.init, top, function)
                } else {
                    top
                };

                print!(".L.begin.{}:\n", seq);

                let top = if !is_null(&_for.cond) {
                    let top = self.gen_expr(&_for.cond, top) - 1;
                    print!("  cmp {}, 0\n", REG[top]);
                    print!("  je  .L.end.{}\n", seq);
                    top
                } else {
                    top
                };

                let top = self.gen_stmt(&_for.then, top, function);

                let top = if !is_null(&_for.inc) {
                    self.gen_stmt(&_for.inc, top, function)
                } else {
                    top
                };

                print!("  jmp  .L.begin.{}\n", seq);
                print!(".L.end.{}:\n", seq);

                return top;
            }
            Node::Null(_) => {
                return top;
            }
            _ => {
                unreachable!("must be statement {:?} \n", node);
            }
        }
    }

    fn gen_expr(&self, expr: &Node, top: usize) -> usize {
        match expr {
            Node::Number(i, _, _) => {
                print!("  mov {}, {}\n", REG[top], i);
                return top + 1;
            }
            Node::Variable(var, _) => {
                let top = self.gen_addr_var(var, top);
                return load(top);
            }
            Node::FuncCall(call, _, _) => {
                let mut n = 0;
                let mut t = top;
                
                for arg in call.args.iter() {
                    t = self.gen_expr(arg, t);
                    n += 1;
                }

                for i in 1..n+1 {
                    t -= 1;
                    print!("  mov {}, {}\n", ARGREG[n - i], REG[t]);
                }

                print!("  push r10\n");
                print!("  push r11\n");
                print!("  mov rax, 0\n");
                print!("  push r11\n");
                print!("  push r10\n");
                print!("  call {}\n", call.name);
                print!("  mov {}, rax\n", REG[t]);
                return t + 1;
            }
            Node::Assign(node, _, _) => {
                let top = self.gen_expr(&node.right, top);
                let top = self.gen_addr(&node.left, top);
                return store(top);
            }
            Node::Unary(node, Operator1::Deref, _, _) => {
                let top = self.gen_expr(&node.left, top);
                let top = load(top);
                top
            }
            Node::Unary(node, Operator1::Addr, _, _) => {
                let top = self.gen_addr(&node.left, top);
                top
            }
            Node::Binary(node, op, _, _) => {
                let top = self.gen_expr(&node.left, top);
                let top = self.gen_expr(&node.right, top);

                let rd = REG[top - 2];
                let rs = REG[top - 1];
                match op {
                    Operator2::Add => print!("  add {}, {}\n", rd, rs),
                    Operator2::Sub => print!("  sub {}, {}\n", rd, rs),
                    Operator2::Mul => print!("  imul {}, {}\n", rd, rs),
                    Operator2::Div => {
                        print!("  mov rax, {}\n", rd);
                        print!("  cqo\n");
                        print!("  idiv {}\n", rs);
                        print!("  mov {}, rax\n", rd);
                    }
                    Operator2::Eq => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  sete al\n");
                        print!("  movzb {}, al\n", rd);
                    }
                    Operator2::Ne => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  setne al\n");
                        print!("  movzb {}, al\n", rd);
                    }
                    Operator2::Lt => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  setl al\n");
                        print!("  movzb {}, al\n", rd);
                    }
                    Operator2::Le => {
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
        if let Node::Variable(var, _) = node {
            return self.gen_addr_var(var, top);
        }
        if let Node::Unary(deref, Operator1::Deref, _, _) = node {
            return self.gen_expr(&deref.left, top);
        }
        unreachable!("{:?}", node);
    }

    fn gen_addr_var(&self, var: &Rc<RefCell<Variable>>, top: usize) -> usize {
        let v = var.borrow();
        print!("  lea {}, [rbp-{}]\n", REG[top], v.offset);
        return top + 1;
    }
}

fn compute_offset(func: &mut Function) {
    let mut offset = 32;
    for local in func.locals.iter() {
        offset += 8;
        local.borrow_mut().offset = offset;
    }
    func.stack_size = align_to(offset, 16);
}

fn align_to(n: i64, align: i64) -> i64 {
    return (n + align - 1) & !(align - 1);
}

fn load(top: usize) -> usize {
    print!("  mov {}, [{}]\n", REG[top - 1], REG[top - 1]);
    top
}

fn store(top: usize) -> usize {
    print!("  mov [{}], {}\n", REG[top - 1], REG[top - 2]);
    top - 1
}

fn is_null(node: &Node) -> bool {
    match node {
        Node::Null(_) => true,
        _ => false,
    }
}
