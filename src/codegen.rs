use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    get_type, size_of, Function, Node, Operator1, Operator2, Program, Type, Variable,
};
use crate::token::Token;

const REG64: [&'static str; 6] = ["r10", "r11", "r12", "r13", "r14", "r15"];
const ARGREG08: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const ARGREG64: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

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
        #[cfg(debug_assertions)]
        eprintln!("{:?}\n", prog);

        print!(".intel_syntax noprefix\n");

        &self.emit_data(&prog);
        &self.emit_text(&prog);
    }

    fn emit_data(&mut self, prog: &Program) {
        print!(".data\n");
        for global in prog.globals.iter().as_ref() {
            let g = global.borrow();
            print!("{}:\n", g.clone().name);
            if let Some(x) = &g.init_data {
                for e in x.chars() {
                    print!("  .byte {}\n", e as u32);
                }
            } else {
                print!("  .zero {}\n", size_of(g.clone().ty));
            }
        }
    }

    fn emit_text(&mut self, prog: &Program) {
        print!(".text\n");
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

            // Save arguments to the stack
            let mut i = function.params.len();
            for var in function.params.iter() {
                i = i - 1;
                match size_of(var.borrow().ty.clone()) {
                    1 => {
                        print!("  mov [rbp-{}], {}\n", var.borrow().offset, ARGREG08[i]);
                    }
                    _ => {
                        print!("  mov [rbp-{}], {}\n", var.borrow().offset, ARGREG64[i]);
                    }
                }
            }

            let top = self.gen_stmt(&function.stmt, 0, f);

            assert_eq!(0, top);

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
                let top = self.gen_expr(&stmt.left, top, function) - 1;
                return top;
            }
            Node::Return(expr, _) => {
                let top = self.gen_expr(&expr.left, top, function) - 1;
                print!("  mov rax, {}\n", REG64[top]);
                print!("  jmp .L.return.{}\n", function.borrow().name);
                return top;
            }
            Node::If(_if, _) => {
                self.label_seq += 1;
                let seq = self.label_seq;

                let top = if !is_null(&_if.otherwise) {
                    let top = self.gen_expr(&_if.cond, top, function) - 1;
                    print!("  cmp {}, 0\n", REG64[top]);
                    print!("  je  .L.else.{}\n", seq);
                    let top = self.gen_stmt(&_if.then, top, function);
                    print!("  jmp .L.end.{}\n", seq);
                    print!(".L.else.{}:\n", seq);
                    let top = self.gen_stmt(&_if.otherwise, top, function);
                    print!(".L.end.{}:\n", seq);
                    top
                } else {
                    let top = self.gen_expr(&_if.cond, top, function) - 1;
                    print!("  cmp {}, 0\n", REG64[top]);
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
                    let top = self.gen_expr(&_for.cond, top, function) - 1;
                    print!("  cmp {}, 0\n", REG64[top]);
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

    fn gen_expr(&mut self, expr: &Node, top: usize, function: &Rc<RefCell<Function>>) -> usize {
        match expr {
            Node::Number(i, _, _) => {
                print!("  mov {}, {}\n", REG64[top], i);
                return top + 1;
            }
            Node::Variable(var, _) => {
                let top = self.gen_addr_var(var, top);
                let ty = &var.borrow().ty;
                return load(top, &ty);
            }
            Node::FuncCall(call, _, _) => {
                let mut n = 0;
                let mut t = top;

                for arg in call.args.iter() {
                    t = self.gen_expr(arg, t, function);
                    n += 1;
                }

                for i in 1..n + 1 {
                    t -= 1;
                    print!("  mov {}, {}\n", ARGREG64[n - i], REG64[t]);
                }

                print!("  push r10\n");
                print!("  push r11\n");
                print!("  mov rax, 0\n");
                print!("  call {}\n", call.name);
                print!("  pop r11\n");
                print!("  pop r10\n");
                print!("  mov {}, rax\n", REG64[t]);
                return t + 1;
            }
            Node::StmtExpr(stmt, _) => {
                let mut t = top;
                for node in stmt.nodes.iter() {
                    t = self.gen_stmt(node, t, function);
                }
                return t + 1;
            }
            Node::Assign(node, _, ty) => {
                let top = self.gen_expr(&node.right, top, function);
                let top = self.gen_addr(&node.left, top, function);
                return store(top, &ty);
            }
            Node::Unary(node, Operator1::Deref, _, base) => {
                let top = self.gen_expr(&node.left, top, function);
                let top = load(top, &base);
                top
            }
            Node::Unary(node, Operator1::Addr, _, _) => {
                let top = self.gen_addr(&node.left, top, function);
                top
            }
            Node::Binary(node, Operator2::Comma, _, _) => {
                let top = self.gen_expr(&node.left, top, function) - 1;
                let top = self.gen_expr(&node.right, top, function);
                top
            }
            Node::Binary(node, op, _, _) => {
                let top = self.gen_expr(&node.left, top, function);
                let top = self.gen_expr(&node.right, top, function);

                let rd = REG64[top - 2];
                let rs = REG64[top - 1];
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
                        print!("  movzx {}, al\n", rd);
                    }
                    Operator2::Ne => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  setne al\n");
                        print!("  movzx {}, al\n", rd);
                    }
                    Operator2::Lt => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  setl al\n");
                        print!("  movzx {}, al\n", rd);
                    }
                    Operator2::Le => {
                        print!("  cmp {}, {}\n", rd, rs);
                        print!("  setle al\n");
                        print!("  movzx {}, al\n", rd);
                    }
                    _ => {

                    }
                }
                return top - 1;
            }
            _ => top,
        }
    }

    fn gen_addr(&mut self, node: &Node, top: usize, function: &Rc<RefCell<Function>>) -> usize {
        if let Node::Variable(var, _) = node {
            return self.gen_addr_var(var, top);
        }
        if let Node::Unary(deref, Operator1::Deref, _, _) = node {
            return self.gen_expr(&deref.left, top, function);
        }
        if let Node::Binary(bin, Operator2::Comma, _, _) = node {
            let top = self.gen_expr(&bin.left, top, function) - 1;
            return self.gen_addr(&bin.right, top, function);
        }
        unreachable!("{:?}", node);
    }

    fn gen_addr_var(&self, var: &Rc<RefCell<Variable>>, top: usize) -> usize {
        let v = var.borrow();
        if v.is_local {
            print!("  lea {}, [rbp-{}]\n", REG64[top], v.offset);
        } else {
            print!("  mov {}, offset {}\n", REG64[top], v.name);
        }
        return top + 1;
    }
}

fn compute_offset(func: &mut Function) {
    let mut offset = 32;
    for local in func.locals.iter() {
        let v = Node::Variable(local.clone(), Token::Eof(0));
        let ty = get_type(&v).ok().unwrap();
        offset += size_of(ty);
        local.borrow_mut().offset = offset;
    }
    func.stack_size = align_to(offset, 16);
}

fn align_to(n: i64, align: i64) -> i64 {
    return (n + align - 1) & !(align - 1);
}

fn load(top: usize, ty: &Type) -> usize {
    if let Type::Array(_) = ty {
        // Do no thing for Array
        // In C, array is regarded as a pointer implicitly.
        return top;
    }
    if size_of(ty.clone()) == 1 {
        print!(
            "  movsx {}, byte ptr [{}]\n",
            REG64[top - 1],
            REG64[top - 1]
        );
    } else {
        print!("  mov {}, [{}]\n", REG64[top - 1], REG64[top - 1]);
    }
    top
}

fn store(top: usize, ty: &Type) -> usize {
    match size_of(ty.clone()) {
        1 => {
            print!("  mov [{}], {}\n", REG64[top - 1], REG64[top - 2]);
        }
        _ => {
            print!("  mov [{}], {}\n", REG64[top - 1], REG64[top - 2]);
        }
    }
    top - 1
}

fn is_null(node: &Node) -> bool {
    match node {
        Node::Null(_) => true,
        _ => false,
    }
}
