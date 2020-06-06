use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Box<Program>),
    Number(i64),
    Variable(Rc<RefCell<Variable>>),
    Assign(Box<Binary>),
    ExprStmt(Box<Unary>),
    Return(Box<Unary>),
    Binary(Box<Binary>, Operator),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub nodes: Vec<Node>,
    pub locals: Vec<Rc<RefCell<Variable>>>,
    pub stack_size: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub offset: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub left: Node,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Node,
    pub right: Node,
}
