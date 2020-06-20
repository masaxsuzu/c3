use std::cell::RefCell;
use std::rc::Rc;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Int,
    Pointer(Box<Type>), // Pointer to X
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Program(Box<Function<'a>>, Token<'a>),
    Number(i64, Token<'a>, Type),
    Variable(Rc<RefCell<Variable>>, Token<'a>),
    ExprStmt(Box<Unary<'a>>, Token<'a>),
    BlockStmt(Box<Block<'a>>, Token<'a>),
    Return(Box<Unary<'a>>, Token<'a>),
    If(Box<If<'a>>, Token<'a>),
    Loop(Box<For<'a>>, Token<'a>),
    FuncCall(Box<FunctionCall<'a>>, Token<'a>, Type),
    Assign(Box<Binary<'a>>, Token<'a>, Type),
    Unary(Box<Unary<'a>>, Operator1, Token<'a>, Type),
    Binary(Box<Binary<'a>>, Operator2, Token<'a>, Type),
    Null(Token<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator1 {
    Addr,
    Deref,
    ExprStmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator2 {
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
pub struct Function<'a> { 
    pub stmt: Node<'a>,
    pub locals: Vec<Rc<RefCell<Variable>>>,
    pub stack_size: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: Type,
    pub name: String,
    pub offset: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall<'a> {
    pub name: String,
    pub args: Vec<Node<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub nodes: Vec<Node<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<'a> {
    pub cond: Node<'a>,
    pub then: Node<'a>,
    pub otherwise: Node<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct For<'a> {
    pub init: Node<'a>,
    pub cond: Node<'a>,
    pub inc: Node<'a>,
    pub then: Node<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'a> {
    pub left: Node<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary<'a> {
    pub left: Node<'a>,
    pub right: Node<'a>,
}
