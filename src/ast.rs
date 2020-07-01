use std::cell::RefCell;
use std::rc::Rc;

use crate::error::Error;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unknown,
    Integer(i64),              // size i
    Pointer(Box<PointerType>), // Pointer to X
    Array(Box<ArrayType>),     // Array of X
    Function(Box<FunctionType>),
    Param(Box<ParameterType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Program(Box<Program<'a>>, Token<'a>),
    Function(Box<Function<'a>>, Token<'a>),
    Number(i64, Token<'a>, Type),
    Str(String, Token<'a>, Type),
    Variable(Rc<RefCell<Variable>>, Token<'a>),
    ExprStmt(Box<Unary<'a>>, Token<'a>),
    BlockStmt(Box<Block<'a>>, Token<'a>),
    StmtExpr(Box<StmtExpr<'a>>, Token<'a>),
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
    Comma,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub globals: Vec<Rc<RefCell<Variable>>>,
    pub functions: Vec<Rc<RefCell<Function<'a>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PointerType {
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub ty: Type,
    pub len: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub return_ty: Type,
    pub name: String,
    pub params: Vec<ParameterType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterType {
    pub ty: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'a> {
    pub ty: Type,
    pub name: String,
    pub stmt: Node<'a>,
    pub params: Vec<Rc<RefCell<Variable>>>,
    pub locals: Vec<Rc<RefCell<Variable>>>,
    pub stack_size: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ty: Type,
    pub name: String,
    pub offset: i64,
    pub is_local: bool,
    pub init_data: Option<String>,
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
pub struct StmtExpr<'a> {
    pub ty: Type,
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

pub fn get_type<'a>(node: &Node<'a>) -> Result<Type, Error<'a>> {
    match node {
        Node::Variable(v, _) => {
            let t = &v.borrow().ty;
            Ok(t.clone())
        }
        Node::StmtExpr(stmt, _) => {
            let t = &stmt.ty;
            Ok(t.clone())
        }
        Node::Function(f, _) => {
            let t = &f.ty;
            Ok(t.clone())
        }
        Node::Assign(_, _, ty) => Ok(ty.clone()),
        Node::Number(_, _, ty) => Ok(ty.clone()),
        Node::Unary(_, _, _, ty) => Ok(ty.clone()),
        Node::Binary(_, _, _, ty) => Ok(ty.clone()),
        Node::FuncCall(_, _, ty) => Ok(ty.clone()),
        _ => unreachable!("{:?}", node),
    }
}

pub fn get_base_type<'a>(node: Node<'a>, t: Token<'a>) -> Result<Type, Error<'a>> {
    let ty = get_type(&node)?;
    match ty {
        Type::Pointer(to) => Ok(to.ty),
        Type::Array(of) => Ok(of.ty),
        _ => Err(Error::ParseError(format!("Not pointer {:?}", ty), t)),
    }
}

pub fn size_of(ty: Type) -> i64 {
    match ty {
        Type::Integer(size) => size,
        Type::Pointer(_) => 8,
        Type::Array(of) => of.len * size_of(of.ty),
        Type::Param(p) => size_of(p.ty),
        _ => 0,
    }
}
