#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Box<Program>),
    Number(i64),
    ExprStmt(Box<Unary>),
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
