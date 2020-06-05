#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Number(i64),
    Binary(Box<Binary>, Operator),
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
pub struct Binary {
    pub left: Node,
    pub right: Node,
}
