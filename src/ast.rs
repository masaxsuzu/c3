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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Node,
    pub right: Node,
}
