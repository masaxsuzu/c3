use crate::ast::{Binary, Node, Operator, Program, Unary};
use crate::error::Error;
use crate::token::Token;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Parser<'a> {
    tokens: Rc<Vec<Token<'a>>>,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(pos: usize, input: Rc<Vec<Token<'a>>>) -> Self {
        let p = Parser {
            tokens: input,
            pos: pos,
        };
        p
    }

    pub fn next(&self, n: usize) -> Self {
        Parser::new(self.pos + n, Rc::clone(&self.tokens))
    }

    pub fn nth(&self, n: usize) -> Token {
        let i = self.pos + n;
        if i >= self.tokens.len() {
            return Token::Eof;
        }
        return self.tokens[self.pos + n];
    }
}

pub fn parse(mut p: Parser) -> Result<Program, Error> {
    let mut nodes = Vec::<Node>::new();
    while p.nth(0) != Token::Eof {
        match stmt(p) {
            Ok((p1, n)) => {
                p = p1;
                nodes.push(n);
            }
            Err(e) => {
                return Err(e);
            }
        }
    }
    Ok(Program { nodes: nodes })
}

fn tag<'a>(p: Parser<'a>, s: &str) -> Result<(Parser<'a>, Node), Error> {
    let t = &p.nth(0);
    if t.is_reserved(s) {
        return Ok((p.next(1), Node::Null));
    }
    Err(Error::ParseError(format!("{:?}: not {}", p.nth(0), s)))
}

// stmt = expr ";"
fn stmt(p: Parser) -> Result<(Parser, Node), Error> {
    let (p, n) = if let Ok((p, _)) = tag(p.next(0), "return") {
        let (p, n) = expr(p)?;
        (p, Node::Return(Box::new(Unary { left: n })))
    } else {
        let (p, n) = expr(p)?;
        (p, Node::ExprStmt(Box::new(Unary { left: n })))
    };
    let (p, _) = tag(p, ";")?;
    Ok((p, n))
}

// expr = equality
fn expr(p: Parser) -> Result<(Parser, Node), Error> {
    equality(p)
}

// equality = relational ("==" relational | "!=" relational)*
fn equality(p: Parser) -> Result<(Parser, Node), Error> {
    let (mut p, mut left) = relational(p)?;
    loop {
        if let Ok((p1, _)) = tag(p.next(0), "==") {
            let (p1, right) = relational(p1)?;
            p = p1;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Eq);
            continue;
        }
        if let Ok((p2, _)) = tag(p.next(0), "!=") {
            let (p2, right) = relational(p2)?;
            p = p2;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Ne);
            continue;
        }
        return Ok((p.next(0), left));
    }
}

// relational = add ("<=" add | "<" add | ">=" add | ">"" add)*
fn relational(p: Parser) -> Result<(Parser, Node), Error> {
    let (mut p, mut left) = add(p)?;
    loop {
        if let Ok((p1, _)) = tag(p.next(0), "<=") {
            let (p1, right) = add(p1)?;
            p = p1;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Le);
            continue;
        }
        if let Ok((p2, _)) = tag(p.next(0), "<") {
            let (p2, right) = add(p2)?;
            p = p2;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Lt);
            continue;
        }
        if let Ok((p3, _)) = tag(p.next(0), ">=") {
            let (p3, right) = add(p3)?;
            p = p3;
            left = Node::Binary(
                Box::new(Binary {
                    right: left,
                    left: right,
                }),
                Operator::Le,
            );
            continue;
        }
        if let Ok((p4, _)) = tag(p.next(0), ">") {
            let (p4, right) = add(p4)?;
            p = p4;
            left = Node::Binary(
                Box::new(Binary {
                    right: left,
                    left: right,
                }),
                Operator::Lt,
            );
            continue;
        }
        return Ok((p.next(0), left));
    }
}

// add = mul ("+" mul | "-" mul)*
fn add(p: Parser) -> Result<(Parser, Node), Error> {
    let (mut p, mut left) = mul(p)?;
    loop {
        if let Ok((p1, _)) = tag(p.next(0), "+") {
            let (p1, right) = mul(p1)?;
            p = p1;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Add);
            continue;
        }
        if let Ok((p2, _)) = tag(p.next(0), "-") {
            let (p2, right) = mul(p2)?;
            p = p2;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Sub);
            continue;
        }
        return Ok((p.next(0), left));
    }
}

// mul = unary ("*" unary | "/" unary)*
fn mul(p: Parser) -> Result<(Parser, Node), Error> {
    let (mut p, mut left) = unary(p)?;
    loop {
        if let Ok((p1, _)) = tag(p.next(0), "*") {
            let (p1, right) = unary(p1)?;
            p = p1;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Mul);
            continue;
        }
        if let Ok((p2, _)) = tag(p.next(0), "/") {
            let (p2, right) = unary(p2)?;
            p = p2;
            left = Node::Binary(Box::new(Binary { left, right }), Operator::Div);
            continue;
        }
        return Ok((p.next(0), left));
    }
}

// unary = ("+" | "-") unary
//       | primary
fn unary(p: Parser) -> Result<(Parser, Node), Error> {
    if let Ok((p, _)) = tag(p.next(0), "+") {
        let (p, left) = unary(p)?;
        return Ok((
            p,
            Node::Binary(
                Box::new(Binary {
                    left,
                    right: Node::Number(0),
                }),
                Operator::Add,
            ),
        ));
    }
    if let Ok((p, _)) = tag(p.next(0), "-") {
        let (p, right) = unary(p)?;
        return Ok((
            p,
            Node::Binary(
                Box::new(Binary {
                    left: Node::Number(0),
                    right: right,
                }),
                Operator::Sub,
            ),
        ));
    }
    primary(p)
}

// primary = "(" expr ")" | num
fn primary(p: Parser) -> Result<(Parser, Node), Error> {
    if p.nth(0).is_reserved("(") {
        let (p, node) = expr(p)?;
        let (p, _) = tag(p, ";")?;
        return Ok((p, node));
    }
    num(p)
}

fn num(p: Parser) -> Result<(Parser, Node), Error> {
    if let Token::Number(i) = p.nth(0) {
        return Ok((p.next(1), Node::Number(i)));
    }
    Err(Error::ParseError(format!("{:?}: not number", p.nth(0))))
}
