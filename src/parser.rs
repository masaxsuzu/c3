use crate::ast::{Binary, Node, Operator, Program, Unary, Variable};
use crate::error::Error;
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a> {
    tokens: Rc<Vec<Token<'a>>>,
    pos: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(pos: usize, input: Rc<Vec<Token<'a>>>) -> Self {
        let p = Tokens {
            tokens: input,
            pos: pos,
        };
        p
    }

    ///
    /// Consume n tokens and create rest tokens. 
    /// 
    pub fn consume(&self, n: usize) -> Self {
        let p = Tokens::new(self.pos + n, Rc::clone(&self.tokens));
        p
    }

    ///
    /// Peek n th token.
    /// Get Eof if out of range.
    /// 
    pub fn peek(&self, n: usize) -> &Token {
        let i = self.pos + n;
        if i >= self.tokens.len() {
            return &Token::Eof;
        }
        return &self.tokens[self.pos + n];
    }

    ///
    /// Take the given reserved token.
    /// 
    pub fn take(&self, s: &str) -> Result<(Tokens<'a>, Node), Error> {
        let t = self.peek(0);
        if t.is_reserved(s) {
            return Ok((self.consume(1), Node::Null));
        }
        Err(Error::ParseError(format!("{:?}: not {}", self.peek(0), s)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub locals: Vec<Rc<RefCell<Variable>>>,
}

impl <'a> Parser {
    pub fn new() -> Self {
        Parser {
            locals: Vec::new(),
        }
    }

    fn find_var(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        for var in self.locals.iter() {
            if var.borrow_mut().name == name { 
                return Some(var.clone());
            }
        }
        None
    }

    pub fn parse(&mut self, mut p: Tokens<'a>) -> Result<Program, Error> {
        let mut nodes = Vec::<Node>::new();
        while p.peek(0) != &Token::Eof {
            match self.stmt(p) {
                Ok((p1, n)) => {
                    p = p1;
                    nodes.push(n);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(Program { nodes: nodes, locals: self.locals.clone(), stack_size:0 })
    }

    /// stmt = expr ";"
    fn stmt(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (p, n) = if let Ok((p, _)) = p.consume(0).take("return") {
            let (p, n) = self.expr(p)?;
            (p, Node::Return(Box::new(Unary { left: n })))
        } else {
            let (p, n) = self.expr(p)?;
            (p, Node::ExprStmt(Box::new(Unary { left: n })))
        };
        let (p, _) = p.take(";")?;
        Ok((p, n))
    }

    /// expr = assign
    fn expr(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        self.assign(p)
    }

    /// assign = equality ("=" assign)?
    fn assign(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (p, mut left) = self.equality(p)?;
        if let Ok((p1, _)) = p.consume(0).take("=") {
            let (p1, right) = self.assign(p1)?;
            left = Node::Assign(Box::new(Binary { left, right }));
            return Ok((p1, left));
        }
        Ok((p, left))
    }

    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (mut p, mut left) = self.relational(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("==") {
                let (p1, right) = self.relational(p1)?;
                p = p1;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Eq);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("!=") {
                let (p2, right) = self.relational(p2)?;
                p = p2;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Ne);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// relational = add ("<=" add | "<" add | ">=" add | ">"" add)*
    fn relational(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (mut p, mut left) = self.add(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("<=") {
                let (p1, right) = self.add(p1)?;
                p = p1;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Le);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("<") {
                let (p2, right) = self.add(p2)?;
                p = p2;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Lt);
                continue;
            }
            if let Ok((p3, _)) = p.consume(0).take(">=") {
                let (p3, right) = self.add(p3)?;
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
            if let Ok((p4, _)) = p.consume(0).take(">") {
                let (p4, right) = self.add(p4)?;
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
            return Ok((p.consume(0), left));
        }
    }

    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (mut p, mut left) = self.mul(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("+") {
                let (p1, right) = self.mul(p1)?;
                p = p1;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Add);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("-") {
                let (p2, right) = self.mul(p2)?;
                p = p2;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Sub);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        let (mut p, mut left) = self.unary(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("*") {
                let (p1, right) = self.unary(p1)?;
                p = p1;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Mul);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("/") {
                let (p2, right) = self.unary(p2)?;
                p = p2;
                left = Node::Binary(Box::new(Binary { left, right }), Operator::Div);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// unary = ("+" | "-") unary
    ///       | primary
    fn unary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        if let Ok((p, _)) = p.consume(0).take("+") {
            let (p, left) = self.unary(p)?;
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
        if let Ok((p, _)) = p.consume(0).take("-") {
            let (p, right) = self.unary(p)?;
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
        self.primary(p)
    }

    /// primary = "(" expr ")" | ident | num
    fn primary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        if let Ok((p, _)) = p.consume(0).take("(") {
            let (p, node) = self.expr(p)?;
            let (p, _) = p.take(";")?;
            return Ok((p, node));
        }
        if let Token::Identifier(x) = p.peek(0) {
            let var = if let Some(v) = self.find_var(x) {
                v
            } else {
                let v = Rc::new(RefCell::new(Variable { name: x.to_string(), offset:0}));
                self.locals.push(v);
                self.find_var(x).unwrap()
            };
            return Ok((p.consume(1), Node::Variable(var)));
        }
        self.num(p)
    }

    fn num(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node), Error> {
        if let Token::Number(i) = p.peek(0) {
            return Ok((p.consume(1), Node::Number(*i)));
        }
        Err(Error::ParseError(format!("{:?}: not number", p.peek(0))))
    }
}