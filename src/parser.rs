use crate::ast::{
    Binary, Block, For, FunctionCall, If, Node, Operator1, Operator2, Program, Type, Unary,
    Variable,
};
use crate::error::Error;
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a> {
    tokens: Rc<Vec<Token<'a>>>,
    pos: usize,
    size: usize,
    eof: Token<'a>,
}

impl<'a> Tokens<'a> {
    pub fn new(size: usize, pos: usize, input: Rc<Vec<Token<'a>>>) -> Self {
        let p = Tokens {
            tokens: input,
            pos: pos,
            size: size,
            eof: Token::Eof(size),
        };
        p
    }

    ///
    /// Consume n tokens and create rest tokens.
    ///
    pub fn consume(&self, n: usize) -> Self {
        let p = Tokens::new(self.size, self.pos + n, Rc::clone(&self.tokens));
        p
    }

    ///
    /// Peek n th token.
    /// Get Eof if out of range.
    ///
    pub fn peek(&self, n: usize) -> &Token<'a> {
        let i = self.pos + n;
        if i >= self.tokens.len() {
            return &self.eof;
        }
        return &self.tokens[self.pos + n];
    }

    ///
    /// Take the given reserved token.
    ///
    pub fn take(&self, s: &str) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = self.peek(0).clone();
        if t.is_reserved(s) {
            return Ok((self.consume(1), Node::Null(t)));
        }
        Err(Error::ParseError(format!("not {}", s), t.clone()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub locals: Vec<Rc<RefCell<Variable>>>,
}

impl<'a> Parser {
    pub fn new() -> Self {
        Parser { locals: Vec::new() }
    }

    fn find_var(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        for var in self.locals.iter() {
            if var.borrow_mut().name == name {
                return Some(var.clone());
            }
        }
        None
    }

    ///
    /// Parse Nodes
    ///

    pub fn parse(&mut self, p: Tokens<'a>) -> Result<Program<'a>, Error<'a>> {
        let (_, n) = self.stmt(p)?;

        Ok(Program {
            stmt: n,
            locals: self.locals.clone(),
            stack_size: 0,
        })
    }

    // stmt = "return" expr ";"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "for" "(" expr? ";" expr? ";" expr ")" stmt
    //      | "while" "(" expr ")" stmt
    //      | "{" compound-stmt
    //      | expr ";"
    fn stmt(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        if let Ok((p, _)) = p.consume(0).take("return") {
            let t = p.peek(0).clone();
            let (p, n) = self.expr(p)?;
            let (p, _) = p.take(";")?;
            return Ok((p, Node::Return(Box::new(Unary { left: n }), t.clone())));
        }
        if let Ok((p, _)) = p.consume(0).take("if") {
            let t = p.peek(0).clone();
            let (p, _) = p.consume(0).take("(")?;
            let (p, cond) = self.expr(p)?;
            let (p, _) = p.consume(0).take(")")?;
            let (p, then) = self.stmt(p)?;
            let (p, otherwise) = if let Ok((p, _)) = p.consume(0).take("else") {
                self.stmt(p)?
            } else {
                (p, Node::Null(t.clone()))
            };
            return Ok((
                p,
                Node::If(
                    Box::new(If {
                        cond: cond,
                        then: then,
                        otherwise: otherwise,
                    }),
                    t.clone(),
                ),
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("for") {
            let (p, _) = p.consume(0).take("(")?;
            let (p, init) = match self.expr(p.consume(0)) {
                Ok((p, expr)) => (p, Node::ExprStmt(Box::new(Unary { left: expr }), t.clone())),
                _ => (p, Node::Null(t.clone())),
            };
            let (p, _) = p.consume(0).take(";")?;
            let (p, cond) = match self.expr(p.consume(0)) {
                Ok((p, expr)) => (p, expr),
                _ => (p, Node::Null(t.clone())),
            };
            let (p, _) = p.consume(0).take(";")?;
            let (p, inc) = match self.expr(p.consume(0)) {
                Ok((p, expr)) => (p, Node::ExprStmt(Box::new(Unary { left: expr }), t.clone())),
                _ => (p, Node::Null(t.clone())),
            };
            let (p, _) = p.consume(0).take(")")?;
            let (p, then) = self.stmt(p)?;

            return Ok((
                p,
                Node::Loop(
                    Box::new(For {
                        init,
                        cond,
                        inc,
                        then,
                    }),
                    t.clone(),
                ),
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("while") {
            let (p, _) = p.consume(0).take("(")?;
            let (p, cond) = self.expr(p)?;
            let (p, _) = p.consume(0).take(")")?;
            let (p, then) = self.stmt(p)?;
            return Ok((
                p,
                Node::Loop(
                    Box::new(For {
                        init: Node::Null(t.clone()),
                        cond: cond,
                        then: then,
                        inc: Node::Null(t.clone()),
                    }),
                    t.clone(),
                ),
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("{") {
            return self.compound_stmt(p);
        }

        let t = p.peek(0).clone();
        let (p, n) = self.expr(p)?;
        let (p, _) = p.take(";")?;
        Ok((p, Node::ExprStmt(Box::new(Unary { left: n }), t)))
    }

    // declaration = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let mut nodes = Vec::<Node<'a>>::new();
        let (mut p, ty) = self.typespec(p.consume(0))?;
        let t = p.consume(0).peek(0).clone();

        let mut after_second: Option<()> = None;
        loop {
            if let Ok((p1, _)) = p.consume(0).take(";") {
                return Ok((
                    p1,
                    Node::BlockStmt(Box::new(Block { nodes: nodes }), t.clone()),
                ));
            }

            if let Some(_) = after_second {
                let (p2, _) = p.consume(0).take(",")?;
                p = p2;
            } else {
                after_second = Some(());
            }

            let (p3, ty) = self.declarator(p.consume(0), ty.clone())?;
            let (var, name) = if let Token::Identifier(x, _) = p3.peek(0).clone() {
                let v = Rc::new(RefCell::new(Variable {
                    name: x.to_string(),
                    offset: 0,
                    ty: ty.clone(),
                }));
                (v, x)
            } else {
                return Err(Error::ParseError(
                    "Not identifier".to_string(),
                    p.peek(0).clone(),
                ));
            };
            p = p3.consume(1);
            self.locals.insert(0, var);

            if let Ok((p4, _)) = p.consume(0).take("=") {
                p = p4;
            } else {
                continue;
            }

            let var = self.find_var(name).unwrap();
            let left = Node::Variable(var, p.peek(0).clone());

            let (p5, right) = self.assign(p.consume(0))?;
            p = p5;

            let node = Node::Assign(Box::new(Binary { left, right }), t.clone(), ty.clone());
            nodes.push(Node::ExprStmt(Box::new(Unary { left: node }), t.clone()));
        }
    }

    // compound-stmt = stmt* "}"
    fn compound_stmt(&mut self, mut p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let mut nodes = Vec::new();
        let t = p.peek(0).clone();
        loop {
            if let Ok((p1, _)) = p.consume(0).take("}") {
                p = p1;
                break;
            }
            let (p2, n) = if let Ok((_, _)) = p.consume(0).take("int") {
                self.declaration(p)?
            } else {
                self.stmt(p.consume(0))?
            };
            p = p2;
            nodes.push(n);
        }

        Ok((p, Node::BlockStmt(Box::new(Block { nodes: nodes }), t)))
    }

    /// expr = assign
    fn expr(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        self.assign(p)
    }

    /// assign = equality ("=" assign)?
    fn assign(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        let (p, mut left) = self.equality(p)?;
        if let Ok((p1, _)) = p.consume(0).take("=") {
            let (p1, right) = self.assign(p1)?;
            let _ = self.get_type(left.clone())?;
            let tr = self.get_type(right.clone())?;

            if let Node::Variable(v, _) = left.clone() {
                let mut var = v.borrow_mut();
                if var.ty == Type::Unknown {
                    var.ty = tr.clone();
                }
            }

            left = Node::Assign(Box::new(Binary { left, right }), t, tr);
            return Ok((p1, left));
        }
        Ok((p, left))
    }

    /// equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        let (mut p, mut left) = self.relational(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("==") {
                let (p1, right) = self.relational(p1)?;
                p = p1;
                left = Self::new_binary_node(left, right, Operator2::Eq, t.clone(), Type::Int);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("!=") {
                let (p2, right) = self.relational(p2)?;
                p = p2;
                left = Self::new_binary_node(left, right, Operator2::Ne, t.clone(), Type::Int);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// relational = add ("<=" add | "<" add | ">=" add | ">"" add)*
    fn relational(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let (mut p, mut left) = self.add(p)?;
        loop {
            let t = p.peek(0).clone();
            if let Ok((p1, _)) = p.consume(0).take("<=") {
                let (p1, right) = self.add(p1)?;
                p = p1;
                left = Self::new_binary_node(left, right, Operator2::Le, t.clone(), Type::Int);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("<") {
                let (p2, right) = self.add(p2)?;
                p = p2;
                left = Self::new_binary_node(left, right, Operator2::Lt, t.clone(), Type::Int);
                continue;
            }
            if let Ok((p3, _)) = p.consume(0).take(">=") {
                let (p3, right) = self.add(p3)?;
                p = p3;
                left = Self::new_binary_node(right, left, Operator2::Le, t.clone(), Type::Int);
                continue;
            }
            if let Ok((p4, _)) = p.consume(0).take(">") {
                let t = p.peek(0).clone();
                let (p4, right) = self.add(p4)?;
                p = p4;
                left = Self::new_binary_node(right, left, Operator2::Lt, t.clone(), Type::Int);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let (mut p, mut left) = self.mul(p)?;
        loop {
            let t = p.peek(0).clone();
            if let Ok((p1, _)) = p.consume(0).take("+") {
                let (p1, right) = self.mul(p1)?;
                p = p1;
                left = self.new_add_node(left, right, t.clone())?;
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("-") {
                let (p2, right) = self.mul(p2)?;
                p = p2;
                left = self.new_sub_node(left, right, t.clone())?;
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// mul = unary ("*" unary | "/" unary)*
    fn mul(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        let (mut p, mut left) = self.unary(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("*") {
                let (p1, right) = self.unary(p1)?;
                p = p1;
                left = Self::new_binary_node(left, right, Operator2::Mul, t.clone(), Type::Int);
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("/") {
                let (p2, right) = self.unary(p2)?;
                p = p2;
                left = Self::new_binary_node(left, right, Operator2::Div, t.clone(), Type::Int);
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// unary = ("+" | "-" | "*" | "&") unary
    ///       | primary
    fn unary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0);
        if let Ok((p, _)) = p.consume(0).take("+") {
            let (p, left) = self.unary(p)?;
            return Ok((
                p,
                self.new_add_node(left, Node::Number(0, t.clone(), Type::Int), t.clone())?,
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("-") {
            let (p, right) = self.unary(p)?;
            return Ok((
                p,
                self.new_sub_node(Node::Number(0, t.clone(), Type::Int), right, t.clone())?,
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("&") {
            let (p, left) = self.unary(p)?;
            let ty = self.get_type(left.clone())?;
            return Ok((
                p,
                Self::new_unary_node(
                    left,
                    Operator1::Addr,
                    t.clone(),
                    Type::Pointer(Box::new(ty)),
                ),
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("*") {
            let t = p.peek(0).clone();
            let (p, left) = self.unary(p)?;
            let base = self.get_base_type(left.clone(), t.clone())?;
            return Ok((
                p,
                Self::new_unary_node(left, Operator1::Deref, t.clone(), base),
            ));
        }
        self.primary(p)
    }

    /// funcall = ident "(" (assign ("," assign)*)? ")"
    fn funccall(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        let x = if let Token::Identifier(x, _) = p.peek(0) {
            *x
        } else {
            return Err(Error::ParseError("not identifier".to_owned(),t));
        };

        let mut args = Vec::<Node>::new();

        let (mut p, _) = p.consume(1).take("(")?;

        if let Ok((mut p1, node)) = self.assign(p.consume(0)) {
            args.push(node);
            loop {
                if let Ok((p2, _)) = p1.consume(0).take(",") {
                    let (p2, node) = self.assign(p2)?;
                    args.push(node);
                    p1 = p2;
                } else {
                    break;
                }
            }
            p = p1;    
        }
        let call = Node::FuncCall(
            Box::new(FunctionCall {
                name: x.to_string(),
                args: args,
            }),
            t.clone(),
            Type::Int,
        );
        let (p, _) = p.take(")")?;
        return Ok((p, call));
    }

    /// primary = "(" expr ")" | ident args? | num
    /// args = "(" ")"
    fn primary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        if let Ok((p, _)) = p.consume(0).take("(") {
            let (p, node) = self.expr(p)?;
            let (p, _) = p.take(")")?;
            return Ok((p, node));
        }
        if let Token::Identifier(x, _) = p.peek(0) {
            if let Ok((_, _)) = p.consume(1).take("(") {
                return self.funccall(p);
            }
            let var = if let Some(v) = self.find_var(x) {
                v
            } else {
                return Err(Error::ParseError(
                    "undefined variable".to_string(),
                    p.peek(0).clone(),
                ));
            };
            return Ok((p.consume(1), Node::Variable(var, t)));
        }
        self.num(p)
    }

    fn num(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        if let Token::Number(i, _) = p.peek(0) {
            return Ok((p.consume(1), Node::Number(*i, t, Type::Int)));
        }
        Err(Error::ParseError(format!("not number"), t.clone()))
    }

    ///
    /// Parse type
    ///

    // typespec = "int"
    fn typespec(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Type), Error<'a>> {
        let (p, _) = p.consume(0).take("int")?;
        Ok((p, Type::Int))
    }

    // declarator = "*"* ident
    fn declarator(&self, mut p: Tokens<'a>, ty: Type) -> Result<(Tokens<'a>, Type), Error<'a>> {
        let mut t = ty;
        while let Ok((p1, _)) = p.consume(0).take("*") {
            t = Type::Pointer(Box::new(t));
            p = p1;
        }
        if let Token::Identifier(_, _) = p.peek(0) {
            return Ok((p.consume(0), t));
        }
        Err(Error::ParseError(
            "Not identifier".to_string(),
            p.peek(0).clone(),
        ))
    }

    ///
    /// Helper
    ///
    fn new_unary_node(left: Node<'a>, op: Operator1, t: Token<'a>, ty: Type) -> Node<'a> {
        Node::Unary(Box::new(Unary { left: left }), op, t, ty)
    }

    fn new_binary_node(
        left: Node<'a>,
        right: Node<'a>,
        op: Operator2,
        t: Token<'a>,
        ty: Type,
    ) -> Node<'a> {
        Node::Binary(
            Box::new(Binary {
                left: left,
                right: right,
            }),
            op,
            t,
            ty,
        )
    }

    fn new_add_node(
        &self,
        left: Node<'a>,
        right: Node<'a>,
        t: Token<'a>,
    ) -> Result<Node<'a>, Error<'a>> {
        let tl = self.get_type(left.clone())?;
        let tr = self.get_type(right.clone())?;

        match (tl, tr) {
            (Type::Int, Type::Int) => Ok(Self::new_binary_node(
                left,
                right,
                Operator2::Add,
                t,
                Type::Int,
            )),
            (Type::Int, Type::Pointer(_)) => Ok(Self::new_binary_node(
                Self::new_binary_node(
                    left,
                    Node::Number(8, t.clone(), Type::Int),
                    Operator2::Mul,
                    t.clone(),
                    Type::Int,
                ),
                right,
                Operator2::Add,
                t,
                Type::Pointer(Box::new(Type::Int)),
            )),
            (Type::Pointer(_), Type::Int) => Ok(Self::new_binary_node(
                left,
                Self::new_binary_node(
                    right,
                    Node::Number(8, t.clone(), Type::Int),
                    Operator2::Mul,
                    t.clone(),
                    Type::Int,
                ),
                Operator2::Add,
                t,
                Type::Pointer(Box::new(Type::Int)),
            )),
            _ => Err(Error::ParseError("invalid operand".to_string(), t)),
        }
    }

    fn new_sub_node(
        &self,
        left: Node<'a>,
        right: Node<'a>,
        t: Token<'a>,
    ) -> Result<Node<'a>, Error<'a>> {
        let tl = self.get_type(left.clone())?;
        let tr = self.get_type(right.clone())?;

        match (tl, tr) {
            (Type::Int, Type::Int) => Ok(Self::new_binary_node(
                left,
                right,
                Operator2::Sub,
                t,
                Type::Int,
            )),
            (Type::Pointer(_), Type::Int) => Ok(Self::new_binary_node(
                left,
                Self::new_binary_node(
                    right,
                    Node::Number(8, t.clone(), Type::Int),
                    Operator2::Mul,
                    t.clone(),
                    Type::Int,
                ),
                Operator2::Sub,
                t,
                Type::Pointer(Box::new(Type::Int)),
            )),
            (Type::Pointer(tl), Type::Pointer(_)) => {
                let node = Self::new_binary_node(
                    left,
                    right,
                    Operator2::Sub,
                    t.clone(),
                    Type::Pointer(tl.clone()),
                );
                let node = Self::new_binary_node(
                    node,
                    Node::Number(8, t.clone(), Type::Int),
                    Operator2::Div,
                    t.clone(),
                    Type::Pointer(tl.clone()),
                );
                Ok(node)
            }
            _ => Err(Error::ParseError("invalid operand".to_string(), t)),
        }
    }

    fn get_type(&self, node: Node<'a>) -> Result<Type, Error<'a>> {
        match node {
            Node::Variable(v, _) => {
                let t = &v.borrow().ty;
                Ok(t.clone())
            }
            Node::Assign(_, _, ty) => Ok(ty),
            Node::Number(_, _, ty) => Ok(ty),
            Node::Unary(_, _, _, ty) => Ok(ty),
            Node::Binary(_, _, _, ty) => Ok(ty),
            Node::FuncCall(_, _, ty) => Ok(ty),
            _ => unreachable!("{:?}", node),
        }
    }

    fn get_base_type(&self, node: Node<'a>, t: Token<'a>) -> Result<Type, Error<'a>> {
        let ty = self.get_type(node)?;
        match ty {
            Type::Pointer(to) => Ok(*to),
            _ => Err(Error::ParseError("Not pointer".to_string(), t)),
        }
    }
}
