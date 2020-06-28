use crate::ast::{
    get_base_type, get_type, size_of, ArrayType, Binary, Block, For, Function, FunctionCall,
    FunctionType, If, Node, Operator1, Operator2, ParameterType, PointerType, Program, StmtExpr,
    Type, Unary, Variable,
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
    pub globals: Vec<Rc<RefCell<Variable>>>,
    pub var_scopes: Vec<Vec<Rc<RefCell<Variable>>>>,
}

impl<'a> Parser {
    pub fn new() -> Self {
        Parser {
            locals: Vec::new(),
            globals: Vec::new(),
            var_scopes: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.var_scopes.push(Default::default());
    }

    fn leave_scope(&mut self) {
        self.var_scopes.pop();
    }

    fn find_var(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        self.var_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .flat_map(|v| {
                if v.borrow().name == name {
                    Some(v.clone())
                } else {
                    None
                }
            })
            .next()
    }

    ///
    /// Parse Nodes
    ///

    // parse = func*
    pub fn parse(&mut self, mut p: Tokens<'a>) -> Result<Program<'a>, Error<'a>> {
        let mut functions: Vec<Rc<RefCell<Function>>> = vec![];

        self.enter_scope();
        loop {
            if let Token::Eof(_) = p.peek(0) {
                break;
            }
            self.locals = vec![];

            let (p1, ty) = self.typespec(p.consume(0))?;
            let (mut p1, (mut ty, mut name)) = self.declarator(p1.consume(0), ty)?;

            if let Type::Function(_) = ty {
                let (p1, func) = self.func(p.consume(0))?;
                functions.push(Rc::new(RefCell::new(func)));
                p = p1;
                continue;
            }

            loop {
                let var = Rc::new(RefCell::new(Variable {
                    name: name,
                    offset: 0,
                    ty: ty.clone(),
                    is_local: false,
                    init_data: None,
                }));
                self.globals.insert(0, var.clone());
                self.var_scopes.last_mut().unwrap().push(var);

                if let Ok((p1, _)) = p1.take(";") {
                    p = p1;
                    break;
                }
                let (p2, _) = p1.take(",")?;
                let (p2, (ty1, name1)) = self.declarator(p2.consume(0), ty)?;
                p1 = p2;
                ty = ty1;
                name = name1;
            }
        }
        Ok(Program {
            functions: functions,
            globals: self.globals.clone(),
        })
    }

    // func = "typespec" "declarator" compound_stmt
    fn func(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Function<'a>), Error<'a>> {
        let (p, ty) = self.typespec(p.consume(0))?;
        let (p, (ty, _)) = self.declarator(p.consume(0), ty)?;
        let mut params: Vec<Rc<RefCell<Variable>>> = vec![];

        self.enter_scope();

        let (ty, name) = if let Type::Function(f) = ty.clone() {
            for param in f.params.iter() {
                let var = Rc::new(RefCell::new(Variable {
                    name: param.clone().name,
                    offset: 0,
                    ty: param.clone().ty,
                    is_local: true,
                    init_data: None,
                }));
                self.locals.insert(0, var.clone());
                self.var_scopes.last_mut().unwrap().push(var.clone());

                params.insert(0, var.clone())
            }
            (ty, f.name)
        } else {
            return Err(Error::ParseError(
                "Not function name".to_owned(),
                p.peek(0).clone(),
            ));
        };
        let (p, _) = p.take("{")?;

        let (p, stmt) = match self.compound_stmt(p) {
            Ok((p, s)) => (p, s),
            Err(e) => {
                #[cfg(debug_assertions)]
                eprintln!("{:?}", e);
                return Err(e);
            }
        };

        let f = Function {
            name: name,
            ty: ty,
            stmt: stmt,
            locals: self.locals.clone(),
            params: params,
            stack_size: 0,
        };

        self.leave_scope();

        Ok((p, f))
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

            let (p3, (ty, name)) = self.declarator(p.consume(0), ty.clone())?;
            let var = Rc::new(RefCell::new(Variable {
                name: name,
                offset: 0,
                ty: ty.clone(),
                is_local: true,
                init_data: None,
            }));

            p = p3;

            self.locals.insert(0, var.clone());
            self.var_scopes.last_mut().unwrap().push(var.clone());

            if let Ok((p4, _)) = p.consume(0).take("=") {
                p = p4;
            } else {
                continue;
            }

            let left = Node::Variable(var.clone(), p.peek(0).clone());

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

        self.enter_scope();
        loop {
            if let Ok((p1, _)) = p.consume(0).take("}") {
                p = p1;
                break;
            }
            let (p2, n) = if let Ok((_, _)) = self.typename(p.consume(0)) {
                self.declaration(p)?
            } else {
                self.stmt(p.consume(0))?
            };
            p = p2;
            nodes.push(n);
        }

        self.leave_scope();

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
            let _ = get_type(&left)?;
            let tr = get_type(&right)?;

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
                left =
                    Self::new_binary_node(left, right, Operator2::Eq, t.clone(), Type::Integer(8));
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("!=") {
                let (p2, right) = self.relational(p2)?;
                p = p2;
                left =
                    Self::new_binary_node(left, right, Operator2::Ne, t.clone(), Type::Integer(8));
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
                left =
                    Self::new_binary_node(left, right, Operator2::Le, t.clone(), Type::Integer(8));
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("<") {
                let (p2, right) = self.add(p2)?;
                p = p2;
                left =
                    Self::new_binary_node(left, right, Operator2::Lt, t.clone(), Type::Integer(8));
                continue;
            }
            if let Ok((p3, _)) = p.consume(0).take(">=") {
                let (p3, right) = self.add(p3)?;
                p = p3;
                left =
                    Self::new_binary_node(right, left, Operator2::Le, t.clone(), Type::Integer(8));
                continue;
            }
            if let Ok((p4, _)) = p.consume(0).take(">") {
                let t = p.peek(0).clone();
                let (p4, right) = self.add(p4)?;
                p = p4;
                left =
                    Self::new_binary_node(right, left, Operator2::Lt, t.clone(), Type::Integer(8));
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
                left =
                    Self::new_binary_node(left, right, Operator2::Mul, t.clone(), Type::Integer(8));
                continue;
            }
            if let Ok((p2, _)) = p.consume(0).take("/") {
                let (p2, right) = self.unary(p2)?;
                p = p2;
                left =
                    Self::new_binary_node(left, right, Operator2::Div, t.clone(), Type::Integer(8));
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// unary = ("+" | "-" | "*" | "&") unary
    ///       | postfix
    fn unary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0);
        if let Ok((p, _)) = p.consume(0).take("+") {
            let (p, left) = self.unary(p)?;
            return Ok((
                p,
                self.new_add_node(
                    left,
                    Node::Number(0, t.clone(), Type::Integer(8)),
                    t.clone(),
                )?,
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("-") {
            let (p, right) = self.unary(p)?;
            return Ok((
                p,
                self.new_sub_node(
                    Node::Number(0, t.clone(), Type::Integer(8)),
                    right,
                    t.clone(),
                )?,
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("&") {
            let (p, left) = self.unary(p)?;
            let ty = get_type(&left)?;
            let ty = if let Type::Array(of) = ty { of.ty } else { ty };
            return Ok((
                p,
                Self::new_unary_node(
                    left,
                    Operator1::Addr,
                    t.clone(),
                    Type::Pointer(Box::new(PointerType { ty })),
                ),
            ));
        }
        if let Ok((p, _)) = p.consume(0).take("*") {
            let t = p.peek(0).clone();
            let (p, left) = self.unary(p)?;
            let base = get_base_type(left.clone(), t.clone())?;
            return Ok((
                p,
                Self::new_unary_node(left, Operator1::Deref, t.clone(), base),
            ));
        }
        self.postfix(p)
    }

    /// postfix = primary ("[" expr "]")*
    fn postfix(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let tok = p.consume(0).peek(0).clone();
        let (mut p, mut left) = self.primary(p)?;
        loop {
            if let Ok((p1, _)) = p.consume(0).take("[") {
                let (p1, expr) = self.expr(p1)?;
                let (p1, _) = p1.consume(0).take("]")?;
                let add = self.new_add_node(left, expr, p1.peek(0).clone())?;
                let base = get_base_type(add.clone(), tok.clone())?;
                left = Self::new_unary_node(add, Operator1::Deref, tok.clone(), base);
                p = p1;
                continue;
            }
            return Ok((p.consume(0), left));
        }
    }

    /// funcall = ident "(" (assign ("," assign)*)? ")"
    fn funccall(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        let x = if let Token::Identifier(x, _) = p.peek(0) {
            *x
        } else {
            return Err(Error::ParseError("not identifier".to_owned(), t));
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
            Type::Integer(8),
        );
        let (p, _) = p.take(")")?;
        return Ok((p, call));
    }

    /// primary = "(" "{" stmt stmt* "}" ")"
    //         | "(" expr ")"
    //         | "sizeof" unary
    //         | ident args?
    //         | str
    //         | num
    fn primary(&mut self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();

        if let (Ok((_, _)), Ok((p, _))) = (p.consume(0).take("("), p.consume(1).take("{")) {
            let (p, stmt) = self.compound_stmt(p)?;
            let (body, t) = if let Node::BlockStmt(body, t) = stmt {
                (body, t)
            } else {
                unimplemented!()
            };

            let last = body.nodes.last();
            let ty = if let Some(Node::ExprStmt(node, _)) = last {
                get_type(&node.left)?
            } else {
                return Err(Error::ParseError(
                    "statement expression returning void is not supported".to_owned(),
                    t,
                ));
            };

            let node = Node::StmtExpr(
                Box::new(StmtExpr {
                    ty: ty,
                    nodes: body.nodes,
                }),
                t,
            );
            let (p, _) = p.take(")")?;
            return Ok((p, node));
        }

        if let Ok((p, _)) = p.consume(0).take("(") {
            let (p, node) = self.expr(p)?;
            let (p, _) = p.take(")")?;
            return Ok((p, node));
        }

        if let Ok((p, _)) = p.consume(0).take("sizeof") {
            let (p, node) = self.unary(p)?;
            let ty = get_type(&node)?;
            let n = size_of(ty);
            return Ok((p.consume(0), Node::Number(n, t, Type::Integer(8))));
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

        if let Token::Str(s, _) = p.peek(0) {
            let tok = p.peek(0);
            // NULL is not included in token, then
            // append NULL at end.
            let v = Variable {
                ty: Type::Array(Box::new(ArrayType {
                    ty: Type::Integer(1),
                    len: 1 + s.len() as i64,
                })),
                name: format!(".L.data.{}", self.globals.len()),
                is_local: false,
                offset: 0,
                init_data: Some(format!("{}\0", s.to_string())),
            };
            let var = Rc::new(RefCell::new(v));
            self.globals.insert(0, var.clone());

            return Ok((p.consume(1), Node::Variable(var.clone(), tok.clone())));
        }
        self.num(p)
    }

    fn num(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        let t = p.peek(0).clone();
        if let Token::Number(i, _) = p.peek(0) {
            return Ok((p.consume(1), Node::Number(*i, t, Type::Integer(8))));
        }
        Err(Error::ParseError(format!("not number"), t.clone()))
    }

    ///
    /// Parse type
    ///

    // typename = "char" | "int"

    fn typename(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Node<'a>), Error<'a>> {
        if let Ok((p, t)) = p.consume(0).take("char") {
            return Ok((p, t));
        }
        if let Ok((p, t)) = p.consume(0).take("int") {
            return Ok((p, t));
        }
        return Err(Error::ParseError(
            "not type name".to_owned(),
            p.consume(0).peek(0).clone(),
        ));
    }

    fn typespec(&self, p: Tokens<'a>) -> Result<(Tokens<'a>, Type), Error<'a>> {
        if let Ok((p, _)) = p.consume(0).take("char") {
            return Ok((p, Type::Integer(1)));
        }
        let (p, _) = p.consume(0).take("int")?;
        Ok((p, Type::Integer(8)))
    }

    // declarator = "*"* ident
    fn declarator(
        &self,
        mut p: Tokens<'a>,
        ty: Type,
    ) -> Result<(Tokens<'a>, (Type, String)), Error<'a>> {
        let mut t = ty;
        while let Ok((p1, _)) = p.consume(0).take("*") {
            t = Type::Pointer(Box::new(PointerType { ty: t }));
            p = p1;
        }
        if let Token::Identifier(name, _) = p.peek(0) {
            let (p, t) = self.type_suffix(p.consume(0), t, name, 1)?;
            return Ok((p.consume(0), (t, name.to_string())));
        }
        Err(Error::ParseError(
            "Not identifier".to_string(),
            p.peek(0).clone(),
        ))
    }

    // type-suffix = "(" func-params
    //      | "[" num "]" type-suffix
    //      | ε
    fn type_suffix(
        &self,
        p: Tokens<'a>,
        ty: Type,
        name: &str,
        next: usize,
    ) -> Result<(Tokens<'a>, Type), Error<'a>> {
        if let Ok((p, _)) = p.consume(next).take("(") {
            return self.func_params(p, ty, name);
        }
        if let Ok((p, _)) = p.consume(next).take("[") {
            let (p, size) = self.num(p)?;
            let len = if let Node::Number(n, _, _) = size {
                n
            } else {
                unreachable!("should be number");
            };
            let (p, _) = p.consume(0).take("]")?;
            let (p, ty) = self.type_suffix(p, ty, name, 0)?;
            return Ok((p, Type::Array(Box::new(ArrayType { ty, len }))));
        }
        Ok((p.consume(next).clone(), ty))
    }

    // func_params = (param ("," param)*)? ")"
    fn func_params(
        &self,
        p: Tokens<'a>,
        ty: Type,
        name: &str,
    ) -> Result<(Tokens<'a>, Type), Error<'a>> {
        let mut after_second: Option<()> = None;
        let mut p = p;
        let mut params: Vec<ParameterType> = vec![];
        loop {
            if let Ok((p, _)) = p.take(")") {
                return Ok((
                    p,
                    Type::Function(Box::new(FunctionType {
                        name: name.to_owned(),
                        return_ty: ty,
                        params: params,
                    })),
                ));
            }

            p = if let Some(_) = after_second {
                let (p, _) = p.consume(0).take(",")?;
                p
            } else {
                after_second = Some(());
                p
            };

            let (p1, basety) = self.typespec(p)?;
            let (p1, (basety, name)) = self.declarator(p1, basety)?;
            p = p1;
            params.push(ParameterType {
                name: name,
                ty: basety,
            });
        }
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
        let tl = get_type(&left)?;
        let tr = get_type(&right)?;
        match (tl, tr) {
            (Type::Integer(i), Type::Integer(_)) => Ok(Self::new_binary_node(
                left,
                right,
                Operator2::Add,
                t.clone(),
                Type::Integer(i),
            )),
            (Type::Integer(_), Type::Pointer(to)) => Ok(Self::new_binary_node(
                Self::new_binary_node(
                    left.clone(),
                    Node::Number(size_of(to.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                right,
                Operator2::Add,
                t,
                Type::Pointer(to.clone()),
            )),
            (Type::Pointer(to), Type::Integer(_)) => Ok(Self::new_binary_node(
                left.clone(),
                Self::new_binary_node(
                    right,
                    Node::Number(size_of(to.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                Operator2::Add,
                t,
                Type::Pointer(to),
            )),
            (Type::Integer(_), Type::Array(of)) => Ok(Self::new_binary_node(
                Self::new_binary_node(
                    left.clone(),
                    Node::Number(size_of(of.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                right,
                Operator2::Add,
                t,
                Type::Array(of),
            )),
            (Type::Array(of), Type::Integer(_)) => Ok(Self::new_binary_node(
                left.clone(),
                Self::new_binary_node(
                    right,
                    Node::Number(size_of(of.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                Operator2::Add,
                t,
                Type::Array(of),
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
        let tl = get_type(&left)?;
        let tr = get_type(&right)?;

        match (tl, tr) {
            (Type::Integer(_), Type::Integer(_)) => Ok(Self::new_binary_node(
                left,
                right,
                Operator2::Sub,
                t,
                Type::Integer(8),
            )),
            (Type::Pointer(to), Type::Integer(_)) => Ok(Self::new_binary_node(
                left,
                Self::new_binary_node(
                    right,
                    Node::Number(size_of(to.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                Operator2::Sub,
                t,
                Type::Pointer(Box::new(PointerType {
                    ty: Type::Integer(8),
                })),
            )),
            (Type::Array(of), Type::Integer(_)) => Ok(Self::new_binary_node(
                left,
                Self::new_binary_node(
                    right,
                    Node::Number(size_of(of.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Mul,
                    t.clone(),
                    Type::Integer(8),
                ),
                Operator2::Sub,
                t,
                Type::Array(of),
            )),
            (Type::Pointer(to), Type::Pointer(_)) => {
                let node = Self::new_binary_node(
                    left,
                    right,
                    Operator2::Sub,
                    t.clone(),
                    Type::Pointer(to.clone()),
                );
                let node = Self::new_binary_node(
                    node,
                    Node::Number(size_of(to.clone().ty), t.clone(), Type::Integer(8)),
                    Operator2::Div,
                    t.clone(),
                    Type::Pointer(to.clone()),
                );
                Ok(node)
            }
            _ => Err(Error::ParseError("invalid operand".to_string(), t)),
        }
    }
}
