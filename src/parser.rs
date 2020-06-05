#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ParseError(String),
}

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, multispace0},
    error::ErrorKind,
    IResult,
};

use crate::ast::{Binary, Node, Operator};

pub fn parse(s: &str) -> Result<Node, Error> {
    match expr(s) {
        Ok((_,n)) => Ok(n),
        Err(e) => {
            return Err(Error::ParseError(e.to_string()));
        }
    }
}

fn expr(s: &str) -> IResult<&str, Node> {
    equality(s)
}

// equality = relational ("==" relational | "!=" relational)*
fn equality(s: &str) -> IResult<&str, Node> {
    let (mut x, mut left) = relational(s)?;
    loop {
        let (y, _) = multispace0(x)?;
        if let Ok((y, op)) = alt((
            tag::<&str, &str, (&str, ErrorKind)>("=="),
            tag::<&str, &str, (&str, ErrorKind)>("!="),
        ))(y)
        {
            let (z, right) = relational(y)?;
            x = z;
            match op {
                "==" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Eq),
                "!=" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Ne),
                _ => {}
            }
        } else {
            return Ok((x, left));
        }
    }
}

// relational = add ("<=" add | "<" add | ">=" add | ">"" add)*
fn relational(s: &str) -> IResult<&str, Node> {
    let (mut x, mut left) = add(s)?;
    loop {
        let (y, _) = multispace0(x)?;
        if let Ok((y, op)) = alt((
            tag::<&str, &str, (&str, ErrorKind)>("<="),
            tag::<&str, &str, (&str, ErrorKind)>("<"),
            tag::<&str, &str, (&str, ErrorKind)>(">="),
            tag::<&str, &str, (&str, ErrorKind)>(">"),
        ))(y)
        {
            let (z, right) = add(y)?;
            x = z;
            match op {
                "<=" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Le),
                "<" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Lt),
                ">=" => left = Node::Binary(Box::new(Binary {  left:right, right:left  }), Operator::Le),
                ">" => left = Node::Binary(Box::new(Binary { left:right, right:left }), Operator::Lt),
                _ => {}
            }
        } else {
            return Ok((x, left));
        }
    }
}

// add = mul ("+" mul | "-" mul)*
fn add(s: &str) -> IResult<&str, Node> {
    let (mut x, mut left) = mul(s)?;

    loop {
        let (y, _) = multispace0(x)?;
        if let Ok((y, op)) = alt((
            tag::<&str, &str, (&str, ErrorKind)>("+"),
            tag::<&str, &str, (&str, ErrorKind)>("-"),
        ))(y)
        {
            let (z, right) = mul(y)?;
            x = z;
            match op {
                "+" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Add),
                "-" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Sub),
                _ => {}
            }
        } else {
            return Ok((x, left));
        }
    }
}

// mul = unary ("*" unary | "/" unary)*
fn mul(s: &str) -> IResult<&str, Node> {
    let (mut x, mut left) = unary(s)?;

    loop {
        let (y, _) = multispace0(x)?;
        if let Ok((y, op)) = alt((
            tag::<&str, &str, (&str, ErrorKind)>("*"),
            tag::<&str, &str, (&str, ErrorKind)>("/"),
        ))(y)
        {
            let (z, right) = unary(y)?;
            x = z;
            match op {
                "*" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Mul),
                "/" => left = Node::Binary(Box::new(Binary { left, right }), Operator::Div),
                _ => {}
            }
        } else {
            return Ok((x, left));
        }
    }
}

// unary = ("+" | "-") unary
//       | primary
fn unary(s: &str) -> IResult<&str, Node> {
    let (s, _) = multispace0(s)?;
    if let Ok((x, _)) = tag::<&str, &str, (&str, ErrorKind)>("+")(s) {
        return unary(x);
    }
    if let Ok((x, _)) = tag::<&str, &str, (&str, ErrorKind)>("-")(s) {
        let left = Node::Number(0);
        let (x, right) = unary(x)?;
        return Ok((
            x,
            Node::Binary(Box::new(Binary { left, right }), Operator::Sub),
        ));
    }
    primary(s)
}

// primary = "(" expr ")" | num
fn primary(s: &str) -> IResult<&str, Node> {
    let (s, _) = multispace0(s)?;
    if let Ok((x, _)) = tag::<&str, &str, (&str, ErrorKind)>("(")(s) {
        let (x, _) = multispace0(x)?;
        let (x, node) = expr(x)?;
        let (x, _) = multispace0(x)?;
        let (x, _) = tag(")")(x)?;

        return Ok((x, node));
    }
    num(s)
}

fn num(s: &str) -> IResult<&str, Node> {
    let (s, v1) = digit1(s)?;
    Ok((s, Node::Number(v1.parse().unwrap())))
}
