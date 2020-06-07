#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Illegal(u8,usize),
    Eof(usize),

    Number(i64,usize),
    Identifier(&'a str,usize),

    Reserved(&'a str,usize),
}

impl<'a> Token<'a> {
    pub fn is_reserved(&self, s: &str) -> bool {
        match self {
            Token::Reserved(r,_) => s.to_string() == r.to_string(),
            _ => false,
        }
    }
}
