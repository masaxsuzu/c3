#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Illegal(u8),
    Eof,

    Number(i64),
    Identifier(&'a str),

    Reserved(&'a str),
}

impl<'a> Token<'a> {
    pub fn is_reserved(&self, s: &str) -> bool {
        match self {
            Token::Reserved(r) => s.to_string() == r.to_string(),
            _ => false,
        }
    }
}
