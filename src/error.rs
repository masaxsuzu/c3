use std::fmt;

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
    ParseError(String, Token<'a>),
    DisplayError(String, String, usize, String), // label, source, loc, message
}

impl<'a> fmt::Display for Error<'a> {
    ///
    /// Display error.
    /// The message is like ...
    /// ```
    /// line: 11
    /// x = 1
    ///      ^ not ;
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ParseError(_, _) => write!(f, "{:?}", self),
            Error::DisplayError(file_name, source, loc, msg) => {
                let (n, p) = get_line_number(source, *loc);
                let line = source.split('\n').nth(n).unwrap();
                write!(f, "{}:{}\n", file_name, n + 1)?;
                write!(f, "{}\n", line)?;
                write!(f, "{}", " ".repeat(p))?;
                write!(f, "^ {}", msg)
            }
        }
    }
}

fn get_line_number(source: &String, pos: usize) -> (usize, usize) {
    let (n, line_start) = source
        .chars()
        .take(pos)
        .enumerate()
        .filter(|c| c.1 == '\n')
        .enumerate()
        .last()
        .map(|(n, (m, _))| (n + 1, m + 1))
        .unwrap_or((0, 0));
    (n, pos - line_start)
}
