use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: u8,
    two_letter_keywords: [&'a str; 4],
    one_letter_keywords: [&'a str; 9],
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: 0,
            two_letter_keywords: ["==", "!=", "<=", ">="],
            one_letter_keywords: ["+", "-", "*", "/", "=", "!", "<", ">", ";"],
        };

        lexer.read_char();

        return lexer;
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        let token = match self.ch {
            b'0'..=b'9' => return self.consume_number(),
            b'+' => return self.consume_keyword(self.ch),
            b'-' => return self.consume_keyword(self.ch),
            b'*' => return self.consume_keyword(self.ch),
            b'/' => return self.consume_keyword(self.ch),
            b'!' => return self.consume_keyword(self.ch),
            b'=' => return self.consume_keyword(self.ch),
            b'<' => return self.consume_keyword(self.ch),
            b'>' => return self.consume_keyword(self.ch),
            b';' => return self.consume_keyword(self.ch),
            0 => Token::Eof,
            x => Token::Illegal(x),
        };

        self.read_char();

        return token;
    }

    fn consume_number(&mut self) -> Token<'a> {
        let start_pos = self.pos;
        loop {
            match self.ch {
                b'0'..=b'9' => self.read_char(),
                _ => break,
            }
        }

        let consumed = match self.ch {
            0 => &self.input[start_pos..self.pos + 1],
            _ => &self.input[start_pos..self.pos],
        };

        match consumed.parse::<i64>().ok() {
            Some(n) => Token::Number(n),
            None => Token::Illegal(self.ch),
        }
    }

    fn consume_keyword(&mut self, start: u8) -> Token<'a> {
        for p in self.two_letter_keywords.iter() {
            if self.starts_with(p) {
                let token: Token<'a> = Token::Reserved(*p);
                &self.read_char();
                &self.read_char();
                return token;
            }
        }
        for p in self.one_letter_keywords.iter() {
            if self.starts_with(p) {
                let token = Token::Reserved::<'a>(*p);
                &self.read_char();
                return token;
            }
        }
        Token::Illegal(start)
    }

    fn starts_with(&self, word: &str) -> bool {
        let size = word.len();

        let max = self.input.len();
        let consumed = if self.pos + size < max {
            &self.input[self.pos..self.pos + size]
        } else {
            &self.input[self.pos..max]
        };
        word == consumed
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = 0;
            return;
        }

        self.ch = self.input.as_bytes()[self.next_pos];
        self.pos = self.next_pos;
        self.next_pos += 1;
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' | b'\r' => self.read_char(),
                _ => break,
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Token<'a>> {
        match self.next_token() {
            Token::Eof => None,
            x => {
                // print!("# {:?}\n",x);
                Some(x)
            }
        }
    }
}
