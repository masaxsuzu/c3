use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    next_pos: usize,
    ch: u8,
    keywords: [&'a str; 5],
    two_letter_punctuations: [&'a str; 4],
    one_letter_punctuations: [&'a str; 14],
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            next_pos: 0,
            ch: 0,
            keywords: ["return", "if", "else", "for", "while"],
            two_letter_punctuations: ["==", "!=", "<=", ">="],
            one_letter_punctuations: [
                "+", "-", "*", "/", "=", "!", "<", ">", ";", "=", "(", ")", "{", "}",
            ],
        };

        lexer.read_char();

        return lexer;
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        let token = match self.ch {
            0 => Token::Eof(self.pos),
            b'0'..=b'9' => self.consume_number(),
            _ => self.consume_keyword(self.ch),
        };

        token
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
            Some(n) => Token::Number(n, self.pos),
            None => Token::Illegal(self.ch, self.pos),
        }
    }

    fn consume_keyword(&mut self, start: u8) -> Token<'a> {
        for keyword in self.keywords.iter() {
            let n = keyword.len();
            if self.starts_with(keyword) && !Self::is_alpha_num(self.nth_char(n)) {
                let token: Token<'a> = Token::Reserved(*keyword, self.pos);
                self.read_n(n);
                return token;
            }
        }

        for p in self.two_letter_punctuations.iter() {
            if self.starts_with(p) {
                let token: Token<'a> = Token::Reserved(*p, self.pos);
                self.read_n(2);
                return token;
            }
        }

        for p in self.one_letter_punctuations.iter() {
            if self.starts_with(p) {
                let token = Token::Reserved::<'a>(*p, self.pos);
                self.read_n(1);
                return token;
            }
        }

        let mut _x = Some(0);
        while let Some(n) = _x {
            if Self::is_alpha_num(self.nth_char(n)) {
                _x = Some(n + 1);
            } else if n == 0 {
                return Token::Eof(self.pos);
            } else {
                _x = None;
                let name = self.nth_str(n);
                let token = Token::Identifier::<'a>(name, self.pos);
                self.read_n(n);
                return token;
            }
        }

        self.read_n(1);
        Token::Illegal(start,self.pos)
    }

    fn starts_with(&self, word: &str) -> bool {
        let size = word.len();
        let consumed = self.nth_str(size);
        word == consumed
    }

    fn read_n(&mut self, n: usize) {
        for _ in 0..n {
            self.read_char();
        }
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

    fn is_alpha(ch: u8) -> bool {
        match ch {
            b'a'..=b'z' => true,
            b'A'..=b'Z' => true,
            b'_' => true,
            _ => false,
        }
    }

    fn is_alpha_num(ch: u8) -> bool {
        match ch {
            b'0'..=b'9' => true,
            c => Self::is_alpha(c),
        }
    }

    fn nth_char(&self, n: usize) -> u8 {
        let max = self.input.len();
        let ch = if self.pos + n < max {
            self.input.as_bytes()[self.pos + n]
        } else {
            0
        };
        ch
    }

    fn nth_str(&self, n: usize) -> &'a str {
        let max = self.input.len();
        let consumed = if self.pos + n < max {
            &self.input[self.pos..self.pos + n]
        } else {
            &self.input[self.pos..max]
        };
        consumed
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
            Token::Eof(_) => None,
            x => {
                #[cfg(debug_assertions)]
                eprintln!("{:?}", x);
                Some(x)
            }
        }
    }
}
