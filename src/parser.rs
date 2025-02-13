pub struct Parsec {
    tokens: Vec<Token>,
    cursor: usize
}

impl Parsec {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
            Some(self.tokens[self.cursor - 1].clone())
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Token) -> Result<Token, String> {
        match self.advance() {
            Some(token) if token == expected => Ok(token),
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected {:?}, found EOF", expected)),
        }
    }
}