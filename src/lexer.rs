#![allow(non_camel_case_types)]

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    eof,
    ident(String),
    keyword(String),
    unknown(String),

    _int(u64),
    _float(f64),
    _char(u8),
    _string(String),

    plus, minus, star, slash, percent, exclamation,
    assign, // =
    
    eq, neq, lt, lte, gt, gte,

    logical_and, logical_or,
    shiftl, shiftr, incr, decr,

    comma, semicolon,
    lparen, rparen, lbrace, rbrace,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            self.pos += 1;
            self.input.get(self.pos - 1).copied()
        } else {
            None
        }
    }

    /// Parses a number (integer or float)
    fn lex_number(&mut self) -> Token {
        let start = self.pos;
        let mut has_dot = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' {
                if has_dot {
                    break; // Second dot encountered -> end of number
                }
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let num: String = self.input[start..self.pos].iter().collect();
        if has_dot {
            Token::_float(num.parse().unwrap())
        } else {
            Token::_int(num.parse().unwrap())
        }
    }

    /// Parses an identifier or keyword
    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let ident: String = self.input[start..self.pos].iter().collect();
        let keywords = ["int", "float", "return", "if", "else", "for", "while", "void"];
        if keywords.contains(&ident.as_str()) {
            Token::keyword(ident)
        } else {
            Token::ident(ident)
        }
    }

    /// Parses string literals `"..."` and character literals `'a'`
    fn lex_string_or_char(&mut self) -> Token {
        let quote = self.advance().unwrap(); // Consume starting quote (' or ")
        let start = self.pos;
        let mut escaped = false;

        while let Some(c) = self.peek() {
            if escaped {
                escaped = false;
                self.advance();
            } else if c == '\\' {
                escaped = true;
                self.advance();
            } else if c == quote {
                break;
            } else {
                self.advance();
            }
        }

        let literal: String = self.input[start..self.pos].iter().collect();
        self.advance(); // Consume closing quote

        if quote == '\'' {
            if literal.len() == 1 {
                Token::_char(literal.chars().next().unwrap() as u8)
            } else {
                Token::unknown(format!("Invalid char literal: '{}'", literal))
            }
        } else {
            Token::_string(literal)
        }
    }

    /// Parses operators and punctuations
    fn lex_operator(&mut self) -> Option<Token> {
        match self.advance()? {
            '+' => Some(if self.peek() == Some('+') {
                self.advance();
                Token::incr
            } else {
                Token::plus
            }),
            '-' => Some(if self.peek() == Some('-') {
                self.advance();
                Token::decr
            } else {
                Token::minus
            }),
            '*' => Some(Token::star),
            '/' => Some(Token::slash),
            '%' => Some(Token::percent),
            '!' => Some(if self.peek() == Some('=') {
                self.advance();
                Token::neq
            } else {
                Token::exclamation
            }),
            '=' => Some(if self.peek() == Some('=') {
                self.advance();
                Token::eq
            } else {
                Token::assign
            }),
            '<' => Some(if self.peek() == Some('=') {
                self.advance();
                Token::lte
            } else if self.peek() == Some('<') {
                self.advance();
                Token::shiftl
            } else {
                Token::lt
            }),
            '>' => Some(if self.peek() == Some('=') {
                self.advance();
                Token::gte
            } else if self.peek() == Some('>') {
                self.advance();
                Token::shiftr
            } else {
                Token::gt
            }),
            '&' => Some(if self.peek() == Some('&') {
                self.advance();
                Token::logical_and
            } else {
                Token::unknown("&".to_string())
            }),
            '|' => Some(if self.peek() == Some('|') {
                self.advance();
                Token::logical_or
            } else {
                Token::unknown("|".to_string())
            }),
            ',' => Some(Token::comma),
            ';' => Some(Token::semicolon),
            '(' => Some(Token::lparen),
            ')' => Some(Token::rparen),
            '{' => Some(Token::lbrace),
            '}' => Some(Token::rbrace),
            _ => None,
        }
    }

    fn next_token(&mut self) -> Token {
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => return self.lex_number(),
                'a'..='z' | 'A'..='Z' | '_' => return self.lex_ident_or_keyword(),
                '"' | '\'' => return self.lex_string_or_char(),
                '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>' | '&' | '|' | '%' | ',' | ';' | '(' | ')' | '{' | '}' => {
                    return self.lex_operator().unwrap_or(Token::unknown(c.to_string()))
                }
                _ if c.is_whitespace() => {
                    self.advance();
                    continue;
                }
                _ => {
                    self.advance();
                    return Token::unknown(c.to_string());
                }
            }
        }
        Token::eof
    }
    

    /// Tokenizes the entire input
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            if token == Token::eof {
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}
