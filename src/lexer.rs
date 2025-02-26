#![allow(non_camel_case_types)]

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    eof,
    ident(String),
    unknown(String),

    // keywords
    int_t, float_t, char_t, _return, _void
    
    _int(i64),
    _float(f64),
    _char(char),

    // binop
    plus, minus, star, slash, percent,
    assign, logical_and, logical_or,
    
    // icmp (integer comparison)
    eq, neq, lt, lte, gt, gte,

    // unop
    exclamation, shiftl, shiftr, incr, decr,

    comma, semicolon,
    lparen, rparen, lbrace, rbrace,
}

impl Token {
    pub fn is_op(&self) -> bool {
        match self {
            Token::exclamation| Token::shiftl| Token::shiftr| Token::incr| Token::decr|
            Token::plus| Token::minus| Token::star| Token::slash| Token::percent|
            Token::assign| Token::logical_and| Token::logical_or| => true,
            _ => false
        }
    }
}

pub struct Lexer {
    input: Vec<char>,
    cursor: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            cursor: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.cursor).copied()
    }

    fn advance(&mut self) -> Option<char> {
        if self.cursor < self.input.len() {
            self.cursor += 1;
            self.input.get(self.cursor - 1).copied()
        } else {
            None
        }
    }
    
    fn lex_number(&mut self) -> Token {
        let start = self.cursor;
        let mut has_dot = false;

        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' {
                if has_dot {
                    break;
                }
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let num: String = self.input[start..self.cursor].iter().collect();
        if has_dot {
            Token::_float(num.parse::<f32>().unwrap())
        } else {
            Token::_int(num.parse::<i32>().unwrap())
        }
    }
    

    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.cursor;
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let ident: String = self.input[start..self.cursor].iter().collect();
        match ident.as_str() {
            "int" => Token::int_t,
            "float" => Token::float_t,
            "char" => Token::char_t,
            "return" => Token::_return,
            "void" => Token::_void,
            _ => Token::ident(ident)
        }
    }

    fn lex_string_or_char(&mut self) -> Token {
        let quote = self.advance().unwrap();
        let start = self.cursor;
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

        let literal: String = self.input[start..self.cursor].iter().collect();
        self.advance();

        if quote == '\'' {
            if literal.len() == 1 {
                Token::_char(literal.chars().next().unwrap())
            } else {
                Token::unknown(format!("Invalid char literal: '{}'", literal))
            }
        } else {
            Token::unknown(literal)
        }
    }

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
