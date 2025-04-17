use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Float,
    Char,
    Void,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Variable(String),
}

#[derive(Debug)]
pub enum Stmt {
    Return(Expr),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: PrimitiveType,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

pub struct Parsec {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parsec {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.pos);
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &Token) -> bool {
        if self.peek() == Some(expected) {
            self.next();
            true
        } else {
            false
        }
    }
}

impl Parsec {
    pub fn parse_program(&mut self) -> Program {
        let mut functions = Vec::new();
        while self.peek().is_some() {
            functions.push(self.parse_function());
        }
        Program { functions }
    }

    fn parse_function(&mut self) -> Function {
        let return_type = match self.next() {
            Some(Token::Keyword(kw)) => match kw.as_str() {
                "int" => PrimitiveType::Int,
                "float" => PrimitiveType::Float,
                "char" => PrimitiveType::Char,
                "void" => PrimitiveType::Void,
                _ => panic!("Unknown return type keyword"),
            },
            _ => panic!("Expected return type keyword"),
        };

        let name = match self.next() {
            Some(Token::Identifier(name)) => name.clone(),
            _ => panic!("Expected function name"),
        };

        assert!(self.expect(&Token::Symbol('(')));
        assert!(self.expect(&Token::Symbol(')')));
        assert!(self.expect(&Token::Symbol('{')));

        let mut body = Vec::new();
        while let Some(Token::Keyword(k)) = self.peek() {
            if k == "return" {
                self.next();
                let expr = self.parse_expr();
                assert!(self.expect(&Token::Symbol(';')));
                body.push(Stmt::Return(expr));
            } else {
                panic!("Unsupported statement");
            }
        }

        assert!(self.expect(&Token::Symbol('}')));

        Function {
            name,
            return_type,
            body,
        }
    }

    fn parse_expr(&mut self) -> Expr {
        match self.next() {
            Some(Token::Number(n)) => Expr::Number(n.parse().unwrap()),
            Some(Token::Identifier(name)) => Expr::Variable(name.clone()),
            _ => panic!("Unsupported expression"),
        }
    }
}
