#![allow(non_camel_case_types)]
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum ProgramItems {
    Function(Function),
    GlobalVar(Stmt)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<ProgramItems>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub return_type: String,
    pub name: String, 
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(String, Option<Expr>),
    Expression(Expr),
    Return(Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Box<Stmt>, Expr, Box<Stmt>, Box<Stmt>),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    _int(i64),
    _float(f64),
    _char(u8),
    ident(String),
    binop(Box<Expr>, Token, Box<Expr>),
    unop(Token, Box<Expr>),
}

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

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut items = Vec::new();
    
        while let Some(token) = self.peek() {
            match token {
                Token::keyword(ref kw) if kw == "int" || kw == "void" || kw == "float" => {
                    if let Some(Token::ident(_)) = self.tokens.get(self.cursor + 1) {
                        let func = self.parse_function()?;
                        items.push(ProgramItems::Function(func));
                    } else {
                        let var_decl = self.parse_statement()?;
                        items.push(ProgramItems::GlobalVar(var_decl));
                    }
                }
                _ => break,
            }
        }
    
        Ok(Program { items })
    }

    fn parse_function(&mut self) -> Result<Function, String> {
        println!("parsing function");
        let return_type = match self.advance() {
            Some(Token::keyword(kw)) => kw,
            _ => return Err("Expected return type".to_string()),
        };

        let name = match self.advance() {
            Some(Token::ident(name)) => name,
            _ => return Err("Expected function name".to_string()),
        };

        self.expect(Token::lparen)?;
        let mut params = Vec::new();

        while let Some(Token::ident(param)) = self.peek().cloned() {
            self.advance();
            params.push(param);
            if self.peek() == Some(&Token::comma) {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(Token::rparen)?;
        self.expect(Token::lbrace)?;
        let mut body = Vec::new();

        while self.peek() != Some(&Token::rbrace) {
            body.push(self.parse_statement()?);
        }

        self.expect(Token::rbrace)?;
        println!("parsed function");
        Ok(Function { return_type, name, params, body })
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        println!("parsing statement: {:?}", self.peek());
        match self.peek() {
            Some(Token::keyword(ref kw)) if kw == "int" || kw == "float" || kw == "char" => {
                self.parse_variable_declaration()
            }
            Some(Token::keyword(ref kw)) if kw == "return" => {
                self.advance();
                let expr = if self.peek() != Some(&Token::semicolon) {
                    Some(self.parse_expression(0)?)
                } else {
                    None
                };
                self.expect(Token::semicolon)?;
                Ok(Stmt::Return(expr))
            }
            Some(Token::keyword(ref kw)) if kw == "for" => self.parse_for_statement(),
            Some(Token::keyword(ref kw)) if kw == "if" => self.parse_if_statement(),
            Some(Token::keyword(ref kw)) if kw == "while" => self.parse_while_statement(),
            Some(Token::lbrace) => self.parse_block(),
            _ => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::semicolon)?;
                Ok(Stmt::Expression(expr))
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Stmt, String> {
        let var_type = self.advance();
        let name = match self.advance() {
            Some(Token::ident(name)) => name,
            _ => return Err("Expected variable name".to_string()),
        };

        println!("vr_type: {:?}, name: {:?}", var_type, name);

        let init = if self.peek() == Some(&Token::assign) {
            self.advance();
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        self.expect(Token::semicolon)?;
        println!("declared var");
        Ok(Stmt::VarDecl(name, init))
    }

    fn parse_block(&mut self) -> Result<Stmt, String> {
        self.expect(Token::lbrace)?;
        let mut statements = Vec::new();
        while self.peek() != Some(&Token::rbrace) {
            statements.push(self.parse_statement()?);
        }
        self.expect(Token::rbrace)?;
        Ok(Stmt::Block(statements))
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::keyword("if".to_string()))?;
        self.expect(Token::lparen)?;
        let condition = self.parse_expression(0)?;
        self.expect(Token::rparen)?;
        let then_branch = Box::new(self.parse_statement()?);
        let else_branch = if self.peek() == Some(&Token::keyword("else".to_string())) {
            self.advance();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        Ok(Stmt::If(condition, then_branch, else_branch))
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::keyword("while".to_string()))?;
        self.expect(Token::lparen)?;
        let condition = self.parse_expression(1)?;
        self.expect(Token::rparen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::While(condition, body))
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, String> {
        self.expect(Token::keyword("for".to_string()))?;
        self.expect(Token::lparen)?;
        let init = Box::new(self.parse_statement()?);
        let condition = self.parse_expression(0)?;
        self.expect(Token::semicolon)?;
        let update = Box::new(self.parse_statement()?);
        self.expect(Token::rparen)?;
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::For(init, condition, update, body))
    }

    fn parse_expression(&mut self, min_precedence: u8) -> Result<Expr, String> {
        let mut left = self.parse_primary()?;

        while let Some(op) = self.peek() {
            let precedence = match op {
                Token::plus | Token::minus => 2,
                Token::star | Token::slash | Token::percent => 3,
                Token::eq | Token::neq | Token::lt | Token::gt | Token::lte | Token::gte => 1,
                Token::logical_and | Token::logical_or => 0,
                _ => break,
            };

            if precedence < min_precedence {
                break;
            }

            let op = self.advance().unwrap();
            let right = self.parse_expression(precedence + 1)?;

            left = Expr::binop(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        println!("parsing token: {:?}", self.peek());
        let expr = match self.advance() {
            Some(Token::_int(n)) => Ok(Expr::_int(n)),
            Some(Token::_float(f)) => Ok(Expr::_float(f)),
            Some(Token::_char(c)) => Ok(Expr::_char(c)),
            Some(Token::ident(name)) => Ok(Expr::ident(name)),
            Some(Token::lparen) => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::rparen)?;
                Ok(expr)
            }
            Some(tok) => Err(format!("Unexpected token in primary expression: {:?}", tok)),
            None => Err("Unexpected EOF in primary expression".to_string()),
        }?;

        if let Some(&Token::incr) | Some(&Token::decr) = self.peek() {
            let op = self.advance().unwrap();
            return Ok(Expr::unop(op, Box::new(expr)));
        }
    
        Ok(expr)
    }
}