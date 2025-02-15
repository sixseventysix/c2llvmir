#![allow(non_camel_case_types)]
use crate::lexer::Token;

fn get_llvm_type(type_str: &str) -> &str {
    match type_str {
        "int" => "i32",
        "float" => "float",
        "char" => "i8",
        _ => "void",
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProgramItems {
    Function(Function),
    GlobalVar(Stmt)
}

impl ProgramItems {
    pub fn to_llvm_ir(&self) -> String {
        match self {
            ProgramItems::Function(func) => func.to_llvm_ir(),
            ProgramItems::GlobalVar(var) => var.to_llvm_ir(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<ProgramItems>
}

impl Program {
    pub fn to_llvm_ir(&self) -> String {
        let mut ir = String::new();
        for item in &self.items {
            ir.push_str(&item.to_llvm_ir());
        }
        ir
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub return_type: String,
    pub name: String, 
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn to_llvm_ir(&self) -> String {
        let llvm_ret_ty = get_llvm_type(&self.return_type);

        let mut ir = format!("define {} @{} () {{\nentry:\n", llvm_ret_ty, self.name);

        for stmt in &self.body {
            ir.push_str(&stmt.to_llvm_ir());
        }

        ir.push_str("}\n");
        ir
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    VarDecl(String, String, Option<Expr>),
    Expression(Expr),
    Return(Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Box<Stmt>, Expr, Box<Stmt>, Box<Stmt>),
    Block(Vec<Stmt>),
}

impl Stmt {
    pub fn to_llvm_ir(&self) -> String {
        match self {
            Stmt::VarDecl(var_ty, name, Some(expr)) => format!(
                "%{} = alloca {}\nstore {} {} {}* %{}\n",
                name, get_llvm_type(var_ty), get_llvm_type(var_ty), expr.to_llvm_ir(), get_llvm_type(var_ty), name
            ),
            Stmt::VarDecl(var_ty, name, None) => format!(
                "%{} = alloca {}\n",
                name, get_llvm_type(var_ty)
            ),

            Stmt::Expression(expr) => expr.to_llvm_ir(),
            Stmt::Return(Some(expr)) => format!(
                "ret {}\n",
                expr.to_llvm_ir()
            ),
            Stmt::Return(None) => "ret void\n".to_string(),
            Stmt::Block(statements) => {
                let mut ir = String::new();
                for stmt in statements {
                    ir.push_str(&stmt.to_llvm_ir());
                }
                ir
            }
            misc => format!("{:?} is not supported right now\n",misc),
        }
    }
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

impl Expr {
    pub fn to_llvm_ir(&self) -> String {
        match self {
            Expr::_int(value) => format!("i32 {}", value),

            // Floating-Point Literal
            Expr::_float(value) => format!("double {}", value),

            // Character Literal (stored as i8)
            Expr::_char(value) => format!("i8 {}", value),

            // Identifier (load variable from memory)
            Expr::ident(name) => format!("load {}, {}* %{}", get_llvm_type(name), get_llvm_type(name), name),

            // Unary Operators (negation, logical NOT)
            Expr::unop(op, expr) => {
                let expr_ir = expr.to_llvm_ir();
                let result_var = generate_temp_var();

                match op {
                    Token::minus => format!("{} = sub {} 0, {}\n", result_var, get_expr_type(expr), expr_ir),
                    Token::exclamation => format!("{} = xor {} {}, -1\n", result_var, get_expr_type(expr), expr_ir),
                    _ => panic!("Unsupported unary operator"),
                }
            }

            Expr::binop(left, op, right) => {
                let left_ir = left.to_llvm_ir();
                let right_ir = right.to_llvm_ir();
                let result_var = generate_temp_var();
                let result_type = get_expr_type(left);

                let operation = match op {
                    Token::plus => "add",
                    Token::minus => "sub",
                    Token::star => "mul",
                    Token::slash => "sdiv", // signed division
                    Token::percent => "srem", // modulus

                    Token::eq => "icmp eq",
                    Token::neq => "icmp ne",
                    Token::lt => "icmp slt",
                    Token::lte => "icmp sle",
                    Token::gt => "icmp sgt",
                    Token::gte => "icmp sge",

                    Token::logical_and => "and",
                    Token::logical_or => "or",

                    _ => panic!("Unsupported binary operator"),
                };

                format!("{} = {} {} {}, {}\n", result_var, operation, result_type, left_ir, right_ir)
            }
        }
    }
}

fn get_expr_type(expr: &Expr) -> &str {
    match expr {
        Expr::_int(_) => "i32",
        Expr::_float(_) => "float",
        Expr::_char(_) => "i8",
        _ => "i32",
    }
}

fn generate_temp_var() -> String {
    static mut COUNTER: i32 = 0;
    unsafe {
        COUNTER += 1;
        format!("%{}", COUNTER)
    }
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
        let var_type = match self.advance() {
            Some(Token::keyword(kw)) => kw,
            _ => return Err("Expected variable type".to_string()),
        };

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
        Ok(Stmt::VarDecl(var_type, name, init))
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