use crate::token::{Token, Keyword};

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Float,
    Char,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    // add support for structs, enums later
}

#[derive(Debug)]
pub struct Expr {
    seq: Vec<SubExpr>
}

#[derive(Debug, Clone)]
pub enum SubExpr {
    Number(i32),
    Char(char),
    Variable(String),
    BinOp(BinOp),
    UnOp(UnOp),
    Call {
        name: String,
        arg_count: usize,
    },
}

#[derive(Debug)]
pub enum Stmt {
    Return(Expr),
    VarDecl {
        name: String,
        var_type: Type,
        init: Option<Expr>
    },
    Assign {
        target: String,
        value: Expr,
    },
    ExprStmt(Expr),
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq, Ne,
    Lt, Le,
    Gt, Ge
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not,
    Neg
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<Parameter>,
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

    fn is_primitive_type(k: &Keyword) -> bool {
        matches!(k, Keyword::Int | Keyword::Float | Keyword::Char | Keyword::Void)
    }
    
    fn keyword_to_primitive(k: &Keyword) -> PrimitiveType {
        match k {
            Keyword::Int => PrimitiveType::Int,
            Keyword::Float => PrimitiveType::Float,
            Keyword::Char => PrimitiveType::Char,
            Keyword::Void => PrimitiveType::Void,
            _ => panic!("Invalid primitive type keyword"),
        }
    }    

    fn parse_function(&mut self) -> Function {
        let return_type = match self.next() {
            Some(Token::Keyword(kw)) => match kw {
                Keyword::Int => Type::Primitive(PrimitiveType::Int),
                Keyword::Float => Type::Primitive(PrimitiveType::Float),
                Keyword::Char => Type::Primitive(PrimitiveType::Char),
                Keyword::Void => Type::Primitive(PrimitiveType::Void),
                _ => panic!("Unknown return type keyword"),
            },
            _ => panic!("Expected return type keyword"),
        };

        let name = match self.next() {
            Some(Token::Identifier(name)) => name.clone(),
            _ => panic!("Expected function name"),
        };

        let params = self.parse_params();
        assert!(self.expect(&Token::LBrace));

        let mut body = Vec::new();
        while let Some(token) = self.peek() {
            println!("parsing: {:?}", token);
            match token {
                Token::Keyword(_) | Token::Identifier(_) => {
                    body.push(self.parse_stmt());
                }
                Token::RBrace => break,
                _ => panic!("Unexpected token in function body: {:?}", token),
            }
        }        

        assert!(self.expect(&Token::RBrace));

        Function {
            name,
            return_type,
            params,
            body,
        }
    }

    fn parse_params(&mut self) -> Vec<Parameter> {
        assert!(self.expect(&Token::LParen));
        let mut params = Vec::new();
    
        while let Some(token) = self.peek() {
            match token {
                Token::Keyword(kw) if Self::is_primitive_type(kw) => {
                    let param_type = Type::Primitive(Self::keyword_to_primitive(kw));
                    self.next(); // consume type keyword
    
                    let name = match self.next() {
                        Some(Token::Identifier(name)) => name.clone(),
                        _ => panic!("Expected parameter name"),
                    };
    
                    params.push(Parameter {
                        name,
                        param_type,
                    });
    
                    if self.expect(&Token::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }
                Token::RParen => break,
                _ => panic!("Unexpected token in parameter list: {:?}", token),
            }
        }
    
        assert!(self.expect(&Token::RParen));
        params
    }    

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek() {
            Some(Token::Keyword(kw)) if Self::is_primitive_type(kw) => {
                self.parse_var_decl()
            }
            Some(Token::Keyword(Keyword::Return)) => {
                self.next(); // consume "return"
                let expr = self.parse_expr();
                self.expect(&Token::Semicolon);
                Stmt::Return(expr)
            }
            Some(Token::Identifier(_)) => {
                self.parse_assignment_or_expr_stmt()
            }
            _ => panic!("Unknown statement"),
        }
    }

    fn parse_assignment_or_expr_stmt(&mut self) -> Stmt {
        let name = match self.next() {
            Some(Token::Identifier(id)) => id.clone(),
            _ => panic!("Expected identifier"),
        };
    
        match self.peek() {
            Some(Token::Eq) => {
                self.next(); // consume '='
                let value = self.parse_expr();
                self.expect(&Token::Semicolon);
                Stmt::Assign { target: name, value }
            }
            Some(Token::LParen) => {
                let mut tokens = vec![Token::Identifier(name)];
                while let Some(tok) = self.peek() {
                    if *tok == Token::Semicolon {
                        break;
                    }
                    tokens.push(self.next().unwrap().clone());
                }
                self.expect(&Token::Semicolon);
                let expr = Expr { seq: to_subexpr_postfix(&tokens) };
                Stmt::ExprStmt(expr)
            }
            _ => panic!("Expected '=' for assignment or '(' for function call after identifier"),
        }
    }
    
    fn parse_var_decl(&mut self) -> Stmt {
        let var_type = match self.next() {
            Some(Token::Keyword(kw)) => Type::Primitive(Self::keyword_to_primitive(kw)),
            _ => panic!("Expected type keyword"),
        };

        let name = match self.next() {
            Some(Token::Identifier(id)) => id.clone(),
            _ => panic!("Expected variable name"),
        };

        let init = if self.expect(&Token::Eq) {
            Some(self.parse_expr())
        } else {
            None
        };

        self.expect(&Token::Semicolon);

        Stmt::VarDecl {
            name,
            var_type,
            init,
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut expr_tokens = Vec::new();
        let mut paren_depth = 0;
    
        while let Some(token) = self.peek() {
            match token {
                Token::LParen => {
                    paren_depth += 1;
                    expr_tokens.push(self.next().unwrap().clone());
                }
                Token::RParen => {
                    if paren_depth == 0 {
                        break;
                    } else {
                        paren_depth -= 1;
                        expr_tokens.push(self.next().unwrap().clone());
                    }
                }
                Token::Semicolon => {
                    break;
                }
                _ => {
                    expr_tokens.push(self.next().unwrap().clone());
                }
            }
        }
        println!("{:?}", expr_tokens);
    
        Expr {
            seq: to_subexpr_postfix(&expr_tokens),
        }
    }      
}

fn precedence(token: &Token) -> u8 {
    match token {
        Token::Star | Token::Slash => 2,
        Token::Plus | Token::Minus => 1,
        Token::EqEq | Token::NotEq | Token::Lt | Token::LtEq | Token::Gt | Token::GtEq => 0,
        _ => 0,
    }
}

fn token_to_binop(token: &Token) -> BinOp {
    match token {
        Token::Plus => BinOp::Add,
        Token::Minus => BinOp::Sub,
        Token::Star => BinOp::Mul,
        Token::Slash => BinOp::Div,
        Token::EqEq => BinOp::Eq,
        Token::NotEq => BinOp::Ne,
        Token::Lt => BinOp::Lt,
        Token::LtEq => BinOp::Le,
        Token::Gt => BinOp::Gt,
        Token::GtEq => BinOp::Ge,
        _ => panic!("Unknown binary operator: {:?}", token),
    }
}

fn stack_expects_binop(op: &Token) -> Option<BinOp> {
    match op {
        Token::Plus => Some(BinOp::Add),
        Token::Minus => Some(BinOp::Sub),
        Token::Star => Some(BinOp::Mul),
        Token::Slash => Some(BinOp::Div),
        Token::EqEq => Some(BinOp::Eq),
        Token::NotEq => Some(BinOp::Ne),
        Token::Lt => Some(BinOp::Lt),
        Token::LtEq => Some(BinOp::Le),
        Token::Gt => Some(BinOp::Gt),
        Token::GtEq => Some(BinOp::Ge),
        _ => None,
    }
}


fn to_subexpr_postfix(tokens: &[Token]) -> Vec<SubExpr> {
    let mut output = Vec::new();
    let mut op_stack = Vec::new();

    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i] {
            Token::Number(n) => {
                output.push(SubExpr::Number(n.parse().unwrap()));
                i += 1;
            }

            Token::CharLiteral(c) => {
                output.push(SubExpr::Char(*c));
                i += 1;
            }

            Token::Identifier(name) => {
                if i + 1 < tokens.len() && tokens[i + 1] == Token::LParen {
                    let mut j = i + 2;
                    let mut paren_depth = 1;
                    while j < tokens.len() {
                        match &tokens[j] {
                            Token::LParen => paren_depth += 1,
                            Token::RParen => {
                                paren_depth -= 1;
                                if paren_depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                        j += 1;
                    }

                    let args_slice = &tokens[i + 2..j];
                    let args_split = split_args(args_slice);
                    for arg_tokens in &args_split {
                        output.extend(to_subexpr_postfix(arg_tokens));
                    }

                    output.push(SubExpr::Call {
                        name: name.clone(),
                        arg_count: args_split.len(),
                    });

                    i = j + 1;
                } else {
                    output.push(SubExpr::Variable(name.clone()));
                    i += 1;
                }
            }

            Token::Not => {
                i += 1;
                let mut rhs = to_subexpr_postfix(&tokens[i..]);
                output.append(&mut rhs);
                output.push(SubExpr::UnOp(UnOp::Not));
                break;
            }

            Token::Minus => {
                let is_unary = if i == 0 {
                    true
                } else {
                    matches!(
                        tokens.get(i - 1),
                        Some(
                            Token::Plus | Token::Minus | Token::Star | Token::Slash |
                            Token::Eq | Token::EqEq | Token::NotEq |
                            Token::Lt | Token::LtEq | Token::Gt | Token::GtEq |
                            Token::LParen | Token::Comma
                        )
                    )
                };

                if is_unary {
                    i += 1;
                    let mut rhs = to_subexpr_postfix(&tokens[i..]);
                    output.append(&mut rhs);
                    output.push(SubExpr::UnOp(UnOp::Neg));
                    break;
                } else {
                    while let Some(top) = op_stack.last() {
                        if precedence(top) >= precedence(&tokens[i]) {
                            output.push(SubExpr::BinOp(token_to_binop(&op_stack.pop().unwrap())));
                        } else {
                            break;
                        }
                    }
                    op_stack.push(tokens[i].clone());
                    i += 1;
                }
            }

            Token::LParen => {
                op_stack.push(tokens[i].clone());
                i += 1;
            }

            Token::RParen => {
                while let Some(top) = op_stack.pop() {
                    if top == Token::LParen {
                        break;
                    }
                    output.push(SubExpr::BinOp(token_to_binop(&top)));
                }
                i += 1;
            }

            Token::Plus | Token::Star | Token::Slash |
            Token::EqEq | Token::NotEq | Token::Lt | Token::LtEq | Token::Gt | Token::GtEq => {
                while let Some(top) = op_stack.last() {
                    if precedence(top) >= precedence(&tokens[i]) {
                        let op = op_stack.pop().unwrap();
                        if let Some(binop) = stack_expects_binop(&op) {
                            output.push(SubExpr::BinOp(binop));
                        }
                    } else {
                        break;
                    }
                }
                
                op_stack.push(tokens[i].clone());
                i += 1;
            }

            token => panic!("Invalid token in expression: {:?}", token),
        }
    }

    while let Some(op) = op_stack.pop() {
        if let Some(binop) = stack_expects_binop(&op) {
            output.push(SubExpr::BinOp(binop));
        }
    }    

    output
}

fn split_args(tokens: &[Token]) -> Vec<Vec<Token>> {
    let mut args = Vec::new();
    let mut current = Vec::new();
    let mut paren_depth = 0;

    for token in tokens {
        match token {
            Token::LParen => {
                paren_depth += 1;
                current.push(token.clone());
            }
            Token::RParen => {
                paren_depth -= 1;
                current.push(token.clone());
            }
            Token::Comma if paren_depth == 0 => {
                args.push(current);
                current = Vec::new();
            }
            _ => current.push(token.clone()),
        }
    }

    if !current.is_empty() {
        args.push(current);
    }

    args
}