use crate::token::Token;

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
    Op(BinOp),
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
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
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

    fn is_primitive_type(s: &str) -> bool {
        matches!(s, "int" | "float" | "char" | "void")
    }
    
    fn keyword_to_primitive(s: &str) -> PrimitiveType {
        match s {
            "int" => PrimitiveType::Int,
            "float" => PrimitiveType::Float,
            "char" => PrimitiveType::Char,
            "void" => PrimitiveType::Void,
            _ => panic!("Unknown primitive type"),
        }
    }

    fn parse_function(&mut self) -> Function {
        let return_type = match self.next() {
            Some(Token::Keyword(kw)) => match kw.as_str() {
                "int" => Type::Primitive(PrimitiveType::Int),
                "float" => Type::Primitive(PrimitiveType::Float),
                "char" => Type::Primitive(PrimitiveType::Char),
                "void" => Type::Primitive(PrimitiveType::Void),
                _ => panic!("Unknown return type keyword"),
            },
            _ => panic!("Expected return type keyword"),
        };

        let name = match self.next() {
            Some(Token::Identifier(name)) => name.clone(),
            _ => panic!("Expected function name"),
        };

        let params = self.parse_params();
        assert!(self.expect(&Token::Symbol('{')));

        let mut body = Vec::new();
        while let Some(token) = self.peek() {
            println!("parsing: {:?}", token);
            match token {
                Token::Keyword(_) => {
                    body.push(self.parse_stmt());
                }
                Token::Symbol('}') => break,
                _ => panic!("Unexpected token in function body: {:?}", token),
            }
        }        

        assert!(self.expect(&Token::Symbol('}')));

        Function {
            name,
            return_type,
            params,
            body,
        }
    }

    fn parse_params(&mut self) -> Vec<Parameter> {
        assert!(self.expect(&Token::Symbol('(')));
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
    
                    if self.expect(&Token::Symbol(',')) {
                        continue;
                    } else {
                        break;
                    }
                }
                Token::Symbol(')') => break,
                _ => panic!("Unexpected token in parameter list: {:?}", token),
            }
        }
    
        assert!(self.expect(&Token::Symbol(')')));
        params
    }    

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek() {
            Some(Token::Keyword(kw)) if Self::is_primitive_type(kw) => {
                self.parse_var_decl()
            }
            Some(Token::Keyword(k)) if k == "return" => {
                self.next(); // consume "return"
                let expr = self.parse_expr();
                self.expect(&Token::Symbol(';'));
                Stmt::Return(expr)
            }
            _ => panic!("Unknown statement"),
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

        let init = if self.expect(&Token::Symbol('=')) {
            Some(self.parse_expr())
        } else {
            None
        };

        self.expect(&Token::Symbol(';'));

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
                Token::Symbol('(') => {
                    paren_depth += 1;
                    expr_tokens.push(self.next().unwrap().clone());
                }
                Token::Symbol(')') => {
                    if paren_depth == 0 {
                        break;
                    } else {
                        paren_depth -= 1;
                        expr_tokens.push(self.next().unwrap().clone());
                    }
                }
                Token::Symbol(';') => {
                    break;
                }
                _ => {
                    expr_tokens.push(self.next().unwrap().clone());
                }
            }
        }
    
        Expr {
            seq: to_subexpr_postfix(&expr_tokens),
        }
    }      
}

fn to_subexpr_postfix(tokens: &[Token]) -> Vec<SubExpr> {
    let mut output = Vec::new();
    let mut op_stack = Vec::new();

    fn precedence(op: &Token) -> u8 {
        match op {
            Token::Symbol('+') | Token::Symbol('-') => 1,
            Token::Symbol('*') | Token::Symbol('/') => 2,
            _ => 0,
        }
    }

    fn token_to_binop(token: &Token) -> BinOp {
        match token {
            Token::Symbol('+') => BinOp::Add,
            Token::Symbol('-') => BinOp::Sub,
            Token::Symbol('*') => BinOp::Mul,
            Token::Symbol('/') => BinOp::Div,
            _ => panic!("Unknown operator"),
        }
    }

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
                // Check for function call
                if i + 1 < tokens.len() && tokens[i + 1] == Token::Symbol('(') {
                    // Find matching ')'
                    let mut j = i + 2;
                    let mut paren_depth = 1;
                    while j < tokens.len() {
                        match &tokens[j] {
                            Token::Symbol('(') => paren_depth += 1,
                            Token::Symbol(')') => {
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
                        output.extend(to_subexpr_postfix(&arg_tokens));
                    }

                    output.push(SubExpr::Call {
                        name: name.clone(),
                        arg_count: args_split.len(),
                    });

                    i = j + 1; // skip past ')'
                } else {
                    output.push(SubExpr::Variable(name.clone()));
                    i += 1;
                }
            }
            Token::Symbol('(') => {
                op_stack.push(tokens[i].clone());
                i += 1;
            }
            Token::Symbol(')') => {
                while let Some(top) = op_stack.pop() {
                    if top == Token::Symbol('(') {
                        break;
                    }
                    output.push(SubExpr::Op(token_to_binop(&top)));
                }
                i += 1;
            }
            Token::Symbol(op @ ('+' | '-' | '*' | '/')) => {
                while let Some(top) = op_stack.last() {
                    if matches!(top, Token::Symbol('+') | Token::Symbol('-') | Token::Symbol('*') | Token::Symbol('/'))
                        && precedence(top) >= precedence(&tokens[i])
                    {
                        output.push(SubExpr::Op(token_to_binop(&op_stack.pop().unwrap())));
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
        output.push(SubExpr::Op(token_to_binop(&op)));
    }

    output
}

fn split_args(tokens: &[Token]) -> Vec<Vec<Token>> {
    let mut args = Vec::new();
    let mut current = Vec::new();
    let mut paren_depth = 0;

    for token in tokens {
        match token {
            Token::Symbol('(') => {
                paren_depth += 1;
                current.push(token.clone());
            }
            Token::Symbol(')') => {
                paren_depth -= 1;
                current.push(token.clone());
            }
            Token::Symbol(',') if paren_depth == 0 => {
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