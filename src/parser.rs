#![allow(non_camel_case_types)]
use crate::lexer::Token;

#[derive(Debug, Clone)]
enum PrimitiveType {
    _int,
    _float,
    _char,
    _void
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>
}

#[derive(Debug, Clone)]
struct Function {
    name: String, 
    ret_type: PrimitiveType,
    params: Vec<(PrimitiveType, String)>,
    body_block: Vec<Statement>
}

#[derive(Debug, Clone)]
enum Statement {
    VarDecl(PrimitiveType, String, Expression),  // (type, name, expr)
    Assignment(String, Expression),
    Return(Expression),
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    plus, minus, star, slash, percent, 
    logical_and, logical_or,

    exclamation, shiftl, shiftr, incr, decr,
}

#[derive(Debug, Clone)]
enum ExpressionAtomic {
    _int(i32),
    _float(f32),
    _char(char),
    Var(String)
}

#[derive(Debug, Clone)]
enum SubExpression {
    Binop(ExpressionAtomic, Operator, ExpressionAtomic),
    Unop(Operator, ExpressionAtomic),
    Atomic(ExpressionAtomic)
}

#[derive(Debug, Clone)]
struct Expression {
    sub_exprs: Vec<SubExpression>
}

#[derive(Debug, Clone)]
enum PostfixItem {
    Atomic(ExpressionAtomic),
    Op(Operator),
}

#[derive(Debug, Clone)]
enum ParsecErrT {
    NotPrimitive,
    NotIdentifier,
    NotOperator,
    Unexpected,
    EOF_e
}

#[derive(Debug, Clone)]
pub struct ParsecErr {
    ty: ParsecErrT,
    erroneous_token: Token
}

#[derive(Debug)]
pub struct Parsec {
    tokens: Vec<Token>,
    cursor: usize
}

impl Parsec {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens, 
            cursor: 0
        }
    }

    #[inline]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    #[inline]
    fn next(&mut self) -> Token {
        let t = self.tokens.get(self.cursor).unwrap().clone();
        self.cursor += 1;
        t
    }

    fn expect(&mut self, expected: Token) -> Result<Token, ParsecErr> {
        match self.peek() {
            Some(token) if *token == expected => {
                Ok(self.next())
            },
            Some(token) => Err(ParsecErr {
                ty: ParsecErrT::Unexpected,
                erroneous_token: token.clone(),
            }),
            None => Err(ParsecErr {
                ty: ParsecErrT::EOF_e,
                erroneous_token: Token::eof,
            }),
        }
    }

    fn is_part_of_expr(&self) -> bool {
        match self.peek().unwrap().clone() {
            Token::plus| Token::minus| Token::star| Token::slash| Token::percent|
            Token::assign| Token::logical_and| Token::logical_or| Token::lparen| Token::rparen|
            Token::exclamation| Token::shiftl| Token::shiftr| Token::incr| Token::decr|
            Token::_int(_)| Token::_float(_)| Token::_char(_)| Token::ident(_) => true,
            _ => false
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParsecErr> {
        let mut functions = Vec::new();
        while self.peek().is_some() {
            functions.push(self.parse_function()?);
        }
        Ok(Program { functions })
    }

    fn parse_function(&mut self) -> Result<Function, ParsecErr> {
        println!("parsing function: {:?}", self.peek());
        let ret_type = self.parse_primitive()?;
        let name = self.parse_identifier()?;
        self.expect(Token::lparen)?;
        let params = self.parse_parameters()?;
        self.expect(Token::lbrace)?;
        let body_block = self.parse_block()?;
        
        Ok(Function {
            name,
            ret_type,
            params,
            body_block,
        })
    }

    fn parse_primitive(&mut self) -> Result<PrimitiveType, ParsecErr> {
        match self.next() {
            Token::int_t => Ok(PrimitiveType::_int),
            Token::float_t => Ok(PrimitiveType::_float),
            Token::char_t => Ok(PrimitiveType::_char),
            _ => Err(ParsecErr { ty: ParsecErrT::NotPrimitive, erroneous_token: self.peek().unwrap().clone() })
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParsecErr> {
        let t = self.peek().cloned();
        match t {
            Some(Token::ident(name)) => {
                self.next();
                Ok(name)
            }
            Some(other) => Err(ParsecErr { ty: ParsecErrT::NotIdentifier, erroneous_token: other }),
            None => Err(ParsecErr { ty: ParsecErrT::EOF_e, erroneous_token: Token::eof }),
        }
    }

    fn parse_parameters(&mut self) -> Result<Vec<(PrimitiveType, String)>, ParsecErr> {
        println!("parsing parameters: {:?}", self.peek());
        let mut params = Vec::new();
        
        loop {
            if matches!(self.next(), Token::rparen) {
                break;
            }
            let param_type = self.parse_primitive()?;
            let param_name = self.parse_identifier()?;
            params.push((param_type, param_name));

            if !matches!(self.next(), Token::comma) || self.peek().is_none() {
                break;
            }
        }
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Vec<Statement>, ParsecErr> {
        println!("parsing block: {:?}", self.peek());
        let mut statements = Vec::new();
        loop {
            if matches!(self.next(), Token::rbrace) {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParsecErr> {
        println!("parsing statement: {:?}", self.peek());
        let token = self.peek().cloned().ok_or(ParsecErr {
            ty: ParsecErrT::EOF_e,
            erroneous_token: Token::eof,
        })?;
    
        match token {
            Token::int_t | Token::float_t | Token::char_t => {
                let prim_type = self.parse_primitive()?;
                let name = self.parse_identifier()?;
                self.expect(Token::assign)?;
                let expr = self.parse_expression()?;
                Ok(Statement::VarDecl(prim_type, name, expr))
            }
            Token::_return => {
                self.next();
                let expr = self.parse_expression()?;
                self.expect(Token::semicolon)?;
                Ok(Statement::Return(expr))
            }
            Token::ident(_) => {
                let name = self.parse_identifier()?;
                self.expect(Token::assign)?;
                let expr = self.parse_expression()?;
                Ok(Statement::Assignment(name, expr))
            }
            _ => Err(ParsecErr { 
                ty: ParsecErrT::Unexpected, 
                erroneous_token: token 
            }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParsecErr> {
        let mut expr_tokens: Vec<Token> = Vec::new();
        while self.is_part_of_expr() {
            let token = self.next();
            expr_tokens.push(token);
        }
        let postfix_items = infix_to_postfix(&expr_tokens);
        let sub_exprs = postfix_to_subexprs(&postfix_items);
        
        Ok(Expression { sub_exprs })
    }
}

fn token_to_operator(token: &Token) -> Operator {
    match token {
        Token::plus       => Operator::plus,
        Token::minus      => Operator::minus,
        Token::star       => Operator::star,
        Token::slash      => Operator::slash,
        Token::percent    => Operator::percent,
        Token::logical_and=> Operator::logical_and,
        Token::logical_or => Operator::logical_or,
        Token::exclamation=> Operator::exclamation,
        Token::shiftl     => Operator::shiftl,
        Token::shiftr     => Operator::shiftr,
        Token::incr       => Operator::incr,
        Token::decr       => Operator::decr,
        _ => panic!("Not an operator token"),
    }
}

fn infix_to_postfix(tokens: &[Token]) -> Vec<PostfixItem> {
    let mut output: Vec<PostfixItem> = Vec::new();
    let mut op_stack: Vec<Token> = Vec::new();

    fn precedence(token: &Token) -> i32 {
        match token {
            Token::plus | Token::minus                => 2,
            Token::star | Token::slash | Token::percent   => 3,
            Token::logical_and                         => 1,
            Token::logical_or                          => 0,
            Token::exclamation | Token::incr | Token::decr => 4,
            Token::shiftl | Token::shiftr               => 4,
            _ => -1,
        }
    }

    fn is_operator(token: &Token) -> bool {
        matches!(token, Token::plus | Token::minus | Token::star | Token::slash | Token::percent |
                      Token::logical_and | Token::logical_or | Token::exclamation | Token::shiftl |
                      Token::shiftr | Token::incr | Token::decr)
    }

    for token in tokens {
        match token {
            Token::_int(n)    => output.push(PostfixItem::Atomic(ExpressionAtomic::_int(*n))),
            Token::_float(f)  => output.push(PostfixItem::Atomic(ExpressionAtomic::_float(*f))),
            Token::_char(c)   => output.push(PostfixItem::Atomic(ExpressionAtomic::_char(*c))),
            Token::ident(s)   => output.push(PostfixItem::Atomic(ExpressionAtomic::Var(s.clone()))),
            Token::lparen     => op_stack.push(token.clone()),
            Token::rparen     => {
                while let Some(top) = op_stack.pop() {
                    if let Token::lparen = top {
                        break;
                    } else {
                        output.push(PostfixItem::Op(token_to_operator(&top)));
                    }
                }
            },
            t if is_operator(t) => {
                while let Some(top) = op_stack.last() {
                    if is_operator(top) && precedence(token) <= precedence(top) {
                        output.push(PostfixItem::Op(token_to_operator(&op_stack.pop().unwrap())));
                    } else {
                        break;
                    }
                }
                op_stack.push(token.clone());
            },
            _ => {}
        }
    }
    while let Some(tok) = op_stack.pop() {
        if is_operator(&tok) {
            output.push(PostfixItem::Op(token_to_operator(&tok)));
        }
    }
    output
}

fn postfix_to_subexprs(items: &[PostfixItem]) -> Vec<SubExpression> {
    let mut stack: Vec<ExpressionAtomic> = Vec::new();
    let mut sub_exprs: Vec<SubExpression> = Vec::new();
    let mut temp_counter = 0;

    let mut new_temp = || -> ExpressionAtomic {
        let name = format!("t{}", temp_counter);
        temp_counter += 1;
        ExpressionAtomic::Var(name)
    };

    fn is_unary(op: &Operator) -> bool {
        matches!(op, Operator::exclamation | Operator::incr | Operator::decr)
    }

    for item in items {
        match item {
            PostfixItem::Atomic(atom) => stack.push(atom.clone()),
            PostfixItem::Op(op) => {
                if is_unary(op) {
                    let operand = stack.pop().expect("Insufficient operand for unary operator");
                    sub_exprs.push(SubExpression::Unop(*op, operand));
                    stack.push(new_temp());
                } else {
                    let right = stack.pop().expect("Insufficient operands for binary operator");
                    let left = stack.pop().expect("Insufficient operands for binary operator");
                    sub_exprs.push(SubExpression::Binop(left, *op, right));
                    stack.push(new_temp());
                }
            }
        }
    }

    if sub_exprs.is_empty() && stack.len() == 1 {
        sub_exprs.push(SubExpression::Atomic(stack.pop().unwrap()));
    }
    sub_exprs
}