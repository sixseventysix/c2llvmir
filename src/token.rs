#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Number(String),
    StringLiteral(String),
    CharLiteral(char),

    LParen, RParen,
    LBrace, RBrace,
    Semicolon, Comma,

    Plus, Minus, Star, Slash,
    Eq, EqEq,
    Not, NotEq,
    Lt, LtEq,
    Gt, GtEq,

    Whitespace,
    Newline,
    Unknown(char),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Int, Float, Char, Return, Void, If, Else,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c.is_whitespace() || c == '\n' {
            chars.next();
            continue;
        } else if c.is_alphabetic() || c == '_' {
            tokens.push(read_identifier_or_keyword(&mut chars));
        } else if c.is_ascii_digit() {
            tokens.push(read_number(&mut chars));
        } else if c == '"' {
            tokens.push(read_string_literal(&mut chars));
        } else if c == '\'' {
            tokens.push(read_char_literal(&mut chars));
        } else if is_symbol(c) {
            tokens.push(read_symbol(&mut chars));
        } else {
            tokens.push(read_unknown(&mut chars));
        }
    }

    tokens
}

fn read_identifier_or_keyword<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    let mut ident = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(c);
            chars.next();
        } else {
            break;
        }
    }

    match ident.as_str() {
        "int" => Token::Keyword(Keyword::Int),
        "float" => Token::Keyword(Keyword::Float),
        "char" => Token::Keyword(Keyword::Char),
        "return" => Token::Keyword(Keyword::Return),
        "void" => Token::Keyword(Keyword::Void),
        "if" => Token::Keyword(Keyword::If),
        "else" => Token::Keyword(Keyword::Else),
        _ => Token::Identifier(ident),
    }
    
}

fn read_number<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    let mut num = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            num.push(c);
            chars.next();
        } else {
            break;
        }
    }
    Token::Number(num)
}

fn read_string_literal<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    chars.next(); // consume opening "
    let mut literal = String::new();
    while let Some(&c) = chars.peek() {
        if c == '"' {
            chars.next(); // consume closing "
            break;
        } else {
            literal.push(c);
            chars.next();
        }
    }
    Token::StringLiteral(literal)
}

fn read_char_literal<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    chars.next(); // consume opening '
    let ch = match chars.next() {
        Some('\\') => match chars.next() {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('0') => '\0',
            Some('\'') => '\'',
            Some('\"') => '\"',
            Some('\\') => '\\',
            Some(other) => other,
            None => return Token::Unknown('\\'),
        },
        Some(c) => c,
        None => return Token::Unknown('\''),
    };

    if chars.next() == Some('\'') {
        Token::CharLiteral(ch)
    } else {
        Token::Unknown(ch)
    }
}

fn is_symbol(c: char) -> bool {
    matches!(c, '{' | '}' | '(' | ')' | ';' | ',' | '=' | '<' | '>' | '+' | '-' | '*' | '/')
}

fn read_symbol<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    match chars.next().unwrap() {
        '(' => Token::LParen,
        ')' => Token::RParen,
        '{' => Token::LBrace,
        '}' => Token::RBrace,
        ';' => Token::Semicolon,
        ',' => Token::Comma,
        '+' => Token::Plus,
        '-' => Token::Minus,
        '*' => Token::Star,
        '/' => Token::Slash,
        '=' => {
            if chars.peek() == Some(&'=') {
                chars.next();
                Token::EqEq
            } else {
                Token::Eq
            }
        }
        '!' => {
            if chars.peek() == Some(&'=') {
                chars.next();
                Token::NotEq
            } else {
                Token::Not
            }
        }
        '<' => {
            if chars.peek() == Some(&'=') {
                chars.next();
                Token::LtEq
            } else {
                Token::Lt
            }
        }
        '>' => {
            if chars.peek() == Some(&'=') {
                chars.next();
                Token::GtEq
            } else {
                Token::Gt
            }
        }
        other => Token::Unknown(other),
    }
}

fn read_unknown<I: Iterator<Item = char>>(chars: &mut std::iter::Peekable<I>) -> Token {
    let c = chars.next().unwrap();
    Token::Unknown(c)
}
