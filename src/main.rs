use c2llvmir::{
    lexer::Lexer,
    parser::Parsec
};

fn main() {
    let code = r#"
        int main() {
            int x = 42 + 3.14;
            char c = 'a';
            if (x > 10 && x < 100) { x++; }
            else { x--; }
            return x;
        }
    "#;
    
    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize();
    
    for token in &tokens {
        println!("{:?}", token);
    }

    let mut p = Parsec::new(tokens);
    let program = p.parse_program();
    println!("{:?}", program);
}

