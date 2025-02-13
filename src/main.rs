use c2llvmir::lexer::Lexer;
use c2llvmir::parser::Parsec;

fn main() {
    let code = r#"
        int main() {
            int x = 42 + 3.14;
            char c = 'a';
            string s = "hello";
            if (x > 10 && x < 100) { x++; }
            return x;
        }
    "#;
    
    let mut lexer = Lexer::new(code);
    let tokens = lexer.tokenize();
    
    for token in &tokens {
        println!("{:?}", token);
    }
}

