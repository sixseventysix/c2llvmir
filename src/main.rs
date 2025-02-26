use c2llvmir::{
    lexer::Lexer,
    parser::Parsec
};

fn main() {
    let code = r#"
        int main() {
            int x = 42 + 3;
            return 0;
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

    // println!("{}", program.to_llvm_ir());
}

