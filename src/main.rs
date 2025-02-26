use c2llvmir::{
    lexer::Lexer,
    parser::Parsec
};
use std::fs;
use std::io::{self, Read};

fn main() -> io::Result<()>{
    let filename = "data/a.c";
    let code = fs::read_to_string(filename)?;
    
    let mut lexer = Lexer::new(&code);
    let tokens = lexer.tokenize();
    
    for token in &tokens {
        println!("{:?}", token);
    }

    let mut p = Parsec::new(tokens);
    let program = p.parse_program();
    println!("{:?}", program);

    // println!("{}", program.to_llvm_ir());

    Ok(())
}

