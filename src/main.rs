use std::{io, fs};
use c2llvmir::token::{Token, tokenize};
use c2llvmir::parsec::{Parsec, Program};

fn main() -> io::Result<()>{
    // file string to TokenStream
    // TokenStream to Program { constants, functions { name, param_count, params, expressions { subexprs } } }
    // Program to LLVM IR
    let path = "data/a.c";
    let content = fs::read_to_string(path)?;
    let tokens = tokenize(&content);
    for token in &tokens {
        println!("{:?}", token);
    }
    let mut parsec = Parsec::new(tokens);
    let p = parsec.parse_program();
    println!("{:?}", p);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
}

