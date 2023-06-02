use std::println;

use clap::Parser;

#[derive(Parser)]
#[command(version, about = "Yazik dlya Nira")]
struct Args {
    filename: String,
}

use yan::lexer::*;

fn main() {
    let args = Args::parse();

    let Ok(src) = std::fs::read_to_string(&args.filename) else {
        println!("Error: could not open file '{}'", args.filename);
        std::process::exit(1);
    };

    let tokens = Lexer::lex(&src).unwrap_or_else(|e| {
        println!("Error: {e}");
        std::process::exit(1);
    });

    for token in tokens {
        println!("{token:?}");
    }
}
