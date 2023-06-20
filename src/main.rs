use clap::Parser;

#[derive(Parser)]
#[command(version, about = "Yazik dlya NIRa")]
struct Args {
    filename: String,

    ///Print lexer output
    #[arg(short, long)]
    lexer_output: bool,
    ///Print parser output
    #[arg(short, long)]
    parser_output: bool,
}

fn main() {
    let args = Args::parse();

    let Ok(src) = std::fs::read_to_string(&args.filename) else {
        println!("Error: could not open file '{}'", args.filename);
        std::process::exit(1);
    };

    let tokens = yan::Lexer::lex(&src).unwrap_or_else(|e| {
        println!("Error: {e}");
        std::process::exit(1);
    });
    if args.lexer_output {
        println!("Lexer output:");
        for token in &tokens {
            println!("{token:?}");
        }
    }

    let stmts = yan::Parser::parse(tokens).unwrap_or_else(|e| {
        println!("Error: {e}");
        std::process::exit(1);
    });
    if args.parser_output {
        println!("Parser output:");
        for stmt in &stmts {
            println!("{stmt:#?}");
        }
    }

    yan::Evaluator::eval(&stmts).unwrap_or_else(|e| {
        println!("Error: {e}");
        std::process::exit(1);
    });
}
