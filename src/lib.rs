pub mod lexer;
pub use lexer::*;

pub mod parser;
pub use parser::*;

pub mod environment;
pub use environment::*;

pub mod evaluator;
pub use evaluator::*;

pub fn print_tokens(tokens: &[Token]) {
    println!("Lexer output:");

    for token in tokens {
        if *token.ttype() == TokenType::Endl {
            println!("{}", token.ttype());
            continue;
        }

        print!("{}, ", token.ttype());
    }
}

pub fn print_stmts(stmts: &[Stmt]) {
    println!("Parser output:");
    for stmt in stmts {
        print_stmt(stmt, 0);
    }
}

fn print_stmt(s: &Stmt, mut padding: usize) {
    print!("{}", " ".repeat(padding));
    padding += 4;

    use StmtType::*;
    match s.stype() {
        If {
            cond_and_code,
            else_code,
        } => {
            println!("If");
            for (cond, code) in cond_and_code {
                print!("{}", " ".repeat(padding - 4));
                println!("Condition:");
                print_expr(cond, padding);

                print!("{}", " ".repeat(padding - 4));
                println!("Then-code:");
                for stmt in code {
                    print_stmt(stmt, padding);
                }
            }

            if !else_code.is_empty() {
                print!("{}", " ".repeat(padding - 4));
                println!("Else-code:");
                for stmt in else_code {
                    print_stmt(stmt, padding);
                }
            }
        }
        While { cond, code } => {
            println!("While");

            print!("{}", " ".repeat(padding - 4));
            println!("Condition:");
            print_expr(cond, padding);

            print!("{}", " ".repeat(padding - 4));
            println!("Code:");
            for stmt in code {
                print_stmt(stmt, padding);
            }
        }
        FnDef { name, params, code } => {
            println!("Function definition({name})");

            print!("{}", " ".repeat(padding - 4));
            println!("Parameters:");
            for p in params {
                print!("{}", " ".repeat(padding));
                println!("{p}");
            }

            print!("{}", " ".repeat(padding - 4));
            println!("Code:");
            for stmt in code {
                print_stmt(stmt, padding);
            }
        }
        ExprStmt(e) => {
            println!("Expression");
            print_expr(e, padding);
        }
        Return(e) => {
            println!("Return");
            print_expr(e, padding);
        }
        Continue => {
            println!("Continue");
        }
        Break => {
            println!("Break");
        }
    }
}

fn print_expr(e: &Expr, mut padding: usize) {
    print!("{}", " ".repeat(padding));
    padding += 4;

    use ExprType::*;
    match e.etype() {
        Binary { lhs, op, rhs } => {
            println!("Binary({op})");
            print_expr(lhs, padding);
            print_expr(rhs, padding);
        }
        Unary { op, rhs } => {
            println!("Unary({op})");
            print_expr(rhs, padding);
        }
        FnCall { name, args } => {
            println!("Funciton call({name})");
            print!("{}", " ".repeat(padding - 4));
            println!("Arguments:");

            for arg in args {
                print_expr(arg, padding);
            }
        }
        BuiltinFnCall { name, args } => {
            println!("Builtin function call({name})");
            print!("{}", " ".repeat(padding - 4));
            println!("Arguments:");

            for arg in args {
                print_expr(arg, padding);
            }
        }
        Identifier { name } => {
            println!("Identifier({name})");
        }
        Number { value } => {
            println!("Number({value})");
        }
        Str { contents } => {
            println!("String('{contents}')");
        }
        Bool { value } => {
            println!("Boolean({value})");
        }
    }
}
