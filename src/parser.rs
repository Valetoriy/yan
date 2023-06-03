use crate::lexer::*;

#[derive(Debug)]
pub enum Stmt {
    If {
        cond_and_code: Vec<(Expr, Vec<Stmt>)>,
        else_code: Vec<Stmt>,
    },
    While {
        cond: Expr,
        code: Vec<Stmt>,
    },
    FnDef {
        name: String,
        params: Vec<String>,
        code: Vec<Stmt>,
    },
    ExrpStmt(Expr),
    Return(Expr),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct Expr {
    etype: ExprType,
    line_num: usize,
}

impl Expr {
    pub fn new(etype: ExprType, line_num: usize) -> Self {
        Self { etype, line_num }
    }

    fn from_tok(token: Token) -> Self {
        use TokenType::*;
        let etype = match token.ttype() {
            Identifier { name } => ExprType::Identifier { name },
            Number { value } => ExprType::Number { value },
            Str { contents } => ExprType::Str { contents },
            True => ExprType::Bool { value: true },
            False => ExprType::Bool { value: false },
            _ => unreachable!(),
        };

        Expr {
            etype,
            line_num: token.line_num(),
        }
    }
}

#[derive(Debug)]
pub enum ExprType {
    Binary {
        lhs: Box<ExprType>,
        op: TokenType,
        rhs: Box<ExprType>,
    },
    Unary {
        op: TokenType,
        rhs: Box<ExprType>,
    },
    FnCall {
        name: String,
        args: Vec<ExprType>,
    },
    BuiltinFnCall {
        name: TokenType,
        args: Vec<ExprType>,
    },
    Identifier {
        name: String,
    },
    Number {
        value: f64,
    },
    Str {
        contents: String,
    },
    Bool {
        value: bool,
    },
}

pub struct Parser {
    output: Vec<Stmt>,

    tokens: Vec<Token>,
    pos: usize,
}

use ExprType::*;
use TokenType::*;
impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, String> {
        let mut p = Self {
            output: vec![],
            tokens,
            pos: 0,
        };

        while !p.is_done() {
            let res = p.stmt()?;
            p.output.push(res);
        }
        Ok(p.output)
    }

    fn is_done(&self) -> bool {
        self.pos == self.tokens.len()
    }

    fn next(&mut self) {
        self.pos += 1;
    }

    fn curr_tok(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    // TODO(spff): impl std::fmt::Display for TokenType
    fn consume(&mut self, token: TokenType) -> Result<(), String> {
        let curr_tok = self.curr_tok();
        if token != curr_tok.ttype() {
            return Err(format!(
                "line {}: expected: {:?}, got: {:?}",
                curr_tok.line_num(),
                token,
                curr_tok.ttype()
            ));
        }

        self.next();
        Ok(())
    }

    fn stmt(&mut self) -> Result<Stmt, String> {
        let e = Stmt::ExrpStmt(self.expr(0)?);
        self.consume(Endl)?;
        Ok(e)
    }

    fn expr(&mut self, prec_lvl: usize) -> Result<Expr, String> {
        let lhs;
        let c_t = self.curr_tok();
        match c_t.ttype() {
            TokenType::Identifier { name } => {
                self.next();
                match self.curr_tok().ttype() {
                    // Lparen => {
                    // }
                    _ => lhs = Expr::from_tok(c_t),
                }
            }
            TokenType::Number { value: _ }
            | TokenType::Str { contents: _ }
            | True
            | False => {
                self.next();
                lhs = Expr::from_tok(c_t);
            }
            Minus => {
                self.next();
                lhs = Expr::new(
                    Unary {
                        op: Minus,
                        rhs: Box::new(self.expr(9000)?.etype),
                    },
                    c_t.line_num(),
                );
            }
            Not => {
                self.next();
                lhs = Expr::new(
                    Unary {
                        op: Not,
                        rhs: Box::new(self.expr(9000)?.etype),
                    },
                    c_t.line_num(),
                );
            }
            u => {
                return Err(format!(
                    "line {}: unexpected token: {:?}",
                    c_t.line_num(),
                    u
                ))
            }
        }

        Ok(lhs)
    }

    fn args(&mut self) -> Vec<Expr> {
        self.next();
        todo!()
    }
}
