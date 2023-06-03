use crate::lexer::*;

#[derive(Debug)]
pub struct Stmt {
    stype: StmtType,
    line_num: usize,
}

impl Stmt {
    pub fn new(stype: StmtType, line_num: usize) -> Self {
        Self { stype, line_num }
    }
}

#[derive(Debug)]
pub enum StmtType {
    If {
        cond_and_code: Vec<(Expr, Vec<StmtType>)>,
        else_code: Vec<StmtType>,
    },
    While {
        cond: Expr,
        code: Vec<StmtType>,
    },
    FnDef {
        name: String,
        params: Vec<String>,
        code: Vec<StmtType>,
    },
    ExrpStmt(Expr),
    Return(Expr),
    Continue,
    Break,
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        lhs: Box<Expr>,
        op: TokenType,
        rhs: Box<Expr>,
    },
    Unary {
        op: TokenType,
        rhs: Box<Expr>,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    BuiltinFnCall {
        name: TokenType,
        args: Vec<Expr>,
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

impl Expr {
    fn from_tok(token: Token) -> Self {
        use TokenType::*;
        match token.ttype() {
            Identifier { name } => Expr::Identifier { name },
            Number { value } => Expr::Number { value },
            Str { contents } => Expr::Str { contents },
            True => Expr::Bool { value: true },
            False => Expr::Bool { value: false },
            _ => unreachable!(),
        }
    }
}

pub struct Parser {
    output: Vec<Stmt>,

    tokens: Vec<Token>,
    pos: usize,
}

use Expr::*;
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
        let line_num = self.curr_tok().line_num();
        let e = StmtType::ExrpStmt(self.expr(0)?);
        self.consume(Endl)?;
        Ok(Stmt::new(e, line_num))
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
                lhs = Unary {
                    op: Minus,
                    rhs: Box::new(self.expr(9000)?),
                };
            }
            Not => {
                self.next();
                lhs = Unary {
                    op: Not,
                    rhs: Box::new(self.expr(9000)?),
                };
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

    fn args(&mut self) -> Result<Vec<Expr>, String> {
        self.next();
        let out = vec![];

        while {
            let t = self.curr_tok().ttype();
            t != Rparen || t != Endl
        } {}
        self.consume(Rparen)?;

        Ok(out)
    }
}
