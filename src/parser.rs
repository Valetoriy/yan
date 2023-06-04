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
        // Унарные операции
        let mut lhs;
        let c_t = self.curr_tok();
        match c_t.ttype() {
            TokenType::Identifier { name } => {
                self.next();
                match self.curr_tok().ttype() {
                    Lparen => {
                        lhs = FnCall {
                            name,
                            args: self.args()?,
                        }
                    }
                    _ => lhs = Expr::from_tok(c_t),
                }
            }
            Print | Sin | Cos => {
                self.next();
                lhs = BuiltinFnCall {
                    name: c_t.ttype(),
                    args: self.args()?,
                }
            }
            Lparen => {
                self.next();
                lhs = self.expr(0)?;
                self.consume(Rparen)?;
            }
            TokenType::Number { value: _ }
            | TokenType::Str { contents: _ }
            | True
            | False => {
                self.next();
                lhs = Expr::from_tok(c_t);
            }
            Minus | Not => {
                self.next();
                lhs = Unary {
                    op: c_t.ttype(),
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

        // Бинарные операции (кроме `!`, выше его проверять неудобно)
        let mut c_ttype;
        while {
            c_ttype = self.curr_tok().ttype();
            c_ttype.prec_lvl() >= prec_lvl
        } {
            match c_ttype {
                Equals | Plus | Minus | Mult | Div | Mod | Pow | EqEq | NotEq | Less
                | LessEq | Greater | GreaterEq | And | Or => {
                    self.next();
                    lhs = Binary {
                        lhs: Box::new(lhs),
                        op: c_ttype.clone(),
                        rhs: Box::new(self.expr(c_ttype.prec_lvl() + 1)?),
                    };
                }
                Fact => {
                    self.next();
                    lhs = Unary {
                        op: Fact,
                        rhs: Box::new(lhs),
                    };
                }
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn args(&mut self) -> Result<Vec<Expr>, String> {
        self.next(); // Едим `(`
        let mut out = vec![];

        if self.curr_tok().ttype() == Rparen {
            self.next();
            return Ok(out);
        }

        loop {
            out.push(self.expr(0)?);

            let c_t = self.curr_tok();
            match c_t.ttype() {
                Rparen => break,
                Comma => {
                    self.next();
                    continue;
                }
                u => {
                    return Err(format!(
                        "line {}: unexpected token: {:?}",
                        c_t.line_num(),
                        u
                    ))
                }
            }
        }
        self.next(); // Едим `)`

        Ok(out)
    }
}
