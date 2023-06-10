use crate::lexer::*;

#[derive(Debug, Clone)]
pub struct Stmt {
    stype: StmtType,
    line_num: usize,
}

impl Stmt {
    pub fn new(stype: StmtType, line_num: usize) -> Self {
        Self { stype, line_num }
    }

    pub fn stype(&self) -> StmtType {
        self.stype.clone()
    }

    pub fn line_num(&self) -> usize {
        self.line_num
    }
}

#[derive(Debug, Clone)]
pub enum StmtType {
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

#[derive(Debug, Clone)]
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

macro_rules! unexp {
    ($line_num: expr, $token: expr) => {
        Err(format!(
            "line {}: unexpected token: {:?}",
            $line_num, $token
        ))
    };
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
            let c_t = p.curr_tok();
            if c_t.tab_lvl() != 0 {
                return Err(format!("line: {}, unexpected indent", c_t.line_num()));
            }
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
        let c_t = self.curr_tok();
        match c_t.ttype() {
            If => return self.if_stmt(),
            While => {
                self.next();
                let cond = self.expr(0)?;
                self.consume(Endl)?;
                let code = self.stmt_block(c_t.tab_lvl())?;
                return Ok(Stmt::new(StmtType::While { cond, code }, c_t.line_num()));
            }
            Fn => return self.fn_def(),
            Return => {
                self.next();
                let r = StmtType::Return(self.expr(0)?);
                self.consume(Endl)?;
                return Ok(Stmt::new(r, c_t.line_num()));
            }
            Continue => {
                self.next();
                self.consume(Endl)?;
                return Ok(Stmt::new(StmtType::Continue, c_t.line_num()));
            }
            Break => {
                self.next();
                self.consume(Endl)?;
                return Ok(Stmt::new(StmtType::Break, c_t.line_num()));
            }
            _ => (),
        }

        let e = StmtType::ExrpStmt(self.expr(0)?);
        self.consume(Endl)?;
        Ok(Stmt::new(e, c_t.line_num()))
    }

    fn if_stmt(&mut self) -> Result<Stmt, String> {
        let mut branches = vec![];
        let if_t = self.curr_tok();
        self.next(); // Едим `if`

        let if_cond = self.expr(0)?;
        self.consume(Endl)?;
        let if_code = self.stmt_block(if_t.tab_lvl())?;
        branches.push((if_cond, if_code));

        let mut c_t;
        while 'w: {
            if self.is_done() {
                break 'w false;
            }

            c_t = self.curr_tok();
            c_t.ttype() == Elif
        } {
            self.next();
            let elif_cond = self.expr(0)?;
            self.consume(Endl)?;
            branches.push((elif_cond, self.stmt_block(if_t.tab_lvl())?));
        }

        let mut else_code = vec![];
        if !self.is_done() && self.curr_tok().ttype() == Else {
            self.next();
            self.consume(Endl)?;
            else_code = self.stmt_block(if_t.tab_lvl())?;
        }

        Ok(Stmt::new(
            StmtType::If {
                cond_and_code: branches,
                else_code,
            },
            if_t.line_num(),
        ))
    }

    fn fn_def(&mut self) -> Result<Stmt, String> {
        self.next(); // Едим `fn`

        let name_tok = self.curr_tok();
        let TokenType::Identifier {name} = name_tok.ttype() else {
                    return Err(format!("line {}: expected function name, got: {:?}",
                                       name_tok.line_num(), name_tok))
                };
        self.next();

        let params = self.params()?;
        self.consume(Endl)?;

        let code = self.stmt_block(name_tok.tab_lvl())?;

        return Ok(Stmt::new(
            StmtType::FnDef { name, params, code },
            name_tok.line_num(),
        ));
    }

    fn params(&mut self) -> Result<Vec<String>, String> {
        let mut out = vec![];
        self.consume(Lparen)?;

        if self.curr_tok().ttype() == Rparen {
            self.next();
            return Ok(out);
        }

        loop {
            let c_t = self.curr_tok();

            let TokenType::Identifier { name } = c_t.ttype() else {
                return unexp!(c_t.line_num(), c_t.ttype())
            };
            self.next();
            out.push(name);

            let c_t = self.curr_tok();
            match c_t.ttype() {
                Comma => {
                    self.next();
                    continue;
                }
                Rparen => break,
                u => return unexp!(c_t.line_num(), u),
            }
        }

        self.consume(Rparen)?;
        Ok(out)
    }

    fn stmt_block(&mut self, tab_lvl: usize) -> Result<Vec<Stmt>, String> {
        let mut out = vec![];
        let mut block_tab_lvl = 0;

        while !self.is_done() && self.curr_tok().tab_lvl() > tab_lvl {
            let c_t = self.curr_tok();

            if block_tab_lvl == 0 {
                block_tab_lvl = c_t.tab_lvl();
            } else if c_t.tab_lvl() != block_tab_lvl {
                return Err(format!("line: {}, unexpected indent", c_t.line_num()));
            }

            out.push(self.stmt()?);
        }

        Ok(out)
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
            u => return unexp!(c_t.line_num(), u),
        }

        // Бинарные операции (кроме `!`, выше его проверять неудобно)
        let mut c_ttype;
        while {
            c_ttype = self.curr_tok().ttype();
            c_ttype.prec_lvl() >= prec_lvl
        } {
            match c_ttype {
                Equals => {
                    self.next();
                    lhs = Binary {
                        lhs: Box::new(lhs),
                        op: Equals,
                        // Не увеличиваем приоритет, чтобы корректно
                        // работал синтаксис типа `a = b = 12`
                        rhs: Box::new(self.expr(Equals.prec_lvl())?),
                    };
                }
                Plus | Minus | Mult | Div | Mod | Pow | EqEq | NotEq | Less | LessEq
                | Greater | GreaterEq | And | Or => {
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
                u => return unexp!(c_t.line_num(), u),
            }
        }
        self.next(); // Едим `)`

        Ok(out)
    }
}
