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

    pub fn stype(&self) -> &StmtType {
        &self.stype
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
    ExprStmt(Expr),
    Return(Expr),
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct Expr {
    etype: ExprType,
    line_num: usize,
}

impl Expr {
    pub fn new(etype: ExprType, line_num: usize) -> Self {
        Self { etype, line_num }
    }

    pub fn etype(&self) -> &ExprType {
        &self.etype
    }

    pub fn line_num(&self) -> usize {
        self.line_num
    }

    fn from_tok(token: &Token) -> Self {
        use TokenType::*;
        let etype = match token.ttype().clone() {
            Number { value } => ExprType::Number { value },
            Str { contents } => ExprType::Str { contents },
            True => ExprType::Bool { value: true },
            False => ExprType::Bool { value: false },
            _ => unreachable!(),
        };

        Self::new(etype, token.line_num())
    }
}

#[derive(Debug, Clone)]
pub enum ExprType {
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

pub struct Parser {
    output: Vec<Stmt>,

    tokens: Vec<Token>,
    pos: usize,
}

macro_rules! unexp {
    ($line_num: expr, $token: expr) => {
        Err(format!(
            "line {}: unexpected token: `{}`",
            $line_num, $token
        ))
    };
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

    fn consume(&mut self, token: TokenType) -> Result<(), String> {
        let curr_tok = self.curr_tok();
        if token != *curr_tok.ttype() {
            return Err(format!(
                "line {}: expected: `{}`, got: `{}`",
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
            While => return self.while_stmt(),
            Fn => return self.fn_def(),
            Return => return self.ret_stmt(),
            Continue => return self.cont_stmt(),
            Break => return self.brk_stmt(),
            _ => (),
        }

        self.expr_stmt()
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
            *c_t.ttype() == Elif
        } {
            self.next();
            let elif_cond = self.expr(0)?;
            self.consume(Endl)?;
            branches.push((elif_cond, self.stmt_block(if_t.tab_lvl())?));
        }

        let mut else_code = vec![];
        if !self.is_done() && *self.curr_tok().ttype() == Else {
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

    fn while_stmt(&mut self) -> Result<Stmt, String> {
        let while_t = self.curr_tok();
        self.next(); // Едим `while`

        let cond = self.expr(0)?;
        self.consume(Endl)?;

        let code = self.stmt_block(while_t.tab_lvl())?;

        Ok(Stmt::new(
            StmtType::While { cond, code },
            while_t.line_num(),
        ))
    }

    fn fn_def(&mut self) -> Result<Stmt, String> {
        self.next(); // Едим `fn`

        let name_tok = self.curr_tok();
        let TokenType::Identifier {name} = name_tok.ttype() else {
            return Err(format!("line {}: expected function name, got: `{}`",
               name_tok.line_num(), name_tok.ttype()))
        };
        self.next();

        let params = self.params()?;
        self.consume(Endl)?;

        let code = self.stmt_block(name_tok.tab_lvl())?;

        Ok(Stmt::new(
            StmtType::FnDef {
                name: name.clone(),
                params,
                code,
            },
            name_tok.line_num(),
        ))
    }

    fn params(&mut self) -> Result<Vec<String>, String> {
        let mut out = vec![];
        self.consume(Lparen)?;

        if *self.curr_tok().ttype() == Rparen {
            self.next();
            return Ok(out);
        }

        loop {
            let c_t = self.curr_tok();

            let TokenType::Identifier { name } = c_t.ttype() else {
                return unexp!(c_t.line_num(), c_t.ttype())
            };
            self.next();
            out.push(name.clone());

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

    fn ret_stmt(&mut self) -> Result<Stmt, String> {
        let ret_t = self.curr_tok();
        self.next(); // Едим `return`

        let r = StmtType::Return(self.expr(0)?);
        self.consume(Endl)?;

        Ok(Stmt::new(r, ret_t.line_num()))
    }

    fn cont_stmt(&mut self) -> Result<Stmt, String> {
        let cont_t = self.curr_tok();
        self.next(); // Едим `continue`

        self.consume(Endl)?;

        Ok(Stmt::new(StmtType::Continue, cont_t.line_num()))
    }

    fn brk_stmt(&mut self) -> Result<Stmt, String> {
        let brk_t = self.curr_tok();
        self.next(); // Едим `break`

        self.consume(Endl)?;

        Ok(Stmt::new(StmtType::Break, brk_t.line_num()))
    }

    fn expr_stmt(&mut self) -> Result<Stmt, String> {
        let c_t = self.curr_tok();

        let e = StmtType::ExprStmt(self.expr(0)?);
        self.consume(Endl)?;

        Ok(Stmt::new(e, c_t.line_num()))
    }

    fn expr(&mut self, prec_lvl: usize) -> Result<Expr, String> {
        // Унарные операции
        let c_t = self.curr_tok();
        let mut lhs = match c_t.ttype() {
            TokenType::Identifier { name } => self.ident_expr(name)?,
            PrintL | Print | Sin | Cos => self.builtin_fn_call_expr(c_t)?,
            Lparen => self.paren_expr()?,
            TokenType::Number { value: _ }
            | TokenType::Str { contents: _ }
            | True
            | False => self.literal_expr(c_t)?,
            Minus | Not => self.unary_expr(c_t)?,
            u => return unexp!(c_t.line_num(), u),
        };

        // Бинарные операции (кроме `!`, выше его проверять неудобно)
        let mut c_ttype;
        while {
            c_ttype = self.curr_tok().ttype().clone();
            c_ttype.prec_lvl() >= prec_lvl
        } {
            match c_ttype {
                Equals => lhs = self.assign_expr(lhs)?,
                Plus | Minus | Mult | Div | Mod | Pow | EqEq | NotEq | Less | LessEq
                | Greater | GreaterEq | And | Or => lhs = self.binary_expr(lhs)?,
                Fact => lhs = self.fact_expr(lhs)?,
                _ => break,
            }
        }

        Ok(lhs)
    }

    fn ident_expr(&mut self, name: &str) -> Result<Expr, String> {
        self.next(); // Едим имя
        let c_t = self.curr_tok();

        Ok(match self.curr_tok().ttype() {
            Lparen => Expr::new(
                FnCall {
                    name: name.to_owned(),
                    args: self.args()?,
                },
                c_t.line_num(),
            ),
            _ => Expr::new(
                ExprType::Identifier {
                    name: name.to_owned(),
                },
                c_t.line_num(),
            ),
        })
    }

    fn args(&mut self) -> Result<Vec<Expr>, String> {
        self.consume(Lparen)?;
        let mut out = vec![];

        if *self.curr_tok().ttype() == Rparen {
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

    fn builtin_fn_call_expr(&mut self, name: Token) -> Result<Expr, String> {
        self.next(); // Едим имя

        Ok(Expr::new(
            BuiltinFnCall {
                name: name.ttype().clone(),
                args: self.args()?,
            },
            name.line_num(),
        ))
    }

    fn paren_expr(&mut self) -> Result<Expr, String> {
        self.next(); // Едим `(`

        let res = self.expr(0)?;
        self.consume(Rparen)?;

        Ok(res)
    }

    fn literal_expr(&mut self, value: Token) -> Result<Expr, String> {
        self.next(); // Едим значение

        Ok(Expr::from_tok(&value))
    }

    fn unary_expr(&mut self, op: Token) -> Result<Expr, String> {
        self.next(); // Едим оператор

        Ok(Expr::new(
            Unary {
                op: op.ttype().clone(),
                rhs: Box::new(self.expr(9000)?),
            },
            op.line_num(),
        ))
    }

    fn assign_expr(&mut self, lhs: Expr) -> Result<Expr, String> {
        let line_num = self.curr_tok().line_num();
        self.next(); // Едим `=`

        Ok(Expr::new(
            Binary {
                lhs: Box::new(lhs),
                op: Equals,
                // Не увеличиваем приоритет, чтобы корректно
                // работал синтаксис типа `a = b = 12`
                rhs: Box::new(self.expr(Equals.prec_lvl())?),
            },
            line_num,
        ))
    }

    fn binary_expr(&mut self, lhs: Expr) -> Result<Expr, String> {
        let op = self.curr_tok();
        self.next(); // Едим оператор

        Ok(Expr::new(
            Binary {
                lhs: Box::new(lhs),
                op: op.ttype().clone(),
                rhs: Box::new(self.expr(op.ttype().prec_lvl() + 1)?),
            },
            op.line_num(),
        ))
    }

    fn fact_expr(&mut self, lhs: Expr) -> Result<Expr, String> {
        let line_num = lhs.line_num();
        self.next(); // Едим `!`

        Ok(Expr::new(
            Unary {
                op: Fact,
                rhs: Box::new(lhs),
            },
            line_num,
        ))
    }
}
