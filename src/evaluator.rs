use crate::{Environment, Expr, ExprType, RuntimeValue, Stmt, StmtType, TokenType};

pub struct StmtRes {
    srtype: StmtResType,
    line_num: usize,
}

impl StmtRes {
    pub fn new(srtype: StmtResType, line_num: usize) -> Self {
        Self { srtype, line_num }
    }
}

#[derive(Debug)]
pub enum StmtResType {
    Ok,
    Return(RuntimeValue),
    Continue,
    Break,
}

impl std::fmt::Display for StmtResType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtResType::Return(_) => write!(f, "`return` keyword"),
            StmtResType::Continue => write!(f, "`continue` keyword"),
            StmtResType::Break => write!(f, "`break` keyword"),
            _ => unreachable!(),
        }
    }
}

pub struct Evaluator {
    env_stack: Vec<Environment>,
}

impl Evaluator {
    pub fn eval(stmts: &[Stmt]) -> Result<(), String> {
        let mut e = Evaluator { env_stack: vec![] };

        let res = e.eval_block(stmts)?;
        match &res.srtype {
            StmtResType::Ok => (),
            u => return Err(format!("line {}: {u} in global scope", res.line_num)),
        }

        Ok(())
    }

    fn top_env(&mut self) -> &mut Environment {
        self.env_stack.last_mut().unwrap()
    }

    fn eval_block(&mut self, stmts: &[Stmt]) -> Result<StmtRes, String> {
        self.env_stack.push(Environment::new());

        let mut res = StmtRes::new(StmtResType::Ok, 9000);
        for stmt in stmts {
            res = self.eval_stmt(stmt)?;

            use StmtResType::*;
            match res.srtype {
                Ok => (),
                Return(_) | Continue | Break => break,
            }
        }

        self.env_stack.pop();
        Ok(res)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<StmtRes, String> {
        use StmtType::*;
        match stmt.stype() {
            If {
                cond_and_code,
                else_code,
            } => self.eval_if_stmt(cond_and_code, else_code),
            While { cond, code } => self.eval_while_stmt(cond, code),
            FnDef { name, params, code } => {
                self.eval_fn_def(name, params, code, stmt.line_num())
            }
            ExprStmt(e) => self.eval_expr_stmt(e),
            Return(e) => self.eval_ret_stmt(e, stmt.line_num()),
            Continue => Ok(StmtRes::new(StmtResType::Continue, stmt.line_num())),
            Break => Ok(StmtRes::new(StmtResType::Break, stmt.line_num())),
        }
    }

    fn eval_if_stmt(
        &mut self,
        cond_and_code: &[(Expr, Vec<Stmt>)],
        else_code: &[Stmt],
    ) -> Result<StmtRes, String> {
        for (cond, code) in cond_and_code {
            if self.is_true(cond)? {
                return self.eval_block(code);
            }
        }

        self.eval_block(else_code)
    }

    fn is_true(&mut self, expr: &Expr) -> Result<bool, String> {
        match self.eval_expr(expr)? {
            RuntimeValue::Bool { value } => Ok(value),
            u => Err(format!(
                "line {}: codition must evaluate to a boolean, got '{u}' instead",
                expr.line_num()
            )),
        }
    }

    fn eval_while_stmt(&mut self, cond: &Expr, code: &[Stmt]) -> Result<StmtRes, String> {
        while self.is_true(cond)? {
            let res = self.eval_block(code)?;
            match res.srtype {
                StmtResType::Ok | StmtResType::Continue => (),
                StmtResType::Break => break,
                StmtResType::Return(_) => return Ok(res),
            }
        }

        Ok(StmtRes::new(StmtResType::Ok, cond.line_num()))
    }

    fn eval_fn_def(
        &mut self,
        name: &str,
        params: &[String],
        code: &[Stmt],
        line_num: usize,
    ) -> Result<StmtRes, String> {
        self.top_env().bindings.insert(
            name.to_owned(),
            RuntimeValue::Fn {
                params: params.to_owned(),
                code: code.to_owned(),
            },
        );

        Ok(StmtRes::new(StmtResType::Ok, line_num))
    }

    fn eval_expr_stmt(&mut self, expr: &Expr) -> Result<StmtRes, String> {
        self.eval_expr(expr)?;

        Ok(StmtRes::new(StmtResType::Ok, expr.line_num()))
    }

    fn eval_ret_stmt(&mut self, expr: &Expr, line_num: usize) -> Result<StmtRes, String> {
        Ok(StmtRes::new(
            StmtResType::Return(self.eval_expr(expr)?),
            line_num,
        ))
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<RuntimeValue, String> {
        use ExprType::*;
        match expr.etype() {
            Binary { lhs, op, rhs } => self.eval_bin_op(lhs, op, rhs),
            Unary { op, rhs } => self.eval_unary_op(op, rhs),
            FnCall { name, args } => self.eval_fn_call(name, args, expr.line_num()),
            BuiltinFnCall { name, args } => {
                self.eval_builtin_fn_call(name, args, expr.line_num())
            }
            Identifier { name } => self.eval_ident_access(name),
            _ => Ok(RuntimeValue::from_expr(expr)),
        }
    }

    fn eval_bin_op(
        &mut self,
        lhs: &Expr,
        op: &TokenType,
        rhs: &Expr,
    ) -> Result<RuntimeValue, String> {
        let rhs = self.eval_expr(rhs)?;

        if *op == TokenType::Equals {
            let ExprType::Identifier { name } = lhs.etype() else {
                return Err(format!(
                    "line {}: LHS of an assignment operation is not an identifier", lhs.line_num()));
            };

            // FIXME(spff): Выводить ошибку, если `name` - имя функции
            self.set_value(name, rhs.clone());
            return Ok(rhs);
        }

        let line_num = lhs.line_num();
        let lhs = self.eval_expr(lhs)?;

        use RuntimeValue::*;
        Ok(match (&lhs, &rhs) {
            (Number { value: lhs }, Number { value: rhs }) => {
                use TokenType as TT;
                match op {
                    TT::Plus => Number { value: lhs + rhs },
                    TT::Minus => Number { value: lhs - rhs },
                    TT::Mult => Number { value: lhs * rhs },
                    // При делении на 0 выдаёт `inf`, пусть так и будет
                    TT::Div => Number { value: lhs / rhs },
                    TT::Mod => Number {
                        value: (*lhs as i64 % *rhs as i64) as f64,
                    },
                    TT::Pow => Number {
                        value: lhs.powf(*rhs),
                    },

                    TT::EqEq => Bool { value: lhs == rhs },
                    TT::NotEq => Bool { value: lhs != rhs },
                    TT::Less => Bool { value: lhs < rhs },
                    TT::LessEq => Bool { value: lhs <= rhs },
                    TT::Greater => Bool { value: lhs > rhs },
                    TT::GreaterEq => Bool { value: lhs >= rhs },

                    TT::And | TT::Or => {
                        return Err(format!(
                        "line {line_num}: can't perform a logical `{op}` on two numbers"
                    ))
                    }
                    _ => unreachable!(),
                }
            }
            (Bool { value: lhs }, Bool { value: rhs }) => {
                use TokenType as TT;
                match op {
                    TT::EqEq => Bool { value: lhs == rhs },
                    TT::NotEq => Bool { value: lhs != rhs },
                    TT::And => Bool {
                        value: *lhs && *rhs,
                    },
                    TT::Or => Bool {
                        value: *lhs || *rhs,
                    },
                    _ => {
                        return Err(
                            format!(
                        "line {line_num}: can't perform an arithmetic `{op}` on two booleans"),
                        )
                    }
                }
            }
            (_, _) => {
                return Err(format!(
                    "line {line_num}: can't perform binary operation `{op}` \
                    on '{lhs}' and '{rhs}'"
                ))
            }
        })
    }

    // TODO(spff): Заменить копирование значения на что-то с `Rc` и `RefCell`
    fn get_value(&self, name: &str) -> Option<RuntimeValue> {
        for env in self.env_stack.iter().rev() {
            if let Some(value) = env.bindings.get(name) {
                return Some(value.to_owned());
            }
        }

        None
    }

    fn set_value(&mut self, name: &str, value: RuntimeValue) {
        for env in self.env_stack.iter_mut().rev() {
            if let Some(v) = env.bindings.get_mut(name) {
                *v = value;
                return;
            }
        }

        self.top_env().bindings.insert(name.to_owned(), value);
    }

    fn eval_unary_op(
        &mut self,
        op: &TokenType,
        rhs: &Expr,
    ) -> Result<RuntimeValue, String> {
        let line_num = rhs.line_num();
        let rhs = self.eval_expr(rhs)?;

        use RuntimeValue::*;
        use TokenType as TT;
        Ok(match (op, rhs) {
            (TT::Not, Bool { value }) => RuntimeValue::Bool { value: !value },
            (TT::Minus, Number { value }) => RuntimeValue::Number { value: -value },
            (TT::Fact, Number { value }) => RuntimeValue::Number {
                value: (1..=value as i64).product::<i64>() as f64,
            },
            (o, r) => {
                return Err(format!(
                    "line {line_num}: can't perform unary operation `{o}` on '{r}'"
                ))
            }
        })
    }

    fn eval_fn_call(
        &mut self,
        name: &str,
        args: &[Expr],
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        let Some(RuntimeValue::Fn { params, code }) = self.get_value(name) else {
            return Err(format!(
                "line {line_num}: call to an undefined function '{name}'"
            ));
        };

        if args.len() != params.len() {
            return Err(format!(
                "line {line_num}: function '{name}' expected {} arguments, got {}",
                params.len(),
                args.len()
            ));
        }

        let mut eargs = vec![];
        for arg in args {
            eargs.push(self.eval_expr(arg)?);
        }

        // Почти как в `eval_block()`
        self.env_stack.push(Environment::new());

        let top = self.top_env();
        for (var_name, value) in params.into_iter().zip(eargs) {
            top.bindings.insert(var_name, value);
        }

        // FIXME(spff): `return` должен иметь `Option<T>` как значение
        let mut res = RuntimeValue::Nil;
        for stmt in code {
            use StmtResType::*;
            let sr = self.eval_stmt(&stmt)?;
            match sr.srtype {
                Ok => (),
                Return(v) => {
                    res = v;
                    break;
                }
                Continue | Break => {
                    return Err(format!(
                        "line {}: unexpected {} in function body",
                        sr.line_num, sr.srtype
                    ));
                }
            }
        }

        self.env_stack.pop();
        Ok(res)
    }

    fn eval_builtin_fn_call(
        &mut self,
        name: &TokenType,
        args: &[Expr],
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        let mut values = vec![];
        for arg in args {
            values.push(self.eval_expr(arg)?);
        }

        use TokenType::*;
        match name {
            PrintL | Print => {
                if values.is_empty() {
                    return Err(format!(
                        "line {line_num}: `print` or `printl` need at least 1 argument"
                    ));
                }

                for value in values {
                    match value {
                        RuntimeValue::Fn { params: _, code: _ } => {
                            return Err(format!(
                                "line {line_num}: can't print a function"
                            ))
                        }
                        v => print!("{v}"),
                    };
                }

                if *name == PrintL {
                    println!();
                }

                Ok(RuntimeValue::Nil)
            }
            // TODO(spff): Переписать в макрос при добавлении новых мат. функций
            Sin => {
                let num_args = values.len();
                if num_args != 1 {
                    return Err(format!(
                        "line {line_num}: `sin` takes 1 argument, got {num_args}"
                    ));
                }

                let RuntimeValue::Number { value } = values[0] else {
                    return Err(format!(
                        "line {line_num}: `sin` expected a number, got: '{}'", values[0]
                    ));
                };

                Ok(RuntimeValue::Number { value: value.sin() })
            }
            Cos => {
                let num_args = values.len();
                if num_args != 1 {
                    return Err(format!(
                        "line {line_num}: `cos` takes 1 argument, got {num_args}"
                    ));
                }

                let RuntimeValue::Number { value } = values[0] else {
                    return Err(format!(
                        "line {line_num}: `cos` expected a number, got: '{}'", values[0]
                    ));
                };

                Ok(RuntimeValue::Number { value: value.cos() })
            }
            _ => unreachable!(),
        }
    }

    fn eval_ident_access(&mut self, name: &str) -> Result<RuntimeValue, String> {
        Ok(match self.get_value(name) {
            Some(v) => v,
            None => RuntimeValue::Nil,
        })
    }
}
