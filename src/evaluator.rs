use crate::{Environment, Expr, RuntimeValue, Stmt, StmtType, TokenType};

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

pub struct Evaluator {
    env_stack: Vec<Environment>,
}

impl Evaluator {
    pub fn eval(stmts: Vec<Stmt>) -> Result<(), String> {
        let mut e = Evaluator { env_stack: vec![] };

        let res = e.eval_block(stmts)?;
        match &res.srtype {
            StmtResType::Ok => (),
            u => return Err(format!("line {}: {:?} in global scope", res.line_num, u)),
        }

        Ok(())
    }

    fn top_env(&mut self) -> &mut Environment {
        self.env_stack.last_mut().unwrap()
    }

    fn eval_block(&mut self, stmts: Vec<Stmt>) -> Result<StmtRes, String> {
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

    fn eval_stmt(&mut self, stmt: Stmt) -> Result<StmtRes, String> {
        use StmtType::*;
        let res = match stmt.stype() {
            If {
                cond_and_code,
                else_code,
            } => {
                for (cond, code) in cond_and_code.iter() {
                    if self.is_true(cond.clone(), stmt.line_num())? {
                        return Ok(self.eval_block(code.clone())?);
                    }
                }

                return Ok(self.eval_block(else_code)?);
            }
            While { cond, code } => {
                // TODO(spff): Как-то убрать постоянное копирование `cond` и `code`
                while self.is_true(cond.clone(), stmt.line_num())? {
                    let res = self.eval_block(code.clone())?;
                    match res.srtype {
                        StmtResType::Ok | StmtResType::Continue => (),
                        StmtResType::Break => break,
                        StmtResType::Return(_) => return Ok(res),
                    }
                }
                StmtResType::Ok
            }
            FnDef { name, params, code } => {
                self.top_env()
                    .bindings
                    .insert(name, RuntimeValue::Fn { params, code });
                StmtResType::Ok
            }
            ExrpStmt(e) => {
                self.eval_expr(e, stmt.line_num())?;
                StmtResType::Ok
            }
            Return(e) => StmtResType::Return(self.eval_expr(e, stmt.line_num())?),
            Continue => StmtResType::Continue,
            Break => StmtResType::Break,
        };

        Ok(StmtRes::new(res, stmt.line_num()))
    }

    // TODO(spff): Добавить line_num в Expr
    fn eval_expr(&mut self, expr: Expr, line_num: usize) -> Result<RuntimeValue, String> {
        use Expr::*;
        match expr {
            Binary { lhs, op, rhs } => self.eval_bin_op(*lhs, op, *rhs, line_num),
            Unary { op, rhs } => self.eval_unary_op(op, *rhs, line_num),
            FnCall { name, args } => self.eval_fn_call(&name, args, line_num),
            BuiltinFnCall { name, args } => {
                self.eval_builtin_fn_call(name, args, line_num)
            }
            Identifier { name } => match self.get_value(&name) {
                Some(v) => Ok(v.clone()),
                None => Ok(RuntimeValue::Nil),
            },
            e => Ok(RuntimeValue::from_expr(e)),
        }
    }

    fn is_true(&mut self, expr: Expr, line_num: usize) -> Result<bool, String> {
        match self.eval_expr(expr, line_num)? {
            RuntimeValue::Bool { value } => Ok(value),
            u => {
                return Err(format!(
                "line {line_num}: codition must evaluate to a boolean, got {u:?} instead"
            ))
            }
        }
    }

    fn eval_bin_op(
        &mut self,
        lhs: Expr,
        op: TokenType,
        rhs: Expr,
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        let rhs = self.eval_expr(rhs, line_num)?;

        if op == TokenType::Equals {
            let Expr::Identifier { name } = lhs else {
                return Err(format!(
                    "line {line_num}: LHS of an assignment operation is not an identifier"));
            };

            self.set_value(&name, rhs.clone());
            return Ok(rhs);
        }

        let lhs = self.eval_expr(lhs, line_num)?;

        // TODO(spff): Добавить операции со строками
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
                        "line {line_num}: can't perform a logical {op:?} on two numbers"
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
                        "line {line_num}: can't perform an arithmetic {op:?} on two booleans"),
                        )
                    }
                }
            }
            (_, _) => {
                return Err(format!(
                    "line {line_num}: can't perform binary operation ({op:?}) \
                    on {lhs:?} and {rhs:?}"
                ))
            }
        })
    }

    fn eval_unary_op(
        &mut self,
        op: TokenType,
        rhs: Expr,
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        let rhs = self.eval_expr(rhs, line_num)?;

        use RuntimeValue::*;
        use TokenType as TT;
        Ok(match (op, rhs) {
            (TT::Not, Bool { value }) => RuntimeValue::Bool { value: !value },
            (TT::Minus, Number { value }) => RuntimeValue::Number { value: -value },
            (TT::Fact, Number { value }) => RuntimeValue::Number {
                value: (1..value as i64).product::<i64>() as f64,
            },
            (o, r) => {
                return Err(format!(
                    "line {line_num}: can't perform unary operation ({o:?}) on {r:?}"
                ))
            }
        })
    }

    fn eval_fn_call(
        &mut self,
        name: &str,
        args: Vec<Expr>,
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        // TODO(spff): Эта ошибка также вылезет если name - обычная переменная.
        // Возможно, для этого случая нужна отдельная проверка
        let Some(RuntimeValue::Fn { params, code }) = self.get_value(name) else {
            return Err(format!(
                "line {line_num}: call to an undefined function `{name}`"
            ));
        };

        if args.len() != params.len() {
            return Err(format!(
                "line {line_num}: function `{name}` expected {} arguments, got {}",
                params.len(),
                args.len()
            ));
        }

        let mut eargs = vec![];
        for arg in args {
            eargs.push(self.eval_expr(arg, line_num)?);
        }

        // Почти как в `eval_block()`
        self.env_stack.push(Environment::new());

        let top = self.top_env();
        for (var_name, value) in params.into_iter().zip(eargs) {
            top.bindings.insert(var_name, value);
        }

        let mut res = RuntimeValue::Nil;
        for stmt in code {
            use StmtResType::*;
            let sr = self.eval_stmt(stmt)?;
            match sr.srtype {
                Ok => (),
                Return(v) => {
                    res = v;
                    break;
                }
                Continue | Break => {
                    return Err(format!(
                        "line {}: unexpected {:?} in function body",
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
        name: TokenType,
        args: Vec<Expr>,
        line_num: usize,
    ) -> Result<RuntimeValue, String> {
        let mut values = vec![];
        for arg in args {
            values.push(self.eval_expr(arg, line_num)?);
        }

        use TokenType::*;
        match name {
            Print => {
                for value in values {
                    match value {
                        RuntimeValue::Number { value } => print!("{value}"),
                        RuntimeValue::Str { contents } => print!("{contents}"),
                        RuntimeValue::Bool { value } => print!("{value}"),
                        RuntimeValue::Nil => print!("nil"),
                        _ => {
                            return Err(format!(
                                "line {line_num}: can't print a function"
                            ))
                        }
                    };
                }
                Ok(RuntimeValue::Nil)
            }
            // TODO(spff): Переписать в макрос
            Sin => {
                let num_args = values.len();
                if num_args != 1 {
                    return Err(format!(
                        "line {line_num}: `sin` takes 1 argument, got {num_args}"
                    ));
                }

                let RuntimeValue::Number { value } = values[0] else {
                    return Err(format!(
                        "line {line_num}: `sin` expected a number, got: {:?}", values[0]
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
                        "line {line_num}: `cos` expected a number, got: {:?}", values[0]
                    ));
                };

                Ok(RuntimeValue::Number { value: value.cos() })
            }
            _ => unreachable!(),
        }
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
}
