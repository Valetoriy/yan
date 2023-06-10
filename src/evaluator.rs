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
            } => todo!(),
            While { cond, code } => todo!(),
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
            Binary { lhs, op, rhs } => todo!(),
            Unary { op, rhs } => todo!(),
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
}
