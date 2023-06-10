use crate::parser::*;
use std::collections::HashMap;

pub struct Environment {
    pub bindings: HashMap<String, RuntimeValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Fn {
        params: Vec<String>,
        code: Vec<Stmt>,
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
    Nil,
}

impl RuntimeValue {
    pub fn from_expr(e: Expr) -> Self {
        use RuntimeValue::*;
        match e {
            Expr::Number { value } => Number { value },
            Expr::Str { contents } => Str { contents },
            Expr::Bool { value } => Bool { value },
            _ => unreachable!(),
        }
    }
}
