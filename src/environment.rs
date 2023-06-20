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
    pub fn from_expr(e: &Expr) -> Self {
        use RuntimeValue::*;
        match e.etype().clone() {
            ExprType::Number { value } => Number { value },
            ExprType::Str { contents } => Str { contents },
            ExprType::Bool { value } => Bool { value },
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RuntimeValue::*;
        match self {
            Number { value } => write!(f, "{value}"),
            Str { contents } => write!(f, "{contents}"),
            Bool { value } => write!(f, "{value}"),
            Nil => write!(f, "nil"),
            _ => unreachable!(),
        }
    }
}
