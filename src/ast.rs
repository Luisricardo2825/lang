use crate::{primitives, varpool::Variable};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Verb {
    Plus,
    Minus,
    Increment,
    Decrement,
    And,
    Or,
    Not,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    Range,
    ObjectAccess,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MathOperators {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Print(Box<Expr>),
    Terms(Vec<Expr>),
    Identifier {
        name: String,
        expr: Box<Expr>,
        modifier: String,
        context_id: String,
    },
    VarDeclaration {
        name: String,
        expr: Box<Expr>,
        modifier: String,
        context_id: String,
    },
    Assigment {
        name: String,
        expr: Box<Expr>,
        modifier: String,
        context_id: String,
    },
    IsGlobal {
        modifier: String,
        ident: String,
        expr: Box<Expr>,
    },
    Number(f64),
    String(String),
    UnaryMinus(Box<Expr>),
    UnaryPlus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: MathOperators,
        rhs: Box<Expr>,
    },
    Primitives(primitives::Primitives),
    MonadicOp {
        verb: Verb,
        expr: Box<Expr>,
    },
    DyadicOp {
        verb: DyadicVerb,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Fn {
        args: Vec<Expr>,
        body: Vec<Expr>,
        context_id: String,
        name: String,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
        context_id: String,
        body: Vec<Expr>,
    },
    Statments(Statments),
    Void,
    Null,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Statments {
    For {
        variable: Box<Variable>,
        rule: Box<Expr>,
        body: Box<Expr>,
        context_id: String,
        step: Option<Box<Expr>>,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum DyadicVerb {
    Verb(Verb),
    MathOperators(MathOperators),
}

impl Expr {
    pub fn is_primitive(&self) -> bool {
        match self {
            Expr::Primitives(_) => true,
            _ => false,
        }
    }
    pub fn is_terms(&self) -> bool {
        match self {
            Expr::Terms(_) => true,
            _ => false,
        }
    }
    pub fn is_void(&self) -> bool {
        match self {
            Expr::Void => true,
            _ => false,
        }
    }
    pub fn to_terms(&self) -> Vec<Expr> {
        match self {
            Expr::Terms(terms) => terms.clone(),
            _ => panic!("Expected terms"),
        }
    }

    pub fn to_primitive(&self) -> primitives::Primitives {
        match self {
            Expr::Primitives(p) => p.clone(),
            any => panic!("Expected primitive, got: {:?}", any),
        }
    }

    pub fn to_fn(&self) -> (Vec<Expr>, Vec<Expr>, String) {
        match self {
            Expr::Fn {
                args,
                body,
                context_id,
                name,
            } => (args.clone(), body.clone(), context_id.clone()),
            _ => panic!("Expected function"),
        }
    }
    pub fn to_var(&self) -> (String, Box<Expr>, String, String) {
        match self {
            Expr::Identifier {
                name,
                modifier,
                expr,
                context_id,
            } => (
                name.clone(),
                expr.clone(),
                modifier.clone(),
                context_id.clone(),
            ),
            Expr::Assigment {
                name,
                expr,
                modifier,
                context_id,
            } => (
                name.clone(),
                expr.clone(),
                modifier.clone(),
                context_id.clone(),
            ),
            Expr::VarDeclaration {
                name,
                expr,
                modifier,
                context_id,
            } => (
                name.clone(),
                expr.clone(),
                modifier.clone(),
                context_id.clone(),
            ),
            _ => panic!("Expected identifier"),
        }
    }
}
