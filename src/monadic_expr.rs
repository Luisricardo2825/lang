use crate::ast::*;

pub fn parse_monadic_verb(pair: &str, expr: Expr) -> Expr {
    Expr::MonadicOp {
        verb: match pair {
            "+" => Verb::Plus,
            "-" => Verb::Minus,
            "!" => Verb::Not,
            _ => panic!("Unsupported monadic verb: {}", pair),
        },
        expr: Box::new(expr),
    }
}
