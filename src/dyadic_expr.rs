use crate::ast::*;

pub fn parse_dyadic_verb(pair: &str, lhs: Expr, rhs: Expr) -> Expr {
    Expr::DyadicOp {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        verb: match pair {
            "+" => DyadicVerb::Verb(Verb::Plus),
            "!" => DyadicVerb::Verb(Verb::Not),
            "!=" => DyadicVerb::Verb(Verb::Neq),
            "<>" => DyadicVerb::Verb(Verb::Neq),
            "==" => DyadicVerb::Verb(Verb::Eq),
            ".." => DyadicVerb::Verb(Verb::Range),
            "." => DyadicVerb::Verb(Verb::ObjectAccess),
            "<" => DyadicVerb::Verb(Verb::Lt),
            ">" => DyadicVerb::Verb(Verb::Gt),
            "<=" => DyadicVerb::Verb(Verb::Lte),
            ">=" => DyadicVerb::Verb(Verb::Gte),
            "&&" => DyadicVerb::Verb(Verb::And),
            "||" => DyadicVerb::Verb(Verb::Or),
            verb => match verb {
                "*" => DyadicVerb::MathOperators(MathOperators::Multiply),
                "/" => DyadicVerb::MathOperators(MathOperators::Divide),
                "%" => DyadicVerb::MathOperators(MathOperators::Divide),
                _ => panic!("Unexpected dyadic verb: {}", pair),
            },
        },
    }
}
