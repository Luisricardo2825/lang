mod ast;
mod dyadic_expr;
mod monadic_expr;
mod primitives;
mod varpool;

use pest::Parser;
use std::{io, time::Instant};
use varpool::add_var;

use crate::ast::Expr;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct LangParser;

mod ast_parser;

fn main() -> io::Result<()> {
    let now = Instant::now();
    let mut var_pool: Vec<varpool::Variable> = vec![];
    let unparsed_file = std::fs::read_to_string("example.er").expect("cannot read jsc file");
    let pairs = LangParser::parse(Rule::program, &unparsed_file).expect("Erro parsing");
    for pair in pairs {
        let str = pair.as_str();
        if str.len() <= 0 {
            continue;
        }

        let exprs = ast_parser::resolve_rule(pair, &mut var_pool, "global");
        eval(exprs, &mut var_pool);
    }

    println!("Tempo decorrido: {}ms", now.elapsed().as_millis());
    Ok(())
}

fn eval(expr: ast::Expr, var_pool: &mut Vec<varpool::Variable>) -> primitives::Primitives {
    match expr {
        ast::Expr::Number(i) => primitives::Primitives::Number(i),
        ast::Expr::UnaryMinus(e) => {
            let result = -eval(*e, var_pool).to_number();
            primitives::Primitives::Number(result)
        }
        Expr::UnaryPlus(i) => {
            let result = eval(*i, var_pool).to_number();
            primitives::Primitives::Number(result)
        }
        ast::Expr::BinOp { lhs, op, rhs } => {
            let lhs = eval(*lhs, var_pool).to_number();
            let rhs = eval(*rhs, var_pool).to_number();
            let result = match op {
                ast::MathOperators::Add => lhs + rhs,
                ast::MathOperators::Subtract => lhs - rhs,
                ast::MathOperators::Multiply => lhs * rhs,
                ast::MathOperators::Divide => lhs / rhs,
                ast::MathOperators::Modulo => lhs % rhs,
            };
            primitives::Primitives::Number(result)
        }
        ast::Expr::String(value) => primitives::Primitives::String(value),
        ast::Expr::Primitives(value) => value,
        ast::Expr::IsGlobal {
            modifier: _,
            ident: _,
            expr,
        } => {
            eval(*expr, var_pool);
            primitives::Primitives::Void
        }
        ast::Expr::MonadicOp { verb, expr } => match verb {
            ast::Verb::Not => {
                let value = eval(*expr, var_pool);
                match value {
                    primitives::Primitives::String(value) => {
                        primitives::Primitives::Boolean(value.is_empty())
                    }
                    primitives::Primitives::Boolean(value) => {
                        primitives::Primitives::Boolean(!value)
                    }
                    primitives::Primitives::Number(value) => {
                        let value = value.is_nan() || value <= 0.0;
                        primitives::Primitives::Boolean(value)
                    }
                    primitives::Primitives::Array(value) => {
                        primitives::Primitives::Boolean(value.is_empty())
                    }
                    primitives::Primitives::Object(value) => {
                        primitives::Primitives::Boolean(value.is_empty())
                    }
                    primitives::Primitives::Null => primitives::Primitives::Boolean(false),
                    primitives::Primitives::Void => primitives::Primitives::Boolean(false),
                }
            }
            ast::Verb::Plus => {
                let value = eval(*expr, var_pool);
                return primitives::Primitives::Number(value.to_number());
            }
            verb => todo!("Unsupported verb: {:?}", verb),
        },
        ast::Expr::DyadicOp { verb, lhs, rhs } => {
            let lhs = eval(*lhs, var_pool);
            let rhs = eval(*rhs, var_pool);
            return match verb {
                ast::DyadicVerb::Verb(verb) => match verb {
                    ast::Verb::Plus => {
                        if !lhs.is_number() || !rhs.is_number() {
                            return primitives::Primitives::String(lhs.concat_string(&rhs));
                        }
                        primitives::Primitives::Number(lhs.to_number() + rhs.to_number())
                    }
                    ast::Verb::Range => {
                        let lhs = lhs.to_integer();
                        let rhs = rhs.to_integer();

                        let result = lhs..rhs;
                        let vec: Vec<primitives::Primitives> = result
                            .map(|x| primitives::Primitives::Number(x as f64))
                            .collect();
                        primitives::Primitives::Array(vec)
                    }
                    ast::Verb::Lt => {
                        if lhs.is_number() && rhs.is_number() {
                            return primitives::Primitives::Boolean(
                                lhs.to_number() < rhs.to_number(),
                            );
                        }

                        if lhs.is_string() && rhs.is_string() {
                            return primitives::Primitives::Boolean(
                                lhs.to_string() < rhs.to_string(),
                            );
                        }

                        return primitives::Primitives::Boolean(false);
                    }
                    ast::Verb::Gt => {
                        if lhs.is_number() && rhs.is_number() {
                            return primitives::Primitives::Boolean(
                                lhs.to_number() > rhs.to_number(),
                            );
                        }

                        if lhs.is_string() && rhs.is_string() {
                            return primitives::Primitives::Boolean(
                                lhs.to_string() > rhs.to_string(),
                            );
                        }

                        return primitives::Primitives::Boolean(false);
                    }
                    ast::Verb::Lte => {
                        if lhs.is_number() && rhs.is_number() {
                            return primitives::Primitives::Boolean(
                                lhs.to_number() <= rhs.to_number(),
                            );
                        }
                        if lhs.is_string() && rhs.is_string() {
                            return primitives::Primitives::Boolean(
                                lhs.to_string() <= rhs.to_string(),
                            );
                        }
                        return primitives::Primitives::Boolean(false);
                    }
                    ast::Verb::Gte => {
                        if lhs.is_number() && rhs.is_number() {
                            return primitives::Primitives::Boolean(
                                lhs.to_number() >= rhs.to_number(),
                            );
                        }
                        if lhs.is_string() && rhs.is_string() {
                            return primitives::Primitives::Boolean(
                                lhs.to_string() >= rhs.to_string(),
                            );
                        }
                        return primitives::Primitives::Boolean(false);
                    }
                    ast::Verb::Eq => {
                        if lhs.is_number() && rhs.is_number() {
                            return primitives::Primitives::Boolean(
                                lhs.to_number() == rhs.to_number(),
                            );
                        }
                        if lhs.is_string() && rhs.is_string() {
                            return primitives::Primitives::Boolean(
                                lhs.to_string() == rhs.to_string(),
                            );
                        }

                        if lhs.is_boolean() && rhs.is_boolean() {
                            return primitives::Primitives::Boolean(
                                lhs.to_boolean() == rhs.to_boolean(),
                            );
                        }
                        return primitives::Primitives::Boolean(
                            lhs.to_boolean() == rhs.to_boolean(),
                        );
                    }
                    ast::Verb::And => {
                        primitives::Primitives::Boolean(lhs.to_boolean() && rhs.to_boolean())
                    }
                    ast::Verb::Or => {
                        primitives::Primitives::Boolean(lhs.to_boolean() || rhs.to_boolean())
                    }
                    _ => todo!(),
                },
                ast::DyadicVerb::MathOperators(verb) => {
                    let lhs = lhs.to_number();
                    let rhs = rhs.to_number();
                    primitives::Primitives::Number(match verb {
                        ast::MathOperators::Add => lhs + rhs,
                        ast::MathOperators::Subtract => lhs - rhs,
                        ast::MathOperators::Multiply => lhs * rhs,
                        ast::MathOperators::Divide => lhs / rhs,
                        ast::MathOperators::Modulo => lhs % rhs,
                    })
                }
            };
        }
        ast::Expr::Terms(terms) => {
            let terms = terms.clone();
            let terms: Vec<primitives::Primitives> = terms
                .into_iter()
                .map(|x| eval(x.to_owned(), &mut var_pool.clone()))
                .collect();
            return terms.last().unwrap().to_owned();
        }
        ast::Expr::Print(value) => {
            println!("{}", eval(*value, var_pool).to_string());
            primitives::Primitives::Null
        }
        ast::Expr::Void => primitives::Primitives::Void,
        ast::Expr::Fn {
            args,
            body,
            context_id,
            name,
        } => primitives::Primitives::Void,
        ast::Expr::FnCall {
            name,
            args,
            context_id,
            body,
        } => {
            for ele in args {
                eval(ele, var_pool);
            }
            println!("função {name} do contexto {context_id} foi executada");
            eval(Expr::Terms(body), var_pool)
        }
        ast::Expr::Identifier {
            name,
            expr,
            modifier,
            context_id,
        } => {
            let var = varpool::get_var_value(var_pool, &name, &context_id);
            match var {
                Ok(var) => {
                    return eval(var.value, var_pool);
                }
                Err(_) => {
                    let var = add_var(var_pool, &name, &modifier, &expr, &context_id);
                    return eval(var.value, var_pool);
                }
            }
        }
        Expr::Null => primitives::Primitives::Null,
    }
}
