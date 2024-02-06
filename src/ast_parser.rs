use std::collections::HashMap;

use crate::varpool::{add_var, Variable};

use super::LangParser;
use super::{ast::*, dyadic_expr, monadic_expr, primitives, varpool};

use super::eval;

use pest::iterators::Pairs;

use pest::pratt_parser::PrattParser;
use pest::Parser;

use super::Rule;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left)| Op::infix(eq, Left))
            .op(Op::prefix(unary_minus) | Op::prefix(not)| Op::prefix(increment)| Op::prefix(decrement)| Op::prefix(plus))

    };
}

pub fn parse_expr(
    pairs: Pairs<Rule>,
    var_pool: &mut Vec<varpool::Variable>,
    context_id: &str,
) -> Expr {
    if pairs.len() > 0 {
        PRATT_PARSER
            .map_primary(|primary| resolve_rule(primary, var_pool, context_id))
            .map_infix(|lhs, op, rhs| {
                let op = match op.as_rule() {
                    Rule::add => MathOperators::Add,
                    Rule::subtract => MathOperators::Subtract,
                    Rule::multiply => MathOperators::Multiply,
                    Rule::divide => MathOperators::Divide,
                    Rule::modulo => MathOperators::Modulo,
                    rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
                };
                Expr::BinOp {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                }
            })
            .map_prefix(|op, rhs| match op.as_rule() {
                Rule::unary_minus => Expr::UnaryMinus(Box::new(rhs)),
                Rule::plus => Expr::UnaryPlus(Box::new(rhs)),
                _ => unreachable!(),
            })
            .parse(pairs)
    } else {
        panic!("Empty expression: {:?}", pairs);
    }
}

pub(crate) fn resolve_rule(
    primary: pest::iterators::Pair<'_, Rule>,
    var_pool: &mut Vec<varpool::Variable>,
    context_id: &str,
) -> Expr {
    match primary.as_rule() {
        Rule::functions => {
            let mut keyword = primary.into_inner();
            let keyword = keyword.next().unwrap();
            resolve_rule(keyword, var_pool, context_id)
        }
        Rule::print => {
            let mut pair = primary.into_inner();
            let expr = pair.next().unwrap();
            let expr = resolve_rule(expr, var_pool, context_id);
            Expr::Print(Box::new(expr))
        }
        Rule::keywords => {
            let mut keyword = primary.into_inner();
            let keyword = keyword.next().unwrap();
            resolve_rule(keyword, var_pool, context_id)
        }
        Rule::forLoop => {
            let mut pair = primary.into_inner();
            let stmt = pair.next().unwrap().as_str();
            if stmt.trim() != "for" {
                panic!("Expected for loop");
            }
            let ident = pair.next().unwrap();
            let ident = ident.as_str().to_string();
            let expr = pair.next().unwrap();
            let expr = resolve_rule(expr, var_pool, context_id);
            let mut terms = vec![];
            let local_context_id = generate_context_id();

            for body in pair.clone() {
                let body = body.into_inner();
                let body = parse_expr(body, var_pool, &local_context_id);
                // eval(body, var_pool);
                terms.push(body);
            }

            Expr::Statments(Statments::For {
                variable: Box::new(Variable {
                    context_id: local_context_id.clone(),
                    value: Expr::Null,
                    is_const: false,
                    is_func: false,
                    ident,
                }),
                rule: Box::new(expr),
                body: Box::new(Expr::Terms(terms)),
                context_id: local_context_id,
                step: Some(Box::new(Expr::Void)),
            })
        }
        Rule::ifLogic => {
            // TODO: Correção do IF
            let mut pair = primary.into_inner();

            let expression = pair.next().unwrap();
            let if_body = pair.next();
            let else_body = pair.next();

            if if_body.is_some() {
                let if_body = if_body.unwrap();
                let expr = expression.to_owned().into_inner();
                let operations = expr.as_str().split("||");
                let mut expressions = vec![];
                for ele in operations {
                    let value = format!("({})", ele);
                    expressions.push(value);
                }

                let expr = expressions.join("||");
                expressions.clear();

                let operations = expr.as_str().split("&&");
                let mut expressions = vec![];
                for ele in operations {
                    let value = format!("({})", ele);
                    expressions.push(value);
                }
                let expression = expressions.join("&&");

                let pairs = LangParser::parse(Rule::expr, &expression).unwrap();

                let expression = parse_expr(pairs, var_pool, context_id);

                let expression_evaluated = eval(expression, var_pool).to_boolean();

                for body in if_body.into_inner() {
                    let body_expressions = body.to_owned();
                    if expression_evaluated {
                        let body = resolve_rule(body_expressions.clone(), var_pool, context_id);
                        eval(body, var_pool);
                    }
                }
                if else_body.to_owned().is_some() {
                    if expression_evaluated == false {
                        let else_body = else_body.unwrap();

                        for body in else_body.into_inner() {
                            let body = resolve_rule(body, var_pool, context_id);
                            eval(body, var_pool);
                        }
                    }
                }
            }
            return Expr::Void;
        }
        Rule::expr => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::number => Expr::Number(primary.as_str().trim().parse::<f64>().unwrap()),
        Rule::mathExpr => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::string => {
            let str = primary.as_str().to_owned();
            let str = &str[1..str.len() - 1];

            let str = str.replace("\\\\", "\\");
            let str = str.replace("\\\"", "\"");
            let str = str.replace("\\n", "\n");
            let str = str.replace("\\r", "\r");
            let str = str.replace("\\t", "\t");
            Expr::String(String::from(&str[..]))
        }
        Rule::array => {
            let mut arr = vec![];
            for ele in primary.into_inner() {
                let ast = resolve_rule(ele, var_pool, context_id);
                arr.push(eval(ast, var_pool));
            }
            Expr::Primitives(primitives::Primitives::Array(arr))
        }
        Rule::object => {
            let mut obj = vec![];
            for ele in primary.into_inner() {
                let mut pair = ele.into_inner();
                let key = pair.next().unwrap();
                let value = pair.next().unwrap_or(key.clone());
                let key = key.as_str();
                let value = resolve_rule(value, var_pool, context_id);
                obj.push((String::from(key), eval(value, var_pool)));
            }
            Expr::Primitives(primitives::Primitives::Object(obj))
        }
        Rule::objectProp => {
            let mut pairs = primary.into_inner();
            let object_name = pairs.next().unwrap().as_str();
            let sep = pairs.next().unwrap().as_str();
            let object_prop = pairs.next().unwrap().as_str();
            let var = varpool::get_var(var_pool, object_name, context_id);
            let var = var.unwrap();
            let mut object = eval(var.value, var_pool).to_object();
            let props: Vec<&str> = object_prop.split(sep).collect();
            let mut value = primitives::Primitives::Null;
            let mut count = 0;
            let size = props.len();
            for ele in props {
                let ob_value = object.clone().into_iter().find(|x| x.0 == ele);

                if ob_value.is_some() {
                    let ob_value = ob_value.unwrap().1;
                    if ob_value.is_object() {
                        object = ob_value.to_object();
                    }
                    if count == size - 1 {
                        value = ob_value.to_owned();
                    }
                }

                count += 1;
            }
            Expr::Primitives(value)
        }
        Rule::value => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::ident => {
            // get value from var_pool
            let ident = primary.as_str();
            let ident = ident.trim();
            let var = varpool::get_var_value(var_pool, ident, context_id);
            if var.is_err() {
                panic!("Variable {} doesn't exist", ident)
            }

            let var = var.unwrap();

            Expr::Identifier {
                name: ident.to_owned(),
                expr: Box::new(var.value),
                modifier: (if var.is_const {
                    "const"
                } else {
                    if var.is_func {
                        "fn"
                    } else {
                        "let"
                    }
                })
                .to_owned(),
                context_id: context_id.to_owned(),
            }
        }
        Rule::declarationExpr => {
            let mut pair = primary.into_inner();
            let stmt = pair.next().unwrap().as_str();

            let ident = pair.next().unwrap();

            let expr = pair.next().unwrap();

            let expr = resolve_rule(expr, var_pool, context_id);

            let ident = ident.as_str();

            let var = varpool::add_var(var_pool, ident, stmt, &expr, context_id);
            Expr::VarDeclaration {
                name: var.ident,
                expr: Box::new(var.value),
                modifier: stmt.to_owned(),
                context_id: var.context_id,
            }
        }
        Rule::assgmtExpr => {
            let mut pair = primary.into_inner();

            let ident = pair.next().unwrap();

            let expr = pair.next().unwrap();

            let expr = resolve_rule(expr, var_pool, context_id);

            let ident = ident.as_str();

            let mut var: Option<Variable> = None;
            let contexts = varpool::get_contexts(var_pool);
            for context in contexts {
                var = varpool::get_var(var_pool, ident, &context);
                if var.is_some() {
                    break;
                }
            }
            let var = var.unwrap();
            Expr::Assigment {
                name: var.ident,
                expr: Box::new(expr),
                modifier: (if var.is_const {
                    "const"
                } else {
                    if var.is_func {
                        "fn"
                    } else {
                        "let"
                    }
                })
                .to_owned(),
                context_id: var.context_id,
            }
        }
        Rule::monadicExpr => {
            let mut pair = primary.into_inner();
            let verb = pair.next().unwrap();
            let expr = pair.next().unwrap();

            monadic_expr::parse_monadic_verb(
                verb.as_str(),
                resolve_rule(expr, var_pool, context_id),
            )
        }
        Rule::dyadicExpr => {
            let mut pairs = primary.into_inner();
            let lhs = pairs.next().unwrap();
            let verb = pairs.next().unwrap();
            let rhs = pairs.next().unwrap();
            let lhs = resolve_rule(lhs, var_pool, context_id);

            let rhs = resolve_rule(rhs, var_pool, context_id);

            dyadic_expr::parse_dyadic_verb(verb.as_str(), lhs, rhs)
        }
        Rule::terms => {
            let terms: Vec<Expr> = primary
                .into_inner()
                .map(|node| resolve_rule(node, var_pool, context_id))
                .collect();
            // If there's just a single term, return it without
            // wrapping it in a Terms node.
            match terms.len() {
                1 => terms.get(0).unwrap().clone(),
                _ => Expr::Terms(terms),
            }
        }
        Rule::unaryOperation => {
            let mut pairs = primary.into_inner();
            let ident = pairs.next().unwrap();
            let mut ident = ident.as_str();
            let verb;
            if ["++", "--", "!"].contains(&ident) {
                verb = ident;
                ident = pairs.next().unwrap().as_str();
            } else {
                verb = pairs.next().unwrap().as_str();
            }
            let value = varpool::get_var_value(var_pool, ident, context_id)
                .unwrap()
                .value;
            let value = eval(value, var_pool);

            let exp = match verb {
                "++" => {
                    let value = value.to_number();
                    let value = value + 1.0;
                    Expr::Primitives(primitives::Primitives::Number(value))
                }
                "--" => {
                    let value = value.to_number();
                    let value = value - 1.0;
                    Expr::Primitives(primitives::Primitives::Number(value))
                }
                verb => panic!("Unexpected verb: {}", verb),
            };

            varpool::add_var(var_pool, ident, "", &exp, context_id);

            Expr::Void
        }
        Rule::boolean => {
            let pair = primary.as_str();
            let pair = pair.trim();
            Expr::Primitives(primitives::Primitives::Boolean(pair == "true"))
        }
        Rule::functionLogic => {
            let mut pairs = primary.into_inner();
            let stmt = pairs.next().unwrap().as_str();
            if stmt.trim() != "fn" {
                panic!("Expected fn keyword");
            }
            let name = pairs.next().unwrap().as_str().to_owned();
            let args = pairs;
            let mut arguments: Vec<Expr> = vec![];
            let mut body_exprs: Vec<Expr> = vec![];
            let local_context = generate_context_id();

            for arg in args {
                let argument = resolve_rule(arg, var_pool, &local_context);
                body_exprs.push(argument);
            }

            let vars = varpool::get_all_vars_from_context(var_pool, &local_context);
            println!("vars: {vars:?}");
            for ele in vars {
                let var = ele;
                let name = var.ident.clone();
                let expr = var.value.clone();
                let context_id = var.context_id.clone();
                let var = varpool::add_var(var_pool, &name, "", &expr, &context_id);
                let expr = Expr::VarDeclaration {
                    name: var.ident,
                    expr: Box::new(var.value),
                    modifier: "".to_owned(),
                    context_id: var.context_id,
                };
                arguments.push(expr)
            }

            let func = Expr::Fn {
                args: arguments,
                body: body_exprs,
                context_id: local_context,
                name: name.clone(),
            };

            varpool::add_var(var_pool, &name, "", &func, context_id);

            func
        }
        Rule::fnCall => {
            let mut pairs = primary.into_inner();

            let name = pairs.next().unwrap().as_str();

            let args_values = pairs.next().unwrap().as_str();
            let body = varpool::get_var_value(var_pool, &name, context_id).unwrap();
            let (args, body, context_id) = body.value.to_fn();
            let concated = "[".to_owned() + args_values + "]";

            let args_pair = LangParser::parse(Rule::program, &concated).unwrap();

            let mut args_expr = Expr::Null;
            for pair in args_pair {
                let str = pair.as_str();
                if str.len() <= 0 {
                    continue;
                }

                args_expr = resolve_rule(pair, var_pool, &context_id);
            }

            let args_values = args_expr.to_primitive().to_array();

            let mut terms = vec![];
            let mut index = 0;
            for ele in args {
                let var = ele.to_var();
                let value = args_values.get(index);
                if value.is_some() {
                    let value =
                        Box::new(Expr::Primitives(args_values.get(index).unwrap().to_owned()));
                    let expr = Expr::Assigment {
                        name: var.0.clone(),
                        expr: value.clone(),
                        modifier: "const".to_owned(),
                        context_id: context_id.clone(),
                    };
                    varpool::add_var(var_pool, &var.0, "const", &value, &context_id);
                    terms.push(expr);
                }
                index += 1;
            }

            Expr::FnCall {
                name: name.to_owned(),
                args: terms,
                body,
                context_id: context_id,
            }
        }
        Rule::functionArgs => {
            let pairs = primary.into_inner();
            let args = pairs.as_str();
            let args: Vec<&str> = args.split(",").collect();
            for ele in args {
                varpool::add_var(var_pool, ele, "let", &Expr::Null, context_id);
            }
            Expr::Void
        }
        Rule::argsList => {
            let pairs = primary.into_inner();
            let args = pairs.as_str();
            let args: Vec<&str> = args.split(",").collect();

            for ele in args {
                varpool::add_var(var_pool, ele.trim(), "let", &Expr::Null, context_id);
            }
            Expr::Void
        }
        Rule::null => Expr::Null,
        Rule::EOI => Expr::Void,
        rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
    }
}

fn generate_context_id() -> String {
    use rand::{thread_rng, Rng};

    use rand::distributions::Alphanumeric;
    let rand_string: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(30)
        .map(char::from)
        .collect();
    rand_string
}
