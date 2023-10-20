use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest::Parser;
use std::{collections::HashMap, io};

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct LangParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
            .op(Op::prefix(unary_minus))
    };
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Verb {
    Plus,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
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
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keywords {
    For { body: Box<Vec<Expr>> },
}
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Print(Box<Expr>),
    Keywords(Keywords),
    Terms(Vec<Expr>),
    IsGlobal {
        modifier: String,
        ident: String,
        expr: Box<Expr>,
    },
    Number(f64),
    String(String),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Verb,
        rhs: Box<Expr>,
    },
    Primitives(Primitives),
    MonadicOp {
        verb: Verb,
        expr: Box<Expr>,
    },
    DyadicOp {
        verb: Verb,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Null,
}

pub fn parse_expr(pairs: Pairs<Rule>, var_pool: &mut Vec<Variable>, context_id: &str) -> Expr {
    if pairs.len() > 0 {
        PRATT_PARSER
            .map_primary(|primary| resolve_rule(primary, var_pool, context_id))
            .map_infix(|lhs, op, rhs| {
                let op = match op.as_rule() {
                    Rule::add => Verb::Add,
                    Rule::subtract => Verb::Subtract,
                    Rule::multiply => Verb::Multiply,
                    Rule::divide => Verb::Divide,
                    Rule::modulo => Verb::Modulo,
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
                _ => unreachable!(),
            })
            .parse(pairs)
    } else {
        panic!("Empty expression: {:?}", pairs);
    }
}

fn resolve_rule(
    primary: pest::iterators::Pair<'_, Rule>,
    var_pool: &mut Vec<Variable>,
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
            let ident = ident.as_str();
            let expr = pair.next().unwrap();
            let expr = resolve_rule(expr, var_pool, context_id);

            let expr = eval(expr);
            let to = expr.to_array();
            let local_context_id = generate_context_id();
            for index in to {
                add_var(
                    var_pool,
                    ident,
                    "",
                    &&Expr::Primitives(index.clone()),
                    &local_context_id,
                );
                for body in pair.clone() {
                    let body = body.into_inner();
                    for code in body {
                        let body = resolve_rule(code, var_pool, &local_context_id);
                        eval(body);
                    }
                }
            }

            Expr::Null
        }
        Rule::expr => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::number => Expr::Number(primary.as_str().trim().parse::<f64>().unwrap()),
        Rule::mathExpr => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::string => {
            let str = primary.as_str().to_owned();

            let str = &str[1..str.len() - 1];

            let str = str.replace("\\\"", "\"");
            Expr::String(String::from(&str[..]))
        }
        Rule::array => {
            let mut arr = vec![];
            for ele in primary.into_inner() {
                let ast = parse_expr(ele.into_inner(), var_pool, context_id);
                arr.push(eval(ast));
            }
            Expr::Primitives(Primitives::Array(arr))
        }
        Rule::object => {
            let mut obj = HashMap::new();
            for ele in primary.into_inner() {
                let mut pair = ele.into_inner();
                let key = pair.next().unwrap();
                let value = pair.next().unwrap_or(key.clone());
                let key = key.as_str();
                let value = resolve_rule(value, var_pool, context_id);
                obj.insert(String::from(key), eval(value));
            }
            Expr::Primitives(Primitives::Object(obj))
        }
        Rule::value => parse_expr(primary.into_inner(), var_pool, context_id),
        Rule::ident => {
            // get value from var_pool
            let ident = primary.as_str();
            let ident = ident.trim();
            let ident = ident.to_string();
            let msg = format!(
                "Variable {} not found. Context: {context_id} {:?}",
                ident, var_pool
            );
            let var = get_var(var_pool, &ident, context_id);
            if var.is_none() {
                let mut contexts = get_contexts(var_pool);
                contexts.reverse();
                for context in contexts {
                    let var = get_var(var_pool, &ident, &context);
                    if var.is_some() {
                        return var.unwrap().value;
                    }
                }
                panic!("{}", msg);
            }
            let var = var.unwrap();
            var.value
        }
        Rule::assgmtExpr => {
            let mut pair = primary.into_inner();
            let stmt = pair.next().unwrap().as_str();

            let ident = pair.next().unwrap();

            let expr = pair.next().unwrap();

            let expr = resolve_rule(expr, var_pool, context_id);

            let ident = ident.as_str();

            add_var(var_pool, ident, stmt, &expr, context_id);
            Expr::Null
        }
        Rule::monadicExpr => {
            let mut pair = primary.into_inner();
            let verb = pair.next().unwrap();
            let expr = pair.next().unwrap();
            parse_monadic_verb(verb, resolve_rule(expr, var_pool, context_id))
        }
        Rule::dyadicExpr => {
            let mut pairs = primary.into_inner();
            let lhs = pairs.next().unwrap();
            let verb = pairs.next().unwrap();
            let rhs = pairs.next().unwrap();
            let lhs = resolve_rule(lhs, var_pool, context_id);

            let rhs = resolve_rule(rhs, var_pool, context_id);

            parse_dyadic_verb(verb, lhs, rhs)
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
        rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
    }
}

fn add_var(
    var_pool: &mut Vec<Variable>,
    ident: &str,
    modifier: &str,
    expr: &Expr,
    context_id: &str,
) {
    let ident = String::from(ident);
    let is_assigment = ["const", "let"].contains(&modifier);
    if is_assigment {
        let var = get_var(var_pool, &ident, context_id);

        if var.is_some() {
            return match var {
                Some(var) => {
                    let index = var_pool
                        .iter()
                        .position(|x| *x.ident == var.ident && x.context_id == context_id);

                    if index.is_none() {
                        return var_pool.push(Variable {
                            str_value: eval(expr.clone()).to_string(),
                            value: expr.to_owned(),
                            is_const: if modifier == "const" { true } else { false },
                            ident,
                            context_id: context_id.to_owned(),
                        });
                    }
                    let index = index.unwrap();
                    var_pool[index].value = expr.to_owned();
                    var_pool[index].is_const = if modifier == "const" { true } else { false };
                    var_pool[index].str_value = eval(expr.clone()).to_string();
                    var_pool[index].ident = ident.clone();
                }

                None => (),
            };
        }
        var_pool.push(Variable {
            str_value: eval(expr.clone()).to_string(),
            value: expr.to_owned(),
            is_const: if modifier == "const" { true } else { false },
            ident,
            context_id: context_id.to_owned(),
        });
    } else {
        let var = get_var(var_pool, &ident, context_id);
        match var {
            Some(var) => {
                let index = var_pool.iter().position(|x| *x == var).unwrap();
                var_pool[index].value = expr.to_owned();
            }
            None => add_var(var_pool, &ident, "let", expr, context_id),
        }
    }
}

fn get_var(var_pool: &mut Vec<Variable>, ident: &str, context_id: &str) -> Option<Variable> {
    let ident = String::from(ident);
    let mut variable = var_pool
        .clone()
        .into_iter()
        .filter(|ele| ele.ident == ident && ele.context_id == context_id);

    let var = variable.nth(0);
    match var {
        Some(var) => Some(var.clone()),
        None => get_from_global(var_pool, ident),
    }
}

fn get_from_global(var_pool: &mut Vec<Variable>, ident: String) -> Option<Variable> {
    let mut variable = var_pool
        .clone()
        .into_iter()
        .filter(|ele| ele.ident == ident && ele.context_id == "global");
    let var = variable.nth(0);
    var
}

fn get_contexts(var_pool: &mut Vec<Variable>) -> Vec<String> {
    let contexts: Vec<String> = var_pool
        .clone()
        .into_iter()
        .map(|ele| ele.context_id)
        .collect();
    contexts
}

fn parse_dyadic_verb(pair: pest::iterators::Pair<Rule>, lhs: Expr, rhs: Expr) -> Expr {
    Expr::DyadicOp {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        verb: match pair.as_str() {
            "+" => Verb::Plus,
            "!" => Verb::Not,
            "!=" => Verb::Neq,
            "==" => Verb::Eq,
            ".." | "." => Verb::Range,
            "*" => Verb::Multiply,
            _ => panic!("Unexpected dyadic verb: {}", pair.as_str()),
        },
    }
}

fn parse_monadic_verb(pair: pest::iterators::Pair<Rule>, expr: Expr) -> Expr {
    Expr::MonadicOp {
        verb: match pair.as_str() {
            "++" => Verb::Increment,
            "!" => Verb::Not,
            ".." => Verb::Range,
            _ => panic!("Unsupported monadic verb: {}", pair.as_str()),
        },
        expr: Box::new(expr),
    }
}

fn main() -> io::Result<()> {
    let mut var_pool: Vec<Variable> = vec![];
    let unparsed_file = std::fs::read_to_string("example.er").expect("cannot read jsc file");
    let pairs = LangParser::parse(Rule::program, &unparsed_file).expect("Erro parsing");
    for pair in pairs {
        let str = pair.as_str();
        if str.len() <= 0 {
            continue;
        }

        let exprs = parse_expr(pair.into_inner(), &mut var_pool, "global");
        eval(exprs);
    }

    Ok(())
}

#[derive(PartialEq, Debug, Clone)]

pub struct Variable {
    pub ident: String,
    pub value: Expr,
    pub is_const: bool,
    pub context_id: String,
    pub str_value: String,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Primitives {
    String(String),
    Boolean(bool),
    Number(f64),
    Array(Vec<Primitives>),
    Object(HashMap<String, Primitives>),
    Null,
    Eof,
}

fn eval(expr: Expr) -> Primitives {
    match expr {
        Expr::Number(i) => Primitives::Number(i),
        Expr::UnaryMinus(e) => {
            let result = -eval(*e).to_number();
            Primitives::Number(result)
        }
        Expr::BinOp { lhs, op, rhs } => {
            let lhs = eval(*lhs).to_number();
            let rhs = eval(*rhs).to_number();
            let result = match op {
                Verb::Add => lhs + rhs,
                Verb::Subtract => lhs - rhs,
                Verb::Multiply => lhs * rhs,
                Verb::Divide => lhs / rhs,
                Verb::Modulo => lhs % rhs,
                Verb::Plus => todo!(),
                Verb::Increment => todo!(),
                Verb::Decrement => todo!(),
                Verb::And => todo!(),
                Verb::Or => todo!(),
                Verb::Not => todo!(),
                Verb::Lt => todo!(),
                Verb::Lte => todo!(),
                Verb::Gt => todo!(),
                Verb::Gte => todo!(),
                Verb::Eq => todo!(),
                Verb::Neq => todo!(),
                Verb::Range => todo!(),
            };
            Primitives::Number(result)
        }
        Expr::String(value) => Primitives::String(value),
        Expr::Primitives(value) => value,
        Expr::IsGlobal {
            modifier,
            ident,
            expr,
        } => {
            eval(*expr);
            Primitives::Eof
        }
        Expr::MonadicOp { verb: _, expr: _ } => todo!(),
        Expr::DyadicOp { verb, lhs, rhs } => match verb {
            Verb::Plus => {
                let lhs = eval(*lhs);
                let rhs = eval(*rhs);
                if lhs.is_string() || rhs.is_string() {
                    return Primitives::String(lhs.concat_string(&rhs));
                }
                Primitives::Number(lhs.to_number() + rhs.to_number())
            }
            Verb::Increment => todo!(),
            Verb::Decrement => todo!(),
            Verb::And => todo!(),
            Verb::Or => todo!(),
            Verb::Not => todo!(),
            Verb::Lt => todo!(),
            Verb::Lte => todo!(),
            Verb::Gt => todo!(),
            Verb::Gte => todo!(),
            Verb::Eq => todo!(),
            Verb::Neq => todo!(),
            Verb::Range => {
                let lhs = eval(*lhs);
                let lhs = lhs.to_integer();
                let rhs = eval(*rhs).to_integer();

                let result = lhs..rhs;
                let vec: Vec<Primitives> = result
                    .into_iter()
                    .map(|x| Primitives::Number(x as f64))
                    .collect();
                Primitives::Array(vec)
                // Primitives::Number(lhs.to_number() + rhs.to_number())
            }
            Verb::Add => todo!(),
            Verb::Subtract => todo!(),
            Verb::Multiply => {
                let lhs = eval(*lhs);
                let rhs = eval(*rhs);
                if !lhs.is_number() {
                    panic!("Unexpected value: {:?}", lhs)
                }
                if !rhs.is_number() {
                    panic!("Unexpected value: {:?}", rhs)
                }
                let calc = lhs.to_number() * rhs.to_number();
                Primitives::Number(calc)
            }
            Verb::Divide => todo!(),
            Verb::Modulo => todo!(),
        },
        Expr::Terms(terms) => {
            let terms = terms.clone();
            let terms: Vec<Primitives> = terms.into_iter().map(|x| eval(x)).collect();
            return terms.last().unwrap().to_owned();
        }
        Expr::Keywords(keyword) => match keyword {
            Keywords::For { body } => {
                let terms = *body;
                for ele in terms {
                    eval(ele);
                }
                Primitives::Eof
            }
        },
        Expr::Print(value) => {
            println!("{}", eval(*value).to_string());
            Primitives::Eof
        }
        Expr::Null => Primitives::Eof,
    }
}

impl Primitives {
    pub fn to_string(&self) -> String {
        match self {
            Primitives::String(value) => value.to_owned(),
            Primitives::Number(value) => value.to_string(),
            Primitives::Boolean(value) => value.to_string(),
            Primitives::Array(value) => serialize_jsonvalue(&Primitives::Array(value.to_owned())),
            Primitives::Object(value) => serialize_jsonvalue(&Primitives::Object(value.to_owned())),
            Primitives::Null => "null".to_owned(),
            Primitives::Eof => "".to_owned(),
        }
    }

    pub fn to_value(&self) -> String {
        match self {
            Primitives::String(value) => value.to_owned(),
            Primitives::Number(value) => value.to_string(),
            Primitives::Boolean(value) => value.to_string(),
            Primitives::Array(value) => serialize_jsonvalue(&Primitives::Array(value.to_owned())),
            Primitives::Object(value) => serialize_jsonvalue(&Primitives::Object(value.to_owned())),
            Primitives::Null => "null".to_owned(),
            Primitives::Eof => "".to_owned(),
        }
    }

    pub fn concat_string(&self, rhs: &Primitives) -> String {
        let lhs = self.to_string();
        let rhs = rhs.to_string();
        lhs + &rhs
    }

    pub fn to_integer(&self) -> i64 {
        match self {
            Primitives::Number(value) => value.round() as i64,
            unknow => panic!("Expected integer, got: {:?}", unknow),
        }
    }
    pub fn to_number(&self) -> f64 {
        match self {
            Primitives::Number(value) => *value,
            Primitives::Null => 0.0,
            un => panic!("Expected float, got: {:?}", un),
        }
    }
    pub fn to_boolean(&self) -> bool {
        match self {
            Primitives::Boolean(value) => *value,
            _ => panic!("Expected boolean"),
        }
    }
    pub fn to_array(&self) -> Vec<Primitives> {
        match self {
            Primitives::Array(value) => value.to_owned(),
            _ => panic!("Expected array"),
        }
    }
    pub fn to_object(&self) -> HashMap<String, Primitives> {
        match self {
            Primitives::Object(value) => value.to_owned(),
            _ => panic!("Expected object"),
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Primitives::Null => true,
            _ => false,
        }
    }
    pub fn is_string(&self) -> bool {
        match self {
            Primitives::String(_) => true,
            _ => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            Primitives::Boolean(_) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            Primitives::Array(_) => true,
            _ => false,
        }
    }
    pub fn is_number(&self) -> bool {
        match self {
            Primitives::Number(_) => true,
            _ => false,
        }
    }
    pub fn is_object(&self) -> bool {
        match self {
            Primitives::Object(_) => true,
            _ => false,
        }
    }
    pub fn is_eof(&self) -> bool {
        match self {
            Primitives::Eof => true,
            _ => false,
        }
    }
}

fn serialize_jsonvalue(val: &Primitives) -> String {
    use Primitives::*;

    match val {
        Object(o) => {
            let contents: Vec<_> = o
                .iter()
                .map(|(name, value)| format!("\"{}\":{}", name, serialize_jsonvalue(value)))
                .collect();
            format!("{{{}}}", contents.join(","))
        }
        Array(a) => {
            let contents: Vec<_> = a.iter().map(serialize_jsonvalue).collect();
            format!("[{}]", contents.join(","))
        }
        String(s) => format!("\"{}\"", s),
        Number(n) => format!("{}", n),
        Boolean(b) => format!("{}", b),
        Null => format!("null"),
        Eof => "".to_owned(),
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
