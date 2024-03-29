use crate::ast::*;
use rayon::prelude::*;

#[derive(PartialEq, Debug, Clone)]

pub struct Variable {
    pub ident: String,
    pub value: Expr,
    pub is_const: bool,
    pub context_id: String,
    pub is_func: bool,
}

pub fn get_var_value(
    var_pool: &mut Vec<Variable>,
    mut ident: &str,
    context_id: &str,
) -> Result<Variable, String> {
    let msg = format!(
        "Could not find {}. Context: {context_id} Pool:{:?}.",
        ident, var_pool
    );
    if ident.contains("@") {
        ident = ident.split("@").next().unwrap();
    }

    let var = get_var(var_pool, &ident, context_id);
    if var.is_none() {
        let mut contexts = get_contexts(var_pool);
        contexts.reverse();
        for context in contexts {
            let var = get_var(var_pool, &ident, &context);
            if var.is_some() {
                return Ok(var.unwrap());
            }
        }
        return Err(msg);
    }
    let var = var.unwrap();
    Ok(var)
}

pub fn add_var(
    var_pool: &mut Vec<Variable>,
    ident: &str,
    modifier: &str,
    expr: &Expr,
    context_id: &str,
) -> Variable {
    let ident = String::from(ident);
    let is_assigment = ["const", "let"].contains(&modifier);
    let is_func = ["fn"].contains(&modifier);
    if is_assigment || is_func {
        let var = get_var(var_pool, &ident, context_id);
        if var.is_some() {
            match var {
                Some(var) => {
                    let index = var_pool
                        .par_iter()
                        .position_any(|x| *x.ident == var.ident && x.context_id == context_id);

                    if index.is_none() {
                        let variable = Variable {
                            value: expr.to_owned(),
                            is_const: if modifier == "const" { true } else { false },
                            ident,
                            context_id: context_id.to_owned(),
                            is_func: is_func,
                        };
                        var_pool.push(variable.clone());
                        return variable;
                    }
                    let index = index.unwrap();
                    var_pool[index].value = expr.to_owned();
                    var_pool[index].is_const = if modifier == "const" { true } else { false };
                    var_pool[index].ident = ident.clone();
                    return var_pool[index].clone();
                }

                None => todo!(),
            };
        }
        let variable = Variable {
            value: expr.to_owned(),
            is_const: if modifier == "const" { true } else { false },
            ident,
            context_id: context_id.to_owned(),
            is_func: is_func,
        };
        var_pool.push(variable.clone());
        return variable;
    } else {
        let var = get_var(var_pool, &ident, context_id);
        match var {
            Some(var) => {
                let index = var_pool.par_iter().position_any(|x| *x == var).unwrap();
                var_pool[index].value = expr.to_owned();
                return var_pool[index].clone();
            }
            None => add_var(var_pool, &ident, "let", expr, context_id),
        }
    }
}

pub fn search_in_all_contexts(var_pool: &mut Vec<Variable>, ident: &str) -> Option<Variable> {
    let ident = String::from(ident);
    let mut contexts = get_contexts(var_pool);
    contexts.reverse();
    for context in contexts {
        let var = get_var(var_pool, &ident, &context);
        if var.is_some() {
            return var;
        }
    }
    None
}
pub fn change_var_value(var_pool: &mut Vec<Variable>, ident: &str, expr: &Expr, context_id: &str) {
    let ident = String::from(ident);
    let var = get_var(var_pool, &ident, context_id);
    match var {
        Some(var) => {
            let index = var_pool
                .par_iter()
                .position_any(|x| *x.ident == var.ident && x.context_id == context_id)
                .unwrap();
            var_pool[index].value = expr.to_owned();
        }
        None => {
            let mut contexts = get_contexts(var_pool);
            contexts.reverse();
            println!("contexts: {contexts:?}");
            for context in contexts {
                let var = get_var(var_pool, &ident, &context);
                if var.is_some() {
                    let index = var_pool
                        .par_iter()
                        .position_any(|x| {
                            *x.ident == var.clone().unwrap().ident && *x.context_id == context
                        })
                        .unwrap();
                    var_pool[index].value = expr.to_owned();
                    return;
                }
            }
            panic!("\"{ident}\" not found")
        }
    }
}

pub fn delete_var(var_pool: &mut Vec<Variable>, ident: &str, context_id: &str) {
    var_pool.retain(|x| x.ident != ident && x.context_id == context_id);
}

pub fn delete_context(var_pool: &mut Vec<Variable>, context_id: &str) {
    if context_id == "global" {
        return;
    }
    var_pool.retain(|x| x.context_id != context_id);
}

pub fn get_var(var_pool: &mut Vec<Variable>, ident: &str, context_id: &str) -> Option<Variable> {
    let ident = String::from(ident);
    let variable = var_pool
        .par_iter()
        .filter(|ele| if ele.ident.contains("@"){ele.ident == format!("{context_id}@{ident}")}else{ele.ident == ident} && ele.context_id == context_id);

    let var = variable.find_first(|_| true);
    match var {
        Some(var) => Some(var.clone()),
        None => get_from_global(var_pool, ident),
    }
}

pub fn get_from_global(var_pool: &mut Vec<Variable>, ident: String) -> Option<Variable> {
    let variable = var_pool
        .par_iter()
        .filter(|ele| ele.ident == ident && ele.context_id == "global");
    let var = variable.find_first(|_| true);
    var.cloned()
}

pub fn get_contexts(var_pool: &mut Vec<Variable>) -> Vec<String> {
    let contexts: Vec<String> = var_pool
        .par_iter()
        .map(|ele| ele.context_id.clone())
        .collect();
    contexts
}

pub fn get_all_vars_from_context(var_pool: &mut Vec<Variable>, context_id: &str) -> Vec<Variable> {
    let vars = var_pool
        .par_iter()
        .filter(|ele| ele.context_id == context_id);

    let vars: Vec<Variable> = vars.map(|ele| ele.clone()).collect();

    vars
}
