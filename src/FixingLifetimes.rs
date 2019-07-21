extern crate core;

use std::cell::{RefCell};
use std::collections::HashMap;
use core::fmt;
use std::fmt::Debug;
use std::clone::Clone;


enum LispVal {
    Float(f64),
    Integer(i64),
    Symbol(String),
    Err(String)    ,
    Sexpr(Vec<LispVal>),
    Qexpr(Vec<LispVal>),
    BuiltInFunction(String, fn(&Vec<Result<LispVal, LispVal>>, &mut Environment) -> Result<LispVal, LispVal>),
    UserDefinedFunction(Vec<LispVal>, Vec<LispVal>)
}


#[derive(Clone, Debug)]
struct Environment<'a> {
    env: RefCell<HashMap<String, LispVal>>,
    parent: Option<&'a Environment<'a>>
}

impl Clone for LispVal {
    fn clone(&self) -> Self {
        match self {
            LispVal::Float(f) => LispVal::Float(f.clone()),
            LispVal::Integer(i) => LispVal::Integer(i.clone()),
            LispVal::Symbol(s) => LispVal::Symbol(s.clone()),
            LispVal::Err(s) => LispVal::Err(s.clone()),
            LispVal::Sexpr(exprs) => LispVal::Sexpr(exprs.clone()),
            LispVal::Qexpr(exprs) => LispVal::Qexpr(exprs.clone()),
            LispVal::BuiltInFunction(name, f) => LispVal::BuiltInFunction(name.clone(), *f),
            LispVal::UserDefinedFunction(args, body) => LispVal::UserDefinedFunction(args.clone(), body.clone())
        }
    }
}

impl Debug for LispVal{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispVal::Float(a) =>  write!(f, "Float({})", a),
            LispVal::Integer(i) => write!(f, "Integer({})",i),
            LispVal::Symbol(s) => write!(f, "Symbol({})",s),
            LispVal::Err(s) => write!(f, "Err({})",s),
            LispVal::Sexpr(exprs) => write!(f, "Sexpr({:?})",exprs),
            LispVal::Qexpr(exprs) => write!(f, "Qexpr({:?})",exprs),
            LispVal::BuiltInFunction(name, _) => write!(f, "Builtin Function({})", name),
            LispVal::UserDefinedFunction(args, _) => write!(f, "Function {:?}", &args[0])
        }
    }
}

impl  <'a> Environment<'a> {
    pub fn new<'b>() ->  Environment<'b>{
        Environment{env : RefCell::new(HashMap::new()), parent: Option::None}
    }

    pub fn new_with_map<'b>(map: HashMap<String, LispVal>) ->  Environment<'b>{
        Environment{env : RefCell::new(map), parent: Option::None}
    }

    pub fn put(&mut self, name_symbol: LispVal, val: LispVal) {
        match name_symbol {
            LispVal::Symbol(name) => {
                let mut ref_mut = self.env.borrow_mut();
                ref_mut.insert(name, val);
            },
            _ => unimplemented!()
        };
    }

    pub fn get(&self, val: &LispVal) -> Result<LispVal, LispVal> {
        match val {
            LispVal::Symbol(name)=> {
                let map = self.env.borrow();
                match map.get(name)  {
                    Some(val) => Ok(val.clone()),
                    None => match self.parent {
                        Some(parent_env) => parent_env.get(val),
                        None => error(format!("Could not find value for symbol {}", name))
                    }
                }
            },
            _ => error_str("Could not retrieve variable for type")

        }
    }
}


fn eval(expr: &LispVal, env: &mut Environment) -> Result<LispVal, LispVal> {
    match expr {
        LispVal::Integer(x) => Ok(LispVal::Integer(x.clone())),
        LispVal::Float(x) => Ok(LispVal::Float(x.clone())),
        LispVal::Symbol(sym) => {
            let val = LispVal::Symbol(sym.clone());
            match env.get(&val) {
                Ok(value) => Ok(value),
                Err(value) => error(format!("Symbol {} not found", sym))
            }},
        LispVal::Sexpr(elements) => eval_sexpr(elements, env),
        LispVal::Qexpr(elements) => eval_qexpr(elements),
        LispVal::Err(message)=> error(message.clone()),
        _ => unimplemented!()
    }
}
fn eval_qexpr(elements: &Vec<LispVal>) -> Result<LispVal, LispVal> {
    Ok(LispVal::Qexpr(elements.iter().map(|x| x.clone()).collect()))
}

fn eval_sexpr(elements: &Vec<LispVal>, env: &mut Environment) -> Result<LispVal, LispVal> {
    let x = match &elements[0] {
        LispVal::Sexpr(values) => {
            eval_sexpr(values, env)
        },
        _ => env.get(&elements[0])
    };
    match x {
        Err(_) => error_str("The first element of an sexpr should be a valid symbol"),
        Ok(value) => match value {
            // // LispVal::BuiltInFunction(name, funct) => {
            // //     let argument_results: Vec<Result<LispVal, LispVal>> = elements[1..].iter().map(|e| eval(e, env)).collect();
            // //     funct(&argument_results, env)
            // // },
            _ =>  error_str("Tried to evaluate a non function")
        }
    }
}


fn has_failure(argument_results: &Vec<Result<LispVal, LispVal>>) -> Option<&Result<LispVal, LispVal>> {
    argument_results.iter().find(|x| match x {
        Ok(e) => false,
        Err(e) => true
    })
}

fn safe_execute(argument_results: &Vec<Result<LispVal, LispVal>>,f: fn(&Vec<Result<LispVal, LispVal>>, &mut Environment) -> Result<LispVal, LispVal>, env: &mut Environment) -> Result<LispVal, LispVal> {
    let failure = has_failure(argument_results);
    if failure.is_some() {
        failure.unwrap().clone()
    } else {
        f(argument_results, env)
    }
}

fn main(){}


fn error(message: String) -> Result<LispVal, LispVal> {
    Err(LispVal::Err(message))
}

fn error_str(message: &str) -> Result<LispVal, LispVal> {
    error(message.to_string())
}
