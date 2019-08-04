extern crate core;
extern crate nom;
extern crate rustyline;

use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit0, digit1, space0, space1};
use nom::combinator::opt;
use nom::IResult;
use nom::multi::{many0, many1, separated_list};
use nom::sequence::tuple;
use rustyline::Editor;
use rustyline::error::ReadlineError;

use ::LispVal::{Qexpr, Sexpr, Symbol};

enum LispVal {
    Float(f64),
    Integer(i64),
    Symbol(String),
    Err(String),
    Sexpr(Vec<LispVal>),
    Qexpr(Vec<LispVal>),
    BuiltInFunction(String, fn(&Vec<LispVal>, Rc<RefCell<Environment>>) -> Result<LispVal, LispVal>),
    UserDefinedFunction(Vec<LispVal>, Vec<LispVal>, Option<Rc<RefCell<Environment>>>)
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
            LispVal::UserDefinedFunction(args, body, parent) => LispVal::UserDefinedFunction(args.clone(), body.clone(), parent.clone())
        }
    }
}

impl PartialEq for LispVal{
    fn eq(&self, other: &LispVal) -> bool {
        match self {
            LispVal::Float(f) => match other {
                LispVal::Float(f1) => f == f1,
                _ => false
            },
            LispVal::Integer(i) => match other {
                LispVal::Integer(i1) => i == i1,
                _ => false
            },
            LispVal::Err(i) => match other {
                LispVal::Err(i1) => i == i1,
                _ => false
            },
            LispVal::Symbol(i) => match other {
                LispVal::Symbol(i1) => i == i1,
                _ => false
            },
            LispVal::Sexpr(i) => match other {
                LispVal::Sexpr(i1) => i == i1,
                _ => false
            },
            LispVal::Qexpr(i) => match other {
                LispVal::Qexpr(i1) => i == i1,
                _ => false
            },
            LispVal::BuiltInFunction(i, f) => match other {
                LispVal::BuiltInFunction(i1, f1) => i == i1,
                _ => false
            },
            LispVal::UserDefinedFunction(args, body, _) => match other {
                LispVal::UserDefinedFunction(args1, body1, _) => {
                    args.iter().zip(args1.iter()).map(|(x, y)| x == y).fold(true, |x, y| x && y)
                        && body.iter().zip(body1.iter()).map(|(x, y)| x == y).fold(true, |x, y| x && y)
                },
                _ => false
            }
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
            LispVal::UserDefinedFunction(args, _, _) => write!(f, "Function {:?}", &args[0])
        }
    }
}

#[derive(Clone, Debug)]
struct Environment {
    env: RefCell<HashMap<String, LispVal>>,
    parent: Option<Rc<RefCell<Environment>>>
}


impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment { env: RefCell::new(HashMap::new()), parent: Option::None }))
    }

    pub fn new_with_map(map: HashMap<String, LispVal>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment { env: RefCell::new(map), parent: Option::None }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<Environment>>)  -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment { env: RefCell::new(HashMap::new()), parent: Some(parent)}))
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
                    None => match &self.parent {
                        Some(parent_env) => parent_env.borrow().get(val),
                        None => error(format!("Could not find value for symbol {}", name))
                    }
                }
            },
            _ => error_str("Could not retrieve variable for type")
        }
    }

}

/* ----------------------------- PARSER ----------------------------------*/
fn minus_sign(input: &str) -> IResult<&str, Option<&str>> {
    opt(tag("-"))(input)
}

fn float(input: &str) -> IResult<&str, LispVal> {
    let (i1, minus_sign) = minus_sign(input)?;
    let (i2, digit0) = opt(digit0)(i1)?;
    let (i3, _) = tag(".")(i2)?;
    let (i4, mantissa) = digit1(i3)?;

    let f: String = format!("{}{}{}{}", minus_sign.unwrap_or("") , digit0.unwrap_or("") , "." , mantissa);
    let result = f.parse::<f64>().unwrap();

    Ok((i4, LispVal::Float(result)))
}


fn symbol(input: &str) -> IResult<&str, LispVal> {
    let (inp, result):(&str, Vec<&str>) = many1(alt((tag("&"), tag("+"), tag("*"), tag("/"), tag("-"),tag("="), tag("\\"), alphanumeric1)))(input)?;
    let i_name: Vec<String> = result.iter().map(|x| x.to_string()).collect();
    Ok((inp, LispVal::Symbol(i_name.join(""))))
}

fn integer(input: &str) -> IResult<&str, LispVal> {
    let (i1, minus_sign) = minus_sign(input)?;
    let (i2, result) = digit1(i1)?;
    let i = result.parse::<i64>().unwrap();

    Ok((i2, LispVal::Integer(minus_sign.map(|_| i * -1)
        .or_else(|| Some(i)).unwrap())))
}

fn number(input:  &str) -> IResult<&str, LispVal> {
    alt(( float, integer))(input)
}

fn sexpr(input: &str) -> IResult<&str, LispVal> {
    let (inp, (t, s0, op, s, nums, t1)) = tuple((tag("("),space0,  expr, space0, separated_list(space1, expr), tag(")")))(input)?;
    let mut vec: Vec<LispVal> = Vec::new();
    vec.push(op);
    vec.extend(nums.iter().cloned());
    Ok((inp, LispVal::Sexpr(vec)))
}

//fn func_parameter_qexpr(input: &str) -> IResult<&str, LispVal> {
//    let (inp, (t, s0, op, s, nums, t1)) = tuple((tag("{"),space0,  symbol, space0, separated_list(space1, symbol), tag("}")))(input)?;
//    let mut vec: Vec<LispVal> = Vec::new();
//    vec.push(op);
//    vec.extend(nums.iter().cloned());
//    Ok((inp, LispVal::Qexpr(vec)))
//}


//fn lambda(input: &str) -> IResult<&str, LispVal> {
//    let (inp, (lmbda, s, args, s1,body)) = tuple((tag("\\"), space1, func_parameter_qexpr,space1, qexpr))(input)?;
//    Ok((inp, LispVal::UserDefinedFunction(vec![args], vec![body], Option::None)))
//}

fn qexpr(input: &str) -> IResult<&str, LispVal> {
    let (inp, (t, s0, op, s, nums, t1)) = tuple((tag("{"),space0,  expr, space0, separated_list(space1, expr), tag("}")))(input)?;
    let mut vec: Vec<LispVal> = Vec::new();
    vec.push(op);
    vec.extend(nums.iter().cloned());
    Ok((inp, LispVal::Qexpr(vec)))
}

fn expr(input: &str) -> IResult<&str, LispVal> {
    let result = alt((number, symbol, sexpr, qexpr))(input);
    result
}

fn space_expr(input: &str) -> IResult<&str, LispVal> {
    let (i, _) = space1(input)?;
    let (i, val) = expr(i)?;

    Ok((i, val))
}

fn lispy(input: &str) -> IResult<&str, Vec<LispVal>> {
    let (i, (e1, e2))= tuple((expr, many0(space_expr)))(input)?;
    let mut return_val:Vec<LispVal> = Vec::new();
    return_val.push(e1);
    return_val.extend(e2);
    Ok((i, return_val))
}

fn parse(input: &str) -> IResult<&str, Vec<LispVal>> {
    lispy(input)
}

/* -----------------------  UTIL FUNCTIONS -------------------------------*/

fn error(message: String) -> Result<LispVal, LispVal> {
    Err(LispVal::Err(message))
}

fn error_str(message: &str) -> Result<LispVal, LispVal> {
    error(message.to_string())
}

/* -----------------------  LISP -------------------------------*/

fn eval(expr: &LispVal, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    match expr {
        LispVal::Integer(x) => Ok(LispVal::Integer(x.clone())),
        LispVal::Float(x) => Ok(LispVal::Float(x.clone())),
        LispVal::Symbol(sym) => {
            let val = LispVal::Symbol(sym.clone());
            match env.borrow().get(&val) {
                Ok(value) => Ok(value),
                Err(value) => error(format!("Symbol {} not found", sym))
            }
        },
        LispVal::Sexpr(elements) => eval_sexpr(elements, env),
        LispVal::Qexpr(elements) => eval_qexpr(elements),
        LispVal::Err(message)=> error(message.clone()),
        LispVal::UserDefinedFunction(args, body, parent) => Ok(LispVal::UserDefinedFunction(args.clone(), body.clone(), parent.clone())),
        LispVal::BuiltInFunction(name, f) => Ok(LispVal::BuiltInFunction(name.clone(), *f)),
        _ => unimplemented!()
    }
}

fn user_defined_function_eval(elements: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    unimplemented!()
}

fn eval_qexpr(elements: &Vec<LispVal>) -> Result<LispVal, LispVal> {
    Ok(LispVal::Qexpr(elements.iter().map(|x| x.clone()).collect()))
}

fn eval_sexpr(elements: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    let x = match &elements[0] {
        LispVal::Sexpr(values) => {
            eval_sexpr(values, env.clone())
        },
        LispVal::UserDefinedFunction(name, body, local_env) => {
            return Ok(LispVal::UserDefinedFunction(name.clone(), body.clone(), local_env.clone()))
        },
        LispVal::BuiltInFunction(name, funct) => Ok(elements[0].clone()),
        _ => env.borrow().get(&elements[0])
    };
    match x {
        Err(_) => error_str("The first element of an sexpr should be a valid symbol"),
        Ok(value) => match value {
            LispVal::BuiltInFunction(name, funct) => {

                let argument_results: Vec<LispVal> = if elements.len() > 1 {
                    elements[1..].iter().map(|x| x.clone()).collect()
                } else {
                    vec![]
                };

                funct(&argument_results, env.clone())
            },
            LispVal::UserDefinedFunction(parameters, body, local_env) => {
                let argument_results: Vec<Result<LispVal, LispVal>> = elements[1..].iter().map(|e| eval(e, env.clone())).collect();
                eval_function(parameters, body, &argument_results, local_env, env.clone())
            },
            _ =>  error_str("Tried to evaluate a non function")
        }
    }
}

fn eval_function (mut parameters: Vec<LispVal>, body: Vec<LispVal>, arguments: &Vec<Result<LispVal, LispVal>>, local_env: Option<Rc<RefCell<Environment>>>, global_env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    let mut result_env = if local_env.is_some() {
        local_env.unwrap().clone()
    } else {
        Environment::new_with_parent(global_env.clone())
    };


    let args_len = arguments.len();
    let mut args_count = 0;
    let mut params_len = 0;
    let mut function_params = match parameters.get(0){
        Some(Qexpr(param_list)) => {
            params_len = param_list.len();
            param_list.clone()
        },
        Some(_) => unimplemented!(),
        None => unimplemented!() // TODO fix this should be the case for function without arguments
    };
    let mut args_list: Vec<LispVal> = arguments.iter().map(|x| x.clone().unwrap()).collect();
    let mut var_args_found = false;
    for i in 0..args_len {
        let param = function_params.get(0).unwrap();
        if *param == Symbol("&".to_string()) {
            function_params.remove(0);
            var_args_found = true;
            break;
        }

        let arg = args_list.get(0).unwrap();
        args_count = args_count + 1;
        result_env.borrow_mut().put(param.clone(), arg.clone());
        function_params.remove(0);
        args_list.remove(0);
    }

    if var_args_found {
        let param = function_params.get(0).unwrap();
        result_env.borrow_mut().put(param.clone(), Qexpr(args_list));
        function_params.remove(0);
    }

    if args_len< params_len  {
        return Ok(LispVal::UserDefinedFunction(vec![LispVal::Qexpr(function_params)], body, Some(result_env)));
    } else {
        let body_sexpr = match body[0].clone() {
            LispVal::Qexpr(elements) => elements,
            _ => unimplemented!()
        };

        if body_sexpr.len() == 1 {
            eval(body_sexpr.get(0).unwrap(), result_env.clone())
        } else {
            eval(&LispVal::Sexpr(body_sexpr), result_env.clone())
        }
    }
}


fn has_failure(argument_results: &Vec<Result<LispVal, LispVal>>) -> Option<&Result<LispVal, LispVal>> {
    argument_results.iter().find(|x| match x {
        Ok(e) => false,
        Err(e) => true
    })
}

fn eval_args_and_safe_execute(arguments: &Vec<LispVal>, f: fn(&Vec<Result<LispVal, LispVal>>, Rc<RefCell<Environment>>) -> Result<LispVal, LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    let argument_results: Vec<Result<LispVal, LispVal>> = arguments[0..].iter().map(|e| eval(e, env.clone())).collect();
    let failure = has_failure(&argument_results);
    if failure.is_some() {
        failure.unwrap().clone()
    } else {
        f(&argument_results, env)
    }
}

fn add(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, add_i64, add_f64) }),
                               env)
}

fn sub(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, sub_i64, sub_f64) }),
                               env)
}

fn mul(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, mul_i64, mul_f64)}),
                               env)
}

fn builtin_lambda(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| {
                     if args.len() < 2 {
                         return error_str("Lambda needs parameters & body")
                     }
                     let parameters = args.get(0).unwrap().clone();
                     let body = args.get(1).unwrap().clone();

                     let parameter_unwrapped = parameters.unwrap();
                     let body_unwrapped = body.unwrap();
                     Ok(LispVal::UserDefinedFunction(vec![parameter_unwrapped], vec![body_unwrapped], Some(Environment::new_with_parent(e.clone()))))
//                     Ok(LispVal::Integer(10))
                 }, env)
}

fn div(argument_results: &Vec<LispVal>, env:Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| {
                     args[1..].iter().fold(args[0].clone(), |x, y| {
                         let divisor_is_zero = match y.clone().unwrap() {
                             LispVal::Integer(z) => z == 0,
                             LispVal::Float(z) => z == 0.0,
                             _ => unimplemented!()
                         };

                         if divisor_is_zero {
                             error_str("Divide by zero")
                         } else {
                             number_function_eval(x, y, div_i64, div_f64)
                         }})},
                               env)
}

fn list(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results, |args: &Vec<Result<LispVal, LispVal>>, e|  Ok(LispVal::Qexpr(args.iter().map(|x| x.clone().unwrap()).collect())), env)
}

fn head(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>)-> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results, |args, e| {
        match args[0].clone().unwrap() {
            LispVal::Qexpr(elements) => Ok(LispVal::Qexpr(vec![elements[0].clone()])),
            _ =>  Ok(LispVal::Qexpr(vec![args[0].clone().unwrap()]))
        }},
                               env)
}

fn tail(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>)-> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| {
                     match args[0].clone().unwrap() {
                         LispVal::Qexpr(elements) => Ok(LispVal::Qexpr(elements[1..].iter().map(|x| x.clone()).collect())),
                         _ => Ok(LispVal::Qexpr(args[1..].iter().map(|x| x.clone().unwrap()).collect()))
                     }
                 }, env)
}

fn join(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>)-> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| {
                     let mut return_val: Vec<LispVal> = vec![];
                     for arg in args {
                         match arg.clone().unwrap() {
                             Qexpr(elements) => (&mut return_val).extend(elements.iter().cloned()),
                             _ => return error_str("All elements to join expected to the Qexprs")
                         }
                     }
                     Ok(Qexpr(return_val))
                 }, env)
}


fn is_qexpr(val: &LispVal) -> bool{
    match val {
        Qexpr(_) => true,
        _ => false
    }
}

fn is_symbol(val: &LispVal) -> bool {
    match val {
        Symbol(_) => true,
        _ => false
    }
}

fn def_local(argument_results: &Vec<LispVal>, env:Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    def(argument_results, env.clone())
}

fn def_global(argument_results:&Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    if env.borrow().parent.is_none() {
        return def(argument_results, env);
    } else {
        let p = &env.borrow().parent;
        match  p {
            Some(parent) => {
                def(argument_results, parent.clone())
            }
            None => return error_str("Unexpted case")
        }
    }
}


fn def(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>) -> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args, e| {
                     // TODO reuben check the clone
                     let values: Vec<LispVal> = args.iter().map(|x| x.clone().unwrap()).collect();
                     if !is_qexpr(&values[0]) {
                         return error_str("Expected the first argument to the a Qexpr")
                     }

                     let mut count = 0;
                     let all_symbols = match &values[0] {
                         LispVal::Qexpr(values) => values.iter().map(|x|{
                             count = count + 1;
                             is_symbol(x)
                         } ).fold(true, |x, y| x && y),
                         _ => false
                     };
                     if !all_symbols {
                         return error_str("All values of the first argument must be symbols")
                     }

                     if values.len() == count {
                         return error_str("Number of symbols to values don't match")
                     }

                     count = 1;
                     match &values[0] {
                         LispVal::Qexpr(vs) => vs.iter().for_each(|x| {
                             e.borrow_mut().put(x.clone(), values[count].clone());
                             count = count + 1;
                         }),
                         _ => ()
                     };
                     Ok(Sexpr(vec![]))
                 }, env)

}

fn fn_eval(argument_results: &Vec<LispVal>, env: Rc<RefCell<Environment>>)-> Result<LispVal, LispVal> {
    eval_args_and_safe_execute(argument_results,
                               |args: &Vec<Result<LispVal, LispVal>>, e| {
                     let val = args[0].clone().unwrap();
                     match val {
                         Qexpr(qelements) => {
                             if qelements.len() == 1 {
                                 eval(&qelements[0], e)
                             } else {
                                 eval(&Sexpr(qelements), e)
                             }
                         }
                         _ => error_str("First argument to eval should be a qexpr")
                     }
                 }, env)
}

fn div_i64(a: i64, b: i64) -> i64{
    a/b
}

fn div_f64(a: f64, b: f64) -> f64{
    a/b
}

fn mul_i64(a: i64, b: i64) -> i64{
    a*b
}

fn mul_f64(a: f64, b: f64) -> f64{
    a*b
}

fn sub_i64(a: i64, b: i64) -> i64{
    a-b
}

fn sub_f64(a: f64, b: f64) -> f64{
    a-b
}

fn add_i64(a: i64, b: i64) -> i64{
    a+b
}

fn add_f64(a: f64, b: f64) -> f64{
    a+b
}
fn number_function_eval(x: Result<LispVal, LispVal>, y: &Result<LispVal, LispVal>, f: fn(i64, i64) -> i64,g: fn(f64, f64) -> f64) -> Result<LispVal, LispVal> {
    if x.is_err() {
        x
    } else {
        match x.unwrap() {
            LispVal::Integer(x2) => {
                let num = x2;
                match y {
                    Ok(val) => {
                        match val {
                            LispVal::Integer(y) => {
                                Ok(LispVal::Integer(f(num, *y)))
                            },
                            LispVal::Float(y) => {
                                Ok(LispVal::Float(g(num as f64, *y)))
                            },
                            _ => unimplemented!()
                        }
                    },
                    _ => unimplemented!()
                }
            },
            LispVal::Float(x2) => {
                let num = x2;
                match y.clone().unwrap() {
                    LispVal::Integer(y) => {
                        Ok(LispVal::Float(g(num, y as f64)))
                    },
                    LispVal::Float(y) => {
                        Ok(LispVal::Float(g(num, y)))
                    },
                    _ => error_str("Operation on unsupported type")
                }
            },
            LispVal::Err(s) => error(s.clone()),
            _ => error_str("Unexpected type for math operation")
        }
    }
}

fn print_val(expr: &LispVal) {
    match expr {
        LispVal::Integer(x) => print!("{}", x),
        LispVal::Float(x) => print!("{}", x),
        LispVal::Err(message) => print!("Error: {}", message),
        LispVal::Symbol(x) => print!("{}", x),
        LispVal::Sexpr(elements) => print_sexprs(elements.to_vec(), "(", ")", true),
        LispVal::Qexpr(elements) => print_sexprs(elements.to_vec(), "{", "}", true),
        LispVal::BuiltInFunction(name, _) => print!("<builtin function {}>", name),
        LispVal::UserDefinedFunction(name, _, _) => print!("<user defined {}>", format!("{:?}", &name[0]))
    }
}

fn print_expr(expr: &Vec<LispVal>, recursive: bool) {
    for e in expr.iter() {
        print_val(e);
        print!(" ");
    }
    if !recursive {
        println!()
    }
}

fn print_sexprs(elements: Vec<LispVal>, opening: &str, closing: &str, recursive: bool) {
    print!("{}", opening);
    print_expr(&elements, recursive);
    print!("{}", closing);
    if !recursive {
        println!()
    }
}

fn build_env<'a>() -> Rc<RefCell<Environment>> {
    let mut environment = Environment::new();
    let add_to_env =  |name: &str, value: fn(&Vec<LispVal>,  Rc<RefCell<Environment>>) -> Result<LispVal, LispVal>,e: Rc<RefCell<Environment>>|  {
        e.borrow_mut().put(LispVal::Symbol(name.to_string()), LispVal::BuiltInFunction(name.to_string(), value));
    };

    add_to_env("+", add, environment.clone());
    add_to_env("-", sub, environment.clone());
    add_to_env("*", mul, environment.clone());
    add_to_env("/", div, environment.clone());
    add_to_env("list", list, environment.clone());
    add_to_env("head", head, environment.clone());
    add_to_env("tail", tail, environment.clone());
    add_to_env("eval", fn_eval, environment.clone());
    add_to_env("def", def_global, environment.clone());
    add_to_env("=", def_local, environment.clone());
    add_to_env("join", join, environment.clone());
    add_to_env("\\", builtin_lambda, environment.clone());
    environment
}

fn main() {
    let mut rl = Editor::<()>::new();
    rl.load_history("lisp_history.txt");
    let mut env = build_env();
    loop {
        let readline = rl.readline("lisp> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match parse(&line) {
                    Ok((str, expr)) => {
                        match eval(&expr[0], env.clone()) {
                            Ok(value) => print_expr(&(vec![value]), false),
                            Err(value) => print_expr(&(vec![value]), false)
                        }
                    },
                    Err(error) => println!("Error {:?}", error),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Bye Fren!!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye Fren!!");
                break;
            }
            Err(err) => {
                println!("Why you do this, Fren??");
                break;
            }
        }
    }

    let x = LispVal::BuiltInFunction("head".to_string(), head);
    rl.save_history("lisp_history.txt");

}

#[cfg(test)]
mod test{
    use std::any::Any;

    use nom::IResult;

    use ::{float, integer};
    use ::{number, symbol};
    use ::{LispVal, lispy};
    use ::{eval, join};
    use ::{add, build_env};
    //    use ::{def, error_str};
    use ::LispVal::{Float, Integer, Qexpr, Sexpr, Symbol, UserDefinedFunction};
    use ::{Environment, error_str};
    use ::{def_global, def_local};
    use def;


    #[test]
    fn simple_expr_parsing() {
        assert_eq!(unwrap_successful(integer("12")), Integer(12));
        assert_eq!(unwrap_successful(integer("-12")), Integer(-12));

        assert_eq!(unwrap_successful(float("-12.0")), Float(-12.0));
        assert_eq!(unwrap_successful(float("-0.12")), Float(-0.12));
        assert_eq!(unwrap_successful(float("-.12")), Float(-0.12));
        assert_eq!(unwrap_successful(float(".12")), Float(0.12));

        assert_eq!(unwrap_successful(symbol("+")), Symbol("+".to_string()));

        assert_eq!(unwrap_successful(number("12")), Integer(12));
        assert_eq!(unwrap_successful(number("-.12")), Float(-0.12));
    }

    fn unwrap_successful(x: IResult<&str, LispVal>) -> LispVal {
        assert!(x.is_ok());
        x.unwrap().1
    }

    #[test]
    fn test_expr_parsing() {
        let result = lispy("+ 123.12 12");
        assert!(result.is_ok());
        let (remaining, lisp_val) = result.unwrap();
        assert_eq!(3, lisp_val.len());
        assert_eq!(LispVal::Symbol("+".to_string()), lisp_val[0]);
        assert_eq!(LispVal::Float(123.12), lisp_val[1]);
        assert_eq!(LispVal::Integer(12), lisp_val[2]);

        let result = lispy("(/ 1 1 (/ 1 0) 1)");
        assert!(result.is_ok());
        let (remaining, value) = result.unwrap();
        assert_eq!(vec![Sexpr(vec![Symbol("/".to_string()), Integer(1), Integer(1), Sexpr(vec![Symbol("/".to_string()), Integer(1), Integer(0)]), Integer(1)])], value);
    }

    #[test]
    fn math_test() {
        assert_eq!(Ok(Integer(3)), call_eval("(eval (head {(+ 1 2) (+ 10 20)}))"));
        assert_eq!(Ok(Float(1.2)), call_eval("(+ 1 0.2)"));
        assert_eq!(Ok(Float(1.4)), call_eval("(+ 1.2 0.2)"));
        assert_eq!(Ok(Float(1.2)), call_eval("(+ 1.2 0)"));

        assert_eq!( call_eval("(+ 1 1)"), Ok(Integer(2)));
    }

    #[test]
    fn list_test() {
        assert_eq!(call_eval("(list 123 123)"), Ok(Qexpr(vec![Integer(123), Integer(123)])))
    }

    #[test]
    fn eval_test() {
        assert_eq!(call_eval("(eval (head {+ - + - * /}))))"), Ok(LispVal::BuiltInFunction("+".to_string(), add)));
        assert_eq!( call_eval("((eval (head {+ - + - * /})) 10 20)"), Ok(Integer(30)));
    }

    fn call_eval(input: &str) -> Result<LispVal, LispVal> {
        let (i, expr) = lispy(input).unwrap();
        let mut env = build_env();
        eval(&expr[0], env.clone())
    }

    #[test]
    fn test_def() {
        let argument_list = vec![LispVal::Integer(1)];
        assert_eq!( def(&argument_list,build_env()), error_str("Expected the first argument to the a Qexpr"));

        let argument_list = vec![LispVal::Qexpr(vec![LispVal::Symbol("a".to_string())])];
        assert_eq!(def(&argument_list, build_env()), error_str("Number of symbols to values don't match"));

        let argument_list = vec![LispVal::Qexpr(vec![LispVal::Integer(10)]), LispVal::Integer(10)];
        assert_eq!(def(&argument_list, build_env()), error_str("All values of the first argument must be symbols")); //, Ok(Sexpr(vec![])));

        let argument_list = vec![LispVal::Qexpr(vec![LispVal::Symbol("a".to_string())]), LispVal::Integer(10)];
        let mut environment = build_env();
        assert_eq!(def(&argument_list, environment.clone()), Ok(Sexpr(vec![])));
        let sym= LispVal::Symbol("a".to_string());
        assert_eq!(environment.borrow().get(&sym), Ok(Integer(10)));
    }

    #[test]
    fn test_join(){
        let args = vec![get_argument_qexpr(vec!["1", "2"]), get_argument_qexpr(vec!["3", "4"]), get_argument_qexpr(vec!["5", "6"])];
        let mut environment = build_env();
        assert_eq!(join(&args, environment.clone() ), Ok(Qexpr(vec![Symbol("1".to_string()), Symbol("2".to_string()), Symbol("3".to_string()), Symbol("4".to_string()), Symbol("5".to_string()), Symbol("6".to_string())])))
    }

    fn get_argument_qexpr(strs: Vec<&str>) -> LispVal {
        LispVal::Qexpr(get_symbol_vector(strs))
    }

    fn get_symbol_vector(strs: Vec<&str>) -> Vec<LispVal> {
        strs.iter().map(|x| Symbol(x.to_string())).collect()
    }

    #[test]
    fn test_parse_lambda() {
        assert_eq!(lispy("\\ {a} {b}").unwrap(),
                   ("",
                    vec![LispVal::Symbol("\\".to_string()),
                         LispVal::Qexpr(vec![LispVal::Symbol("a".to_string())]),
                         LispVal::Qexpr(vec![LispVal::Symbol("b".to_string())])]));
    }

    #[test]
    fn test_environment_get() {
        let mut e1 = build_env();
        e1.borrow_mut().put(LispVal::Symbol("a".to_string()), LispVal::Symbol("b".to_string()));
        let mut e2 =  Environment::new_with_parent(e1.clone());
        let val = LispVal::Symbol("a".to_string());
        let val1 = LispVal::Symbol("c".to_string());

        assert_eq!(e2.borrow().get(&val), Ok(Symbol("b".to_string())));
        assert_eq!(e2.borrow().get(&val1), Err(LispVal::Err("Could not find value for symbol c".to_string())));
    }

    #[test]
    fn test_environment_def() {
        let mut e1 = build_env();
        let val = LispVal::Symbol("a".to_string());

        {
            let mut e2 = Environment::new_with_parent(e1.clone());
            let val1 = LispVal::Symbol("c".to_string());

            let arguments = vec![get_argument_qexpr(vec!["a"]), Integer(1)];
            def_global(&arguments, e2.clone());
            assert_eq!(e2.borrow().get(&val), Ok(Integer(1)));

            let arguments = vec![get_argument_qexpr(vec!["b"]), Integer(3)];
            let val1 = LispVal::Symbol("b".to_string());
            def_local(&arguments,e2.clone());
            assert_eq!(e2.borrow().get(&val1), Ok(Integer(3)));
            assert_eq!(e1.borrow().get(&val1), error_str("Could not find value for symbol b"));

        }
        assert_eq!(e1.borrow().get(&val), Ok(Integer(1)));

    }

    #[test]
    fn test_environment_with_parent() {
        let mut e1 = build_env();
        let val = LispVal::Symbol("a".to_string());

        {
            let mut e2 = Environment::new_with_parent(e1.clone());
            let mut e3 = Environment::new_with_parent(e1.clone());

            let arguments = vec![get_argument_qexpr(vec!["a"]), Integer(1)];
            def_global(&arguments, e2.clone());
            assert_eq!(e2.borrow().get(&val), Ok(Integer(1)));
            assert_eq!(e3.borrow().get(&val), Ok(Integer(1)));

            let arguments = vec![get_argument_qexpr(vec!["a"]), Integer(2)];
            def_global(&arguments, e3.clone());
            assert_eq!(e2.borrow().get(&val), Ok(Integer(2)));
            assert_eq!(e3.borrow().get(&val), Ok(Integer(2)));

            let arguments = vec![get_argument_qexpr(vec!["b"]), Integer(3)];
            let val1 = LispVal::Symbol("b".to_string());
            def_local(&arguments, e2.clone());
            assert_eq!(e2.borrow().get(&val1), Ok(Integer(3)));
            assert_eq!(e1.borrow().get(&val1), error_str("Could not find value for symbol b"));
            assert_eq!(e3.borrow().get(&val1),  error_str("Could not find value for symbol b"));
        }
        assert_eq!(e1.borrow().get(&val), Ok(Integer(2)));

    }

    #[test]
    fn test_eval_user_defined_function() {
        assert_eq!( Ok(UserDefinedFunction(vec![Qexpr(vec![Symbol("a".to_string())])], vec![Qexpr(vec![Symbol("b".to_string())])], Some(build_env()))), call_eval("(\\ {a} {b})"));
        assert_eq!(Ok(LispVal::Integer(5)), call_eval("((\\ {x} {x}) 5)"));

        let mut env = build_env();
        let (i, expr) = lispy("(def {add-mul} (\\ {x y} {+ x (* x y)}))").unwrap();
        eval(&expr[0], env.clone());
        let (i, expr) = lispy("(add-mul 10 20)").unwrap();
        assert_eq!(Ok(LispVal::Integer(210)), eval(&expr[0], env.clone()));

        assert_eq!(Ok(LispVal::Integer(2)), call_eval("(((\\ {a b} {(+ a b)}) 1) 1)"));
    }

    #[test]
    fn test_variable_argument_functions() {
        assert_eq!(Ok(LispVal::Integer(3)), call_eval("((\\ {a & b} {(+ a (eval (head b)))}) 1 2 3)"));
        assert_eq!(Ok(LispVal::Integer(3)), call_eval("(eval (join (list +) {1 2}))"));
    }

}