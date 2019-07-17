extern crate nom;
extern crate rustyline;
extern crate core;

use std::cmp::min;

use nom::bytes::complete::tag;
use nom::character::complete::{digit0, digit1, space0, space1, alphanumeric1, alphanumeric0};
use nom::combinator::{complete, opt};
use nom::error::ErrorKind;
use nom::IResult;
use nom::multi::{many0, separated_list, separated_listc, many1};
use nom::sequence::tuple;
use rustyline::Editor;
use rustyline::error::ReadlineError;
use ::LispVal::{Qexpr, Sexpr, Symbol};
use nom::branch::alt;
use std::collections::HashMap;
use rustyline::config::CompletionType::List;
use std::fmt::Debug;
use core::fmt;
use rustyline::config::ColorMode::Enabled;

enum LispVal {
    Float(f64),
    Integer(i64),
    Symbol(String),
    Err(String),
    Sexpr(Vec<LispVal>),
    Qexpr(Vec<LispVal>),
    Function(String, fn(&Vec<Result<LispVal, LispVal>>, &mut Environment) -> Result<LispVal, LispVal>)
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
            LispVal::Function(name, f) => LispVal::Function(name.clone(), *f),
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
            LispVal::Function(i, f) => match other {
                LispVal::Function(i1, f1) => i == i1,
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
            LispVal::Function(name,_) => write!(f, "Function({})",name),
        }
    }
}

struct Environment {
    env: HashMap<String, LispVal>,
}

impl Environment {
    pub fn new() -> Environment{
        Environment{env : HashMap::new()}
    }

    pub fn put(&mut self, name_symbol: LispVal, val: LispVal) {
        match name_symbol {
            LispVal::Symbol(name) => self.env.insert(name, val),
            _ => unimplemented!()
        };
    }

    pub fn get(&self, val: &LispVal) -> Result<LispVal, LispVal> {
        match val {
            LispVal::Symbol(name) => {
                match self.env.get(name) {
                    Some(val) => Ok(val.clone()),
                    None => error(format!("Could not find value for symbol {}", name))
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
    let (i3, dot) = tag(".")(i2)?;
    let (i4, mantissa) = digit1(i3)?;

    let f: String = format!("{}{}{}{}", minus_sign.unwrap_or("") , digit0.unwrap_or("") , "." , mantissa);
    let result = f.parse::<f64>().unwrap();

    Ok((i4, LispVal::Float(result)))
}


fn symbol(input: &str) -> IResult<&str, LispVal> {
    let (inp, result):(&str, Vec<&str>) = many1(alt((tag("+"), tag("*"), tag("/"), tag("-"), alphanumeric1)))(input)?;
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
            LispVal::Function(name, funct) => {
                let argument_results: Vec<Result<LispVal, LispVal>> = elements[1..].iter().map(|e| eval(e, env)).collect();
                funct(&argument_results, env)
            },
            _ =>  error_str(("Tried to evaluate a non function"))
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

fn add(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, add_i64, add_f64) }),
                 env)
}

fn sub(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, sub_i64, sub_f64) }),
                 env)
}

fn mul(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args, e| args[1..].iter().fold(args[0].clone(), |x, y| {
                     number_function_eval(x, y, mul_i64, mul_f64)}),
                 env)
}

fn div(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results,
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

fn list(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results, |args: &Vec<Result<LispVal, LispVal>>, e|  Ok(LispVal::Qexpr(args.iter().map(|x| x.clone().unwrap()).collect())), env)
}

fn head(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment)-> Result<LispVal, LispVal> {
    safe_execute(argument_results, |args, e| {
        match args[0].clone().unwrap() {
            LispVal::Qexpr(elements) => Ok(elements[0].clone()),
            _ =>  Ok(args[0].clone().unwrap())
        }},
                 env)
}

fn tail(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment)-> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args, e| {
                     match args[0].clone().unwrap() {
                         LispVal::Qexpr(elements) => Ok(LispVal::Qexpr(elements[1..].iter().map(|x| x.clone()).collect())),
                         _ => Ok(LispVal::Qexpr(args[1..].iter().map(|x| x.clone().unwrap()).collect()))
                     }
                 }, env)
}

fn isQexpr(val: &LispVal) -> bool{
    match val {
        Qexpr(_) => true,
        _ => false
    }
}

fn isSymbol(val: &LispVal) -> bool {
    match val {
        Symbol(_) => true,
        _ => false
    }
}

fn def(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment) -> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args, e| {
                     // TODO reuben check the clone
                     let values: Vec<LispVal> = args.iter().map(|x| x.clone().unwrap()).collect();
                     if !isQexpr(&values[0]) {
                         return error_str("Expected the first argument to the a Qexpr")
                     }

                     let mut count = 0;
                     let all_symbols = match &values[0] {
                         LispVal::Qexpr(values) => values.iter().map(|x|{
                             count = count + 1;
                             isSymbol(x)
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
                             e.put(x.clone(), values[count].clone());
                             count = count + 1;
                         }),
                         _ => ()
                     };

                     Ok(Sexpr(vec![]))
                 }, env)

}

fn fn_eval(argument_results: &Vec<Result<LispVal, LispVal>>, env: &mut Environment)-> Result<LispVal, LispVal> {
    safe_execute(argument_results,
                 |args: &Vec<Result<LispVal, LispVal>>, e| {
                     let val = match args[0].clone().unwrap() {
                         LispVal::Qexpr(elements) => LispVal::Qexpr(elements),
                         LispVal::Sexpr(elements) => LispVal::Sexpr(elements),
                         v => v
                     };
                     eval(&val, e)
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
        LispVal::Function(name, _) => print!("<function {}>", name)
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

fn build_env() -> Environment {
    let mut environment = Environment::new();
    let add_to_env =  |name: &str, value: fn(&Vec<Result<LispVal, LispVal>>, &mut Environment) -> Result<LispVal, LispVal>,e: &mut Environment|  {
        e.put(LispVal::Symbol(name.to_string()), LispVal::Function(name.to_string(), value));
    };

    add_to_env("+", add, &mut environment);
    add_to_env("-", sub, &mut environment);
    add_to_env("*", mul, &mut environment);
    add_to_env("/", div, &mut environment);
    add_to_env("list", list, &mut environment);
    add_to_env("head", head, &mut environment);
    add_to_env("tail", tail, &mut environment);
    add_to_env("eval", fn_eval, &mut environment);
    add_to_env("def", def, &mut environment);
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
                        match eval(&expr[0], &mut env) {
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

    let x = LispVal::Function("head".to_string(), head);
    rl.save_history("lisp_history.txt");

}

#[cfg(test)]
mod test{
    use nom::IResult;

    use ::{float, integer};
    use ::{number, symbol};
    use ::{LispVal, lispy};
    use ::LispVal::{Float, Integer, Symbol, Sexpr, Qexpr};
    use ::{eval};
    use std::any::Any;
    use ::{build_env, add};
    use ::{def, error_str};

    #[test]
    fn testSimpleParsing() {
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
    fn testExprParsing() {
        let result = lispy("+ 123.12 12");
        assert!(result.is_ok());
        let (remaining, lispVal) = result.unwrap();
        assert_eq!(3, lispVal.len());
        assert_eq!(LispVal::Symbol("+".to_string()), lispVal[0]);
        assert_eq!(LispVal::Float(123.12), lispVal[1]);
        assert_eq!(LispVal::Integer(12), lispVal[2]);

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
        assert_eq!(call_eval("(eval (head {+ - + - * /}))))"), Ok(LispVal::Function("+".to_string(), add)));
        assert_eq!( call_eval("((eval (head {+ - + - * /})) 10 20)"), Ok(Integer(30)));
    }

    fn call_eval(input: &str) -> Result<LispVal, LispVal> {
        let (i, expr) = lispy(input).unwrap();
        let mut env = build_env();
        eval(&expr[0], &mut env)
    }

    #[test]
    fn test_def() {
        let argument_list = vec![Ok(LispVal::Sexpr(vec![]))];
        assert_eq!( def(&argument_list, &mut (build_env())), error_str("Expected the first argument to the a Qexpr"));

        let argument_list = vec![Ok(LispVal::Qexpr(vec![LispVal::Symbol("a".to_string())]))];
        assert_eq!(def(&argument_list, &mut (build_env())), error_str("Number of symbols to values don't match"));


        let argument_list = vec![Ok(LispVal::Qexpr(vec![LispVal::Integer(10)])), Ok(LispVal::Integer(10))];
        assert_eq!(def(&argument_list, &mut (build_env())), error_str("All values of the first argument must be symbols")); //, Ok(Sexpr(vec![])));

        let argument_list = vec![Ok(LispVal::Qexpr(vec![LispVal::Symbol("a".to_string())])), Ok(LispVal::Integer(10))];
        let mut environment = build_env();
        assert_eq!(def(&argument_list, &mut environment), Ok(Sexpr(vec![])));
        let sym= LispVal::Symbol("a".to_string());
        assert_eq!(environment.get(&sym), Ok(Integer(10)));
    }
}