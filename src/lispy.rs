extern crate nom;
extern crate rustyline;

use std::cmp::min;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit0, digit1, space0, space1};
use nom::combinator::{complete, opt};
use nom::error::ErrorKind;
use nom::IResult;
use nom::multi::{many0, separated_list, separated_listc};
use nom::sequence::tuple;
use rustyline::Editor;
use rustyline::error::ReadlineError;

#[derive(PartialEq, Debug, Clone)]
enum LispVal {
    Float(f64),
    Integer(i64),
    Symbol(String),
    Err(String),
    Sexpr(Vec<LispVal>)
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
    let result: IResult<&str, &str> = alt((tag("+"), tag("-"), tag("/"), tag("-")))(input);
    let (i, symbol) = result?;
    Ok((i, LispVal::Symbol(symbol.to_string())))
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
    let (inp, (t, s0, op, s, nums, t1)) = tuple((tag("("),space0,  expr, space1, separated_list(space1, expr), tag(")")))(input)?;
    let mut vec: Vec<LispVal> = Vec::new();
    vec.push(op);
    vec.extend(nums.iter().cloned());
    Ok((inp, LispVal::Sexpr(vec)))
}

fn expr(input: &str) -> IResult<&str, LispVal> {
    let result = alt((number, symbol, sexpr))(input);
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

/* -----------------------  LISP -------------------------------*/

fn eval(expr: &LispVal) -> Result<LispVal, LispVal> {
    match expr {
        LispVal::Integer(x) => Ok(LispVal::Integer(x.clone())),
        LispVal::Float(x) => Ok(LispVal::Float(x.clone())),
        LispVal::Symbol(sym) => Ok(LispVal::Symbol(sym.clone())),
        LispVal::Sexpr(elements) => eval_sexpr(elements),
        LispVal::Err(message)=> Err(LispVal::Err(message.clone())),
        _ => unimplemented!()
    }
}

fn eval_sexpr(elements: &Vec<LispVal>) -> Result<LispVal, LispVal> {
    let (symbol, is_symbol) = match &elements[0] {
        LispVal::Symbol(sym) => match sym.clone().as_str() {
            "+" => ("+", true),
            "-" => ("-", true),
            "*" => ("*", true),
            "/" => ("/", true),
            _ => ("", false)
        },
        _ => ("", false)
    };

    if !is_symbol {
        return Err(LispVal::Err("The first element of an sexpr should be a valid symbol".to_string()));
    }

    let argument_results: Vec<Result<LispVal, LispVal>> = elements[1..].iter().map(|e| eval(e)).collect();

    match symbol {
        "+" => argument_results[1..].iter().fold(argument_results[0].clone(), |x, y|  {
            number_function_eval(x,y, add_i64, add_f64)
        }),
        "-" => argument_results[1..].iter().fold(argument_results[0].clone(), |x, y|  {
            number_function_eval(x,y, sub_i64, sub_f64)
        }),
        "*" => argument_results[1..].iter().fold(argument_results[0].clone(), |x, y|  {
            number_function_eval(x,y, mul_i64, mul_f64)
        }),
        "/" => argument_results[1..].iter().fold(argument_results[0].clone(), |x, y|  {

            let divisor_is_zero = match y.clone().unwrap() {
                LispVal::Integer(z) => z == 0,
                LispVal::Float(z) => z == 0.0,
                _ => unimplemented!()
            };

            if divisor_is_zero {
                Err(LispVal::Err("Divide by zero".to_string()))
            } else {
                number_function_eval(x, y, div_i64, div_f64)
            }
        }),
        _ => unimplemented!()
    }
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
                let num = x2.clone();
                match y.clone().unwrap() {
                    LispVal::Integer(y) => {
                        let y_clone = y.clone();
                        Ok(LispVal::Integer(f(num, y_clone)))
                    },
                    LispVal::Float(y) => {
                        let y_clone = y.clone();
                        Ok(LispVal::Float(g(num as f64, y_clone)))
                    },
                    _ => unimplemented!()
                }
            },
            LispVal::Float(x2) => {
                let num = x2.clone();
                match y.clone().unwrap() {
                    LispVal::Integer(y) => {
                        let y_clone = y.clone();
                        Ok(LispVal::Float(g(num, y_clone as f64)))
                    },
                    LispVal::Float(y) => {
                        let y_clone = y.clone();
                        Ok(LispVal::Float(g(num, y_clone)))
                    },
                    _ => unimplemented!()
                }
            },
            LispVal::Err(s) => Err(LispVal::Err(s.clone())),
            _ => unimplemented!()
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
        _ => unimplemented!()
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


fn main() {
    let mut rl = Editor::<()>::new();
    rl.load_history("lisp_history.txt");
    loop {
        let readline = rl.readline("lisp> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match parse(&line) {
                    Ok((str, expr)) => {
                        let x1: Vec<LispVal> = expr.iter().map(|x| eval(x)).map(|x| x.unwrap()).collect();
                        print_expr(&x1, false)
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
    rl.save_history("doge_history.txt");

}

#[cfg(test)]
mod test{
    use nom::IResult;

    use ::{float, integer};
    use ::{number, symbol};
    use ::{LispVal, lispy};
    use ::LispVal::{Float, Integer, Symbol};
    use eval;

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
        println!("{:?}", result.is_err());
        let (i, expr) = lispy("(/ 1 1 1 1)").unwrap();
        for e in expr {
            println!("{:?}", eval(&e));
        }
    }
}