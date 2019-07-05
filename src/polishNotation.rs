// The first thing we need is a structure
extern crate nom;
extern crate rustyline;

use std::iter::*;
use std::process::exit;
use std::vec::Vec;


use nom::{IResult, ParseTo};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::{is_digit, is_space};
use nom::character::complete::{digit1, space0, space1};
use nom::multi::{many1, separated_list};
use nom::sequence::tuple;

use ::Expression::Int;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[derive(Debug, PartialEq, Copy, Clone)]
enum OperatorEnum {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    POW
}


trait Evaluatable<T> {
    fn eval(&self) -> Result<T, LispError>;
}

#[derive(PartialEq, Debug, Copy, Clone)]
struct Integer {
    x: i64
}

#[derive(PartialEq, Debug, Clone)]
enum Expression {
    Int(Integer),
    Operator(OperatorEnum, Vec<Expression>)
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum LispError {
    DIV_BY_ZERO,
    BAD_OP,
    BAD_NUM
}

impl Evaluatable<Integer> for Expression {
    fn eval(&self) -> Result<Integer, LispError> {
        match self {
            Expression::Int(x) => Ok(Integer{x: x.x}),
            Expression::Operator(operator, exps ) => {
                let argument_results: Vec<Result<Integer, LispError>> = exps.iter().map(|x| x.eval()).collect();
                let func = |x: Result<Integer, LispError>, y: &Result<Integer, LispError>, f: fn(Integer, Integer) -> Result<Integer, LispError> | {
                    if x.is_err() {
                        x
                    } else if y.is_err() {
                        *y
                    } else {
                        f(x.unwrap(), y.unwrap())
                    }
                };

                match operator {
                    OperatorEnum::ADD => argument_results[1..].iter().fold(argument_results[0], |x, y| {
                        func(x, y, |a, b|  Ok(Integer{x:a.x+b.x}))
                    }),
                    OperatorEnum::DIV => argument_results[1..].iter().fold(argument_results[0], |x, y| {
                        func(x, y, |a, b| {
                            let x1: Option<i64> = a.x.checked_div(b.x);
                            if x1.is_none() {
                                Err(LispError::DIV_BY_ZERO)
                            } else {
                                Ok(Integer { x: x1.unwrap() })
                            }
                        })}),
                    OperatorEnum::MUL => argument_results[1..].iter().fold(argument_results[0], |x, y| {
                        func(x, y, |a, b|  Ok(Integer{x:a.x*b.x}))
                    }),
                    OperatorEnum::SUB => argument_results[1..].iter().fold(argument_results[0], |x, y| {
                        func(x, y, |a, b|  Ok(Integer{x:a.x-b.x}))
                    }),

                    _ => unimplemented!()
                }
            }
        }
    }
}

fn number(input: &str) -> IResult<&str, Expression>{
    let (inp, res) = digit1(input)?;
    Ok((inp, Int(Integer{x: res.parse_to().unwrap()})))
}

fn operator(input: &str) -> IResult<&str, OperatorEnum> {
    let (input, operator)= alt((tag("+"), tag("-"), tag("*"), tag("/")))(input)?;
    Ok((input, match operator {
        "+" => OperatorEnum::ADD,
        "-" => OperatorEnum::SUB,
        "*" => OperatorEnum::MUL,
        "/" => OperatorEnum::DIV,
        "%" => OperatorEnum::MOD,
        "^" => OperatorEnum::POW,
        _ => unimplemented!()
    }))
}


fn bracketed_expression(input: &str) -> IResult<&str, Expression> {
    let (inp, (t, op, s, nums, t1)) = tuple((tag("("), operator, space1, separated_list(space1, expr), tag(")")))(input)?;
    Ok((inp, Expression::Operator(op, nums)))
}

fn expr(input: &str) -> IResult<&str, Expression> {
    alt((number, bracketed_expression))(input)
}

fn main() {

    let mut rl = Editor::<()>::new();
    rl.load_history("lisp_history.txt");

    loop {
        let readline = rl.readline("lisp >>");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match expr(&line) {
                    Ok((str, expr)) => println!("{:#?}", expr.eval()),
                    Err(_) => println!("Error"),
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
    use ::{number, OperatorEnum};
    use ::{bracketed_expression, expr};
    use ::Expression::Operator;
    use ::OperatorEnum::{ADD, MUL};
    use operator;

    use super::Evaluatable;
    use super::Expression;
    use super::Expression::Int;
    use super::Integer;

    #[test]
    fn operator_test() {
        assert_eq!(OperatorEnum::ADD, operator("+").unwrap().1);
    }

    #[test]
    fn int_test() {
        assert!(true);
        let x = Expression::Int(Integer{x:5});
        assert_eq!(x.eval(), Ok(Integer{x:5}));
    }

    #[test]
    fn expression_test() {
        assert!(true);
        let a: String = "a".to_string();
        let x = Expression::Operator(OperatorEnum::ADD, vec![Int(Integer{x:5}), Int(Integer{x:5})]);
        assert_eq!(x.eval(), Ok(Integer{x:10}));
    }

    #[test]
    fn test_bracketed_expr() {
        let (inp, expr) = bracketed_expression("(+ 123)").unwrap();
        println!("{:#?}", expr);
    }

    #[test]
    fn test_number() {
        let (inp, num) = number("123").unwrap();
        println!("{:#?}", num);
    }

    #[test]
    fn test_expr() {
        let (inp, exp) = expr("(+ 123 123)").unwrap();
        assert_eq!(exp, Operator(ADD, vec![Int(Integer{x:123}), Int(Integer{x:123})]));

        let (inp1, exp1) = expr("(+ 123 (* 123 1))").unwrap();
        assert_eq!(exp1, Operator(ADD, vec![Int(Integer{x:123}), Operator(MUL, vec![Int(Integer{x:123}), Int(Integer{x:1})])]));
        println!("{:#?}", exp1.eval());
    }
}