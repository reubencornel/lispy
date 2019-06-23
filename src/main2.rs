// I want to parse a string
// Hello, <World> from the input
// and spit out the name
extern crate nom;
extern crate tuple;

use nom::sequence::{tuple};
use nom::bytes::complete::tag;
use nom::{IResult};
use nom::character::complete::alpha1;
use nom::character::complete::space1;

fn is_hello_string(input: &str) -> IResult<&str, &str> {
    let (input, (_, _, name)) = tuple((tag("Hello"), space1, alpha1))(input)?;
    Ok((input, name))
}

fn main() {
    let result = is_hello_string("Hello   1World").unwrap();
    println!("Hi {}!", result.1);
}