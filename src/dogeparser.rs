extern crate nom;
extern crate rustyline;

use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::sequence::tuple;
use nom::character::complete::space1;
use std::string::{ToString, String};
use nom::combinator::{cond, complete};
use nom::multi::{many1, separated_list, many0};
use nom::error::ErrorKind::Tag;
use nom::Err::Error;
use rustyline::Editor;
use rustyline::error::ReadlineError;

fn adjective(input: &str) -> IResult<&str, &str> {
    alt((tag("so"), tag("many"), tag("little"),tag("long"),tag("such"), tag("very"), tag("poor")))(input)
}

fn noun(input: &str) -> IResult<&str, &str> {
    alt((tag("money"), tag("drink"), tag("doge"),tag("bork")))(input)
}

fn phrase(input: &str) -> IResult<&str, String> {
    tuple((adjective, space1, noun))(input).map(|x| {
        let res = x.1;
        let mut concat_res = String::from(res.0.to_string());
        concat_res.push_str(res.1);
        concat_res.push_str(res.2);
        (x.0, concat_res)
    })
}

fn space_phrase(input: &str) -> IResult<&str, String> {
    tuple((space1, phrase))(input).map(|x|{
        let res = x.1;
        let mut concat_res = String::from(res.0.to_string());
        concat_res.push_str(&res.1);
        (x.0, concat_res)
    } )
}

fn doge(input: &str) -> IResult<&str, String> {
    tuple((phrase, many0(space_phrase)))(input).map(|x| {
        let res = x.1;
//        x.1 == (String, Vec<String>)
        let string_vec: Vec<String> = res.1;
        let mut ret_string0: String = res.0;
        let ret_string = string_vec.iter().fold(String::new(), |mut x, y| {
            x.push_str(y);
            x
        });
        ret_string0.push_str(&ret_string);
        (x.0, ret_string0)
    })
}


fn main() {
//    assert_eq!(adjective("so"), Ok(("", "so")));
//    assert_eq!(adjective("by"), Err(Error(("by", Tag))));
//    assert_eq!(noun("money"), Ok(("", "money")));
//    assert_eq!(noun("by"), Err(Error(("by", Tag))));
//    assert_eq!(phrase("many money"), Ok(("", "many money".to_string())));
//    assert_eq!(phrase("such sad"), Err(Error(("sad", Tag))));
//
//    assert_eq!(doge("such sad 1"), Err(Error(("sad 1", Tag))));
//    assert_eq!(doge("many money little drink"), Ok(("", "many money little drink".to_string())));
//    assert_eq!(doge("random string"), Err(Error(("random string", Tag))));
    let mut rl = Editor::<()>::new();
    rl.load_history("doge_history.txt");

    loop {
        let readline  = rl.readline("doge >>");
        match readline {
            Ok(line)=> {
                rl.add_history_entry(line.as_str());
               if  doge(&line).is_ok() {
                   println!("Much wow!!");
               } else {
                   println!("Such sad!!");
               }
            },
            Err(ReadlineError::Interrupted) => {
                println!("Bye Fren!!");
                break;
            },
            Err(ReadlineError::Eof) => {
                println!("Bye Fren!!");
                break;
            },
            Err(err) =>{
                println!("Why you do this, Fren??");
                break;
            }
        }
    }
    rl.save_history("doge_history.txt");
}