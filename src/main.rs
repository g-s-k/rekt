use std::io::{stdin, Error, Read};

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub rekt);

pub mod ast;

fn main() -> Result<(), Error> {
    let mut si = stdin();
    let mut bufr = String::new();
    let prs = rekt::ScriptParser::new();

    si.read_to_string(&mut bufr)?;

    match prs.parse(&bufr) {
        Ok(ref prg) if prg.is_empty() => (),
        Ok(prg) => println!("{:?}", prg),
        Err(err) => eprintln!("{:?}", err),
    }

    Ok(())
}
