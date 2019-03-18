use std::io::{stdin, Error};

#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub rekt);

pub mod ast;

fn main() -> Result<(), Error> {
    let si = stdin();
    let mut bufr = String::new();
    let prs = rekt::ScriptParser::new();

    loop {
        si.read_line(&mut bufr)?;

        if bufr.trim() == ".exit" {
            break;
        }

        match prs.parse(&bufr) {
            Ok(ref prg) if prg.is_empty() => (),
            Ok(prg) => println!("{:?}", prg),
            Err(err) => eprintln!("{:?}", err),
        }

        bufr.clear();
    }

    Ok(())
}
