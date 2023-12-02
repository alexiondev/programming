mod aoc23_01;
mod aoc23_02;

use std::env;

use anyhow;

fn main() -> anyhow::Result<()>{
    let raw_args: Vec<String> = env::args().collect();
    let args: Vec<&str> = raw_args.iter().map(AsRef::as_ref).collect();

    match args[1..] {
        ["aoc23", "1", "1"] => aoc23_01::problem1()?,
        ["aoc23", "1", "2"] => aoc23_01::problem2()?,
        ["aoc23", "2", "1"] => aoc23_02::problem1()?,
        ["aoc23", "2", "2"] => aoc23_02::problem2()?,
        _ => println!("Unknown problem!")
    }

    Ok(())
}