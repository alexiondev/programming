mod aoc23_01;

use std::env;

fn main() {
    let raw_args: Vec<String> = env::args().collect();
    let args: Vec<&str> = raw_args.iter().map(AsRef::as_ref).collect();

    match args[1..] {
        ["aoc23", "1", "1"] => aoc23_01::problem1(),
        _ => println!("Unknown problem!")
    }
}