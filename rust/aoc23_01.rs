use std::{
    fs::File,
    io::{prelude::*, BufReader},
};
use regex::Regex;

use anyhow::{self, bail};

const INPUT_PATH : &str = "_input/aoc23/01";

fn input() -> Vec<String> {
    let file = File::open(INPUT_PATH).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn find_first_digit(iter: impl Iterator<Item = char>) -> anyhow::Result<u32> { 
    for c in iter {
        if let Some(x) = c.to_digit(10) {
            return Ok(x);
        }
    }

    bail!("Did not find a digit");
}

pub fn problem1() -> anyhow::Result<()> {
    let mut numbers: Vec<i32> = Vec::new();
    for line in input() {
        let first_digit = find_first_digit(line.chars())?;
        let last_digit = find_first_digit(line.chars().rev())?;

        let x = format!("{}{}", first_digit, last_digit).parse::<i32>().unwrap();
        numbers.push(x);
    }

    let sum: i32 = numbers.iter().sum();
    println!("{}", sum);
    Ok(())
}

fn find_first_digit_word(iter: impl Iterator<Item = char>) -> anyhow::Result<u32> {
    let mut buf = String::new();

    for c in iter {
        if let Some(x) = c.to_digit(10) {
            return Ok(x);
        } else {
            buf.push(c);
        }

        if let Some(x) = parse_number(&buf) {
            return Ok(x);
        }
    }

    bail!("Did not find a digit");
}

fn parse_number(s: &str) -> Option<u32> {
    match s {
        "zero" | "orez" => Some(0),
        "one" | "eno" => Some(1),
        "two" | "owt" => Some(2),
        "three" | "eerht" => Some(3),
        "four" | "ruof" => Some(4),
        "five" | "evif" => Some(5),
        "six" | "xis" => Some(6),
        "seven" | "neves" => Some(7),
        "eight" | "thgie" => Some(8),
        "nine" | "enin" => Some(9),
        "" => None,
        _ => parse_number(&s[1..s.len()]),
    }
}

pub fn problem2() -> anyhow::Result<()> {
    let mut numbers: Vec<i32> = Vec::new();
    for line in input() {
        let first_digit = find_first_digit_word(line.chars())?;
        let last_digit = find_first_digit_word(line.chars().rev())?;

        let x = format!("{}{}", first_digit, last_digit).parse::<i32>().unwrap();
        numbers.push(x);
    }

    let sum: i32 = numbers.iter().sum();
    println!("{}", sum);
    Ok(())
}