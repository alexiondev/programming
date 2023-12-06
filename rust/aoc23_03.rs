use std::{
    fs::File,
    io::{prelude::*, BufReader},
};

use anyhow;

const INPUT_PATH : &str = "_input/aoc23/03";

fn input() -> Vec<String> {
    let file = File::open(INPUT_PATH).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn parse(input: Vec<String>) -> Vec<Vec<Value>> {
    let mut parsed = Vec::new();

    for line in input.iter() {
        let mut row: Vec<Value> = Vec::new();

        for c in line.chars() {
            row.push(match c {
                '0' |
                '1' |
                '2' |
                '3' |
                '4' |
                '5' |
                '6' |
                '7' |
                '8' |
                '9' => Value::Num(c.to_digit(10).unwrap()),
                '.' => Value::Empty,
                _ => Value::Symbol,
            });
        }

        parsed.push(row);
    }

    parsed
}

#[derive(Debug, PartialEq)]
enum Value {
    Num(u32),
    Symbol,
    Empty,
}

pub fn problem1() -> anyhow::Result<()> {
    let input = input();
    let parsed = parse(input);

    let mut sum = 0;
    let mut last = Value::Empty;
    let mut symbol_found = false;

    for (x, row) in parsed.iter().enumerate() {
        for (y, val) in row.iter().enumerate() {
            match (&last, val) {
                (Value::Empty, Value::Num(b)) => {
                    last = Value::Num(*b);
                    symbol_found = look_for_symbol(&parsed, x, y);
                }
                (Value::Num(a), Value::Num(b)) => {
                    last = Value::Num(a * 10 + b);
                    symbol_found = symbol_found || look_for_symbol(&parsed, x, y);
                }
                (Value::Num(a), _) => {
                    if symbol_found {
                        sum += a;
                    }

                    last = Value::Empty;
                    symbol_found = false;
                }
                (_, _) => {
                }
            }
        }
    }
    println!("{}", sum);
    Ok(())
}

fn look_for_symbol(input: &Vec<Vec<Value>>, x: usize, y: usize) -> bool {
    for dx in -1i32..2i32 {
        for dy in -1i32..2i32 {
            let x1 = x as i32 + dx;
            let y1 = y as i32 + dy;

            if x1 < 0 || y1 < 0 ||
               x1 >= input.len() as i32 ||
               y1 >= input[x].len() as i32 {
                continue;
            }

            if input[x1 as usize][y1 as usize] == Value::Symbol {
                return true;
            }
        }
    }
    return false;
}

pub fn problem2() -> anyhow::Result<()> {
    Ok(())
}