use std::{
    fs::File,
    io::{prelude::*, BufReader}, ops::Deref,
};

use anyhow::{self, bail};
use nom::{self, IResult, bytes::complete::tag, sequence::{preceded, terminated, pair}, character::complete::u32, multi::{many1, separated_list1}, combinator::opt, branch::alt};

const INPUT_PATH : &str = "_input/aoc23/02";

fn input() -> Vec<Game> {
    let file = File::open(INPUT_PATH).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .map(|s| parse(s.as_str()).unwrap().1)
        .collect()
}

#[derive(Debug)]
struct Game {
    id: u32,
    sets: Vec<Set>,
}

impl From<(u32, Vec<Vec<(u32, &str)>>)> for Game {
    fn from(raw: (u32, Vec<Vec<(u32, &str)>>)) -> Self {
        let game = Game{
            id: raw.0,
            sets: raw.1.iter()
                .map(|v| Set::from(v))
                .collect(),
        };

        game
    }
}

#[derive(Clone, Copy, Debug)]
struct Set {
    r: u32,
    g: u32,
    b: u32,
}

impl From<&Vec<(u32, &str)>> for Set {
    fn from(v: &Vec<(u32, &str)>) -> Self {
        let mut set = Set{r: 0, g: 0, b: 0};
        for e in v.iter() {
            match e {
                (x, " red") => {
                    set.r = *x;
                },
                (x, " green") => {
                    set.g = *x;
                },
                (x, " blue") => {
                    set.b = *x;
                },
                _ => {}
            }
        }

        set
    }
}

fn parse(input: &str) -> IResult<&str, Game> {
    let game_id_parser = terminated(preceded(
        tag("Game "),
        u32
    ),tag(": "));

    let color_parser = alt((
        pair(u32, tag(" red")),
        pair(u32, tag(" blue")),
        pair(u32, tag(" green"))
    ));
    let set_parser = separated_list1(tag(", "), color_parser);
    let sets_parser = separated_list1(tag("; "), set_parser);
    
    let (remaining, raw) = pair(game_id_parser, sets_parser)(input)?;


    Ok((remaining, Game::from(raw)))
}

pub fn problem1() -> anyhow::Result<()> {
    let games = input();
    let (r,g,b) = (12,13,14);

    let valid_sum = games.iter()
        .filter(|game| game.sets.iter()
            .map(|s| s.r <= r && s.g <= g && s.b <= b)
            .reduce(|acc, x| acc && x).unwrap())
        .map(|g| g.id)
        .reduce(|acc, x| acc + x);
    
    println!("{:?}", valid_sum); 

    Ok(())
}

// If 0 is present, the other value is used
fn max(a: u32, b: u32) -> u32 {
    match (a,b) {
        (0, b) => b,
        (a, 0) => a,
        _ => std::cmp::max(a,b)
    }
}

pub fn problem2() -> anyhow::Result<()> {
    let games = input();

    let x = games.iter()
        .map(|g| g.sets.clone())
        .map(|sets| sets.iter().fold((0,0,0), |acc, s| (
            max(acc.0, s.r),
            max(acc.1, s.g),
            max(acc.2, s.b))))
        .map(|(a,b,c)| a*b*c)
        .reduce(|acc, a| acc+a);
    println!("{:?}", x);

    Ok(())
}