#![allow(dead_code)]
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(2)));

    println!("Day 2");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[repr(u8)]
#[derive(Clone, Copy)]
enum Shape {
    Rock = 0,
    Paper = 1,
    Scissors = 2,
}

impl Shape {
    fn from(char: &char) -> Shape {
        match char {
            &'A' => Shape::Rock,
            &'B' => Shape::Paper,
            &'C' => Shape::Scissors,
            &'X' => Shape::Rock,
            &'Y' => Shape::Paper,
            &'Z' => Shape::Scissors,
            &_ => panic!(),
        }
    }

    fn from_num(num: u8) -> Shape {
        match num {
            0 => Shape::Rock,
            1 => Shape::Paper,
            2 => Shape::Scissors,
            _ => panic!()
        }
    }

    fn check_against(&self, other: &Shape) -> MatchResult {
        let (this, that) = (*self as u8, *other as u8);
        if this == that {
            MatchResult::Tie
        }
        else if (this + 1) % 3 == that {
            MatchResult::Loss
        }
        else {
            MatchResult::Win
        }
    }

    fn to_points(&self) -> i32 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    fn find_from(other: &Shape, result: &MatchResult) -> Shape {
        match result {
            MatchResult::Win => Shape::from_num((*other as u8 + 1) % 3),
            MatchResult::Tie => *other,
            MatchResult::Loss => Shape::from_num((*other as u8 + 2) % 3),
        }
    }
}

enum MatchResult {
    Win, Tie, Loss
}

impl MatchResult {
    fn to_points(&self) -> i32 {
        match self {
            MatchResult::Win => 6,
            MatchResult::Tie => 3,
            MatchResult::Loss => 0,
        }
    }

    fn from(char: &char) -> MatchResult {
        match char {
            &'X' => MatchResult::Loss,
            &'Y' => MatchResult::Tie,
            &'Z' => MatchResult::Win,
            &_ => panic!(),
        }
    }
}


fn solve1(data: &[(char, char)]) -> i32 {
    data.iter()
        .fold(0, |acc, (other, my)| {
            let my = Shape::from(my);
            let result = my.check_against(&Shape::from(other));
            acc + my.to_points() + result.to_points()
        })
}

fn solve2(data: &[(char, char)]) -> i32 {
    data.iter()
        .fold(0, |acc, (other, result)| {
            let result = MatchResult::from(result);
            let my = Shape::find_from(&Shape::from(other), &result);
            acc + my.to_points() + result.to_points()
        })
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<(char, char)> {
    data.into_iter()
        .map(|line| {
            let line = line.as_ref();
            let mut chars = line.chars();
            (chars.next().unwrap(), chars.last().unwrap())
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str; 3] = &["A Y", "B X", "C Z"];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(15, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(12, solve2(&data));
    }
}
