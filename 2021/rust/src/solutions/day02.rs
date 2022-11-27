use regex::{Captures, Regex};
use crate::utils;
use crate::utils::Source;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(Source::Day(2)));

    println!("Day 2");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

enum Command {
    Forward(i32),
    Down(i32),
    Up(i32),
}

fn solve1(lines: &Vec<Command>) -> i32 {
    let (x, depth) = lines.iter()
        .fold((0, 0), |(x, depth), next| match next {
            Command::Forward(val) => (x + val, depth),
            Command::Down(val) => (x, depth + val),
            Command::Up(val) => (x, depth - val),
        });
    x * depth
}

fn solve2(lines: &Vec<Command>) -> i32 {
    let (x, depth, aim) = lines.iter()
        .fold((0, 0, 0), |(x, depth, aim), next| match next {
            Command::Forward(val) => (x + val, depth + (aim * val), aim),
            Command::Down(val) => (x, depth, aim + val),
            Command::Up(val) => (x, depth, aim - val),
        });
    x * depth
}

fn parse_data(lines: Vec<String>) -> Vec<Command> {
    let re = Regex::new(r"^(\w+) (\d+)$").unwrap();

    lines.iter()
        .map(|line| re.captures(line).unwrap())
        .map(to_command)
        .collect::<Vec<_>>()
}

fn to_command(cap: Captures) -> Command {
    let left = cap.get(1).unwrap().as_str();
    let right = cap.get(2).unwrap().as_str().parse().unwrap();
    match left {
        "forward" => Command::Forward(right),
        "up" => Command::Up(right),
        "down" => Command::Down(right),
        _ => panic!("Unknown command")
    }
}
