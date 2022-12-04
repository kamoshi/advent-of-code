#![allow(dead_code)]
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(4)));

    println!("Day 4");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &Vec<((i32, i32), (i32, i32))>) -> i32 {
    data.iter()
        .copied()
        .map(|((s1, e1), (s2, e2))|
            if s1 <= s2 && e2 <= e1 || s2 <= s1 && e1 <= e2 { 1 } else { 0 }
        )
        .sum()
}

fn solve2(data: &Vec<((i32, i32), (i32, i32))>) -> i32 {
    data.iter()
        .copied()
        .map(|((s1, e1), (s2, e2))|
            if s1 <= e2 && s2 <= e1 { 1 } else { 0 }
        )
        .sum()
}


fn parse_data(data: Vec<String>) -> Vec<((i32, i32), (i32, i32))> {
    let re = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)$").unwrap();
    data.iter()
        .map(|s| {
            let c = re.captures(s).unwrap();
            (
                (
                    c.get(1).unwrap().as_str().parse().unwrap(),
                    c.get(2).unwrap().as_str().parse().unwrap(),
                ),
                (
                    c.get(3).unwrap().as_str().parse().unwrap(),
                    c.get(4).unwrap().as_str().parse().unwrap(),
                ),
            )
        })
        .collect()
}
