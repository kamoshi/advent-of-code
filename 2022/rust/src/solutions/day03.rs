use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(3)));

    println!("Day 1");
    println!("Part 1: {}", solve1(&data));
    // println!("Part 2: {}", solve2(&data));
}

fn solve1(data: &Vec<(HashSet<char>, HashSet<char>)>) -> i32 {
    data.iter()
        .map(|(left, right)| {
            let char = *left.intersection(right).last().expect("Not found char") as i32;
            char - if char < 97 { 38 } else { 96 }
        })
        .sum()
}

fn solve2(data: &Vec<Vec<i32>>) -> i32 {
    2
}


fn parse_data(data: Vec<String>) -> Vec<(HashSet<char>, HashSet<char>)> {
    data.iter()
        .map(|str| {
            let length = str.len();
            let left = &str[..length / 2];
            let right = &str[length / 2..];
            (left.chars().into_iter().collect(),right.chars().into_iter().collect())
        })
        .collect()
}
