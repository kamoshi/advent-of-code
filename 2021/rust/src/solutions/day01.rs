use crate::utils;
use crate::utils::Source;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(Source::Day(1)));

    println!("Day 1");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

fn solve1(lines: &Vec<i32>) -> i32 {
    let mut increments = 0;
    let mut prev = i32::MAX;
    for &n in lines {
        if n > prev { increments += 1 };
        prev = n;
    };
    increments
}

fn solve2(lines: &Vec<i32>) -> i32 {
    let length = lines.len();

    let left = lines[..length - 1].windows(3);
    let right = lines[1..].windows(3);


    left.zip(right)
        .fold(0, |acc, (left, right)| {
            let sum_left: i32 = left.iter().sum();
            let sum_right: i32 = right.iter().sum();
            acc + if sum_left < sum_right { 1 } else { 0 }
        })
}

fn parse_data(lines: Vec<String>) -> Vec<i32> {
    lines.iter()
        .map(|x| x.parse())
        .collect::<Result<Vec<i32>, _>>()
        .expect("Failed to parse int")
}
