#![allow(dead_code)]
use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(3)));

    println!("Day 3");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

fn solve1(data: &Vec<(HashSet<char>, HashSet<char>)>) -> i32 {
    data.iter()
        .map(|(left, right)| {
            convert_char(*left.intersection(right).next().unwrap())
        })
        .sum()
}

fn solve2(data: &Vec<(HashSet<char>, HashSet<char>)>) -> i32 {
    data.iter()
        .map(|(left, right)| left.union(right).cloned().collect())
        .collect::<Vec<HashSet<_>>>()
        .chunks_exact(3)
        .map(|chunk| {
            convert_char(find_intersection(chunk))
        })
        .sum()
}

fn convert_char(char: char) -> i32 {
    let char = char as i32;
    char - if char < 97 { 38 } else { 96 }
}

fn find_intersection(sets: &[HashSet<char>]) -> char {
    let mut iter = sets.iter().cloned();
    iter.next()
        .map(|set| {
            iter.fold(set, |set1, set2| {
                    set1.intersection(&set2).cloned().collect()
                })
                .into_iter()
                .next().unwrap()
        })
        .unwrap()
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<(HashSet<char>, HashSet<char>)> {
    data.iter()
        .map(|str| {
            let str = str.as_ref();
            let length = str.len();
            let left = &str[..length / 2];
            let right = &str[length / 2..];
            (left.chars().into_iter().collect(), right.chars().into_iter().collect())
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str; 6] = &[
        "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(157, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(70, solve2(&data));
    }
}
