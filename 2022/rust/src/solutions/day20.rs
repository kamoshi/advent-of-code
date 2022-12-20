#![allow(dead_code)]
use std::collections::VecDeque;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(20)));

    println!("Day 20");
    println!("Part 1: {}", solve::<1, 1>(&data));
    println!("Part 2: {}", solve::<811589153, 10>(&data));
}


fn reorder(data: &[(usize, i64)], reps: i32) -> Vec<i64> {
    let mut array = VecDeque::from_iter(data.iter().copied());

    for _ in 0..reps {
        for item in data {
            let index = array.iter().position(|item_ref| item == item_ref).unwrap();
            array.remove(index);
            let index = (index as i64 + item.1).rem_euclid(array.len() as i64) as usize;
            array.insert(index, *item);
        }
    }

    let zero_offset = array.iter().position(|item_ref| 0 == item_ref.1).unwrap();
    array.rotate_left(zero_offset);
    array.into_iter().map(|x| x.1).collect()
}

fn solve<const KEY: i64, const REPS: i32>(data: &[(usize, i64)]) -> i64 {
    let array = reorder(&Vec::from_iter(data.iter().map(|&(index, value)| (index, value * KEY))), REPS);
    let len = array.len();
    array[1000 % len] + array[2000 % len] + array[3000 % len]
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<(usize, i64)> {
    data.iter()
        .enumerate()
        .map(|(index, line)| (index, line.as_ref().parse().unwrap()))
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &["1", "2", "-3", "3", "-2", "0", "4"];

    #[test]
    fn part1() {
        assert_eq!(3, solve::<1, 1>(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(1623178306, solve::<811589153, 10>(&parse_data(DATA)));
    }
}
