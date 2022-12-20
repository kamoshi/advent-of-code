use std::collections::VecDeque;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(20)));

    println!("Day 20");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn reorder(data: &[(usize, i32)]) -> Vec<i32> {
    let mut array = data.iter().copied().collect::<VecDeque<_>>();

    for item in data {
        let index = array.iter().position(|item_ref| item == item_ref).unwrap();
        array.remove(index);
        let index = (index as i32 + item.1).rem_euclid(array.len() as i32) as usize;
        array.insert(index, *item);
    }

    let zero_offset = array.iter().position(|item_ref| 0 == item_ref.1).unwrap();
    array.rotate_left(zero_offset);
    array.into_iter().map(|x| x.1).collect()
}

fn solve1(data: &[(usize, i32)]) -> i32 {
    let array = reorder(data);
    let len = array.len();
    array[1000 % len] + array[2000 % len] + array[3000 % len]
}

fn solve2(data: &[(usize, i32)]) -> i32 {
    reorder(data);
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<(usize, i32)> {
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
        assert_eq!(3, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
