#![allow(dead_code)]
use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(9)));

    println!("Day 9");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


enum Direction { U, D, L, R }

struct Move {
    direction: Direction,
    units: i32
}


#[inline(always)]
fn fix_tail(head: (i32, i32), tail: (i32, i32)) -> (i32, i32) {
    let diff_x = tail.0 - head.0;
    let diff_y = tail.1 - head.1;
    match (diff_x.abs() > 1, diff_y.abs() > 1) {
        (true, true) => (head.0 + diff_x.signum(), head.1 + diff_y.signum()),
        (true, false) => (head.0 + diff_x.signum(), head.1),
        (false, true) => (head.0, head.1 + diff_y.signum()),
        _ => tail,
    }
}

fn tail_visits(data: &[Move], knots: &mut [(i32, i32)]) -> HashSet<(i32, i32)> {
    let mut set = HashSet::from([*knots.last().unwrap()]);
    for m in data {
        for _ in 0..m.units {
            // First move the head
            let head = knots[0];
            knots[0] = match m.direction {
                Direction::U => (head.0, head.1 + 1),
                Direction::D => (head.0, head.1 - 1),
                Direction::L => (head.0 - 1, head.1),
                Direction::R => (head.0 + 1, head.1),
            };
            // Second fix all knots sequentially for arbitrary number of knots
            for idx in 1..knots.len() {
                knots[idx] = fix_tail(knots[idx-1], knots[idx]);
            }
            set.insert(*knots.last().unwrap());
        }
    };
    set
}

fn solve1(data: &[Move]) -> usize {
    let mut knots = [(0, 0); 2];
    tail_visits(&data, &mut knots).len()
}

fn solve2(data: &[Move]) -> usize {
    let mut knots = [(0, 0); 10];
    tail_visits(&data, &mut knots).len()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Move> {
    data.iter()
        .map(|s| {
            let mut s = s.as_ref().split_whitespace().into_iter();
            Move {
                direction: match s.next().unwrap() {
                    "U" => Direction::U,
                    "D" => Direction::D,
                    "L" => Direction::L,
                    "R" => Direction::R,
                    _ => unreachable!()
                },
                units: s.next().unwrap().parse().unwrap(),
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA_A: &[&str; 8] = &["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];
    static DATA_B: &[&str; 8] = &["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"];

    #[test]
    fn part1() {
        assert_eq!(13, solve1(&parse_data(DATA_A)));
    }

    #[test]
    fn part2() {
        assert_eq!(1, solve2(&parse_data(DATA_A)));
        assert_eq!(36, solve2(&parse_data(DATA_B)));
    }
}
