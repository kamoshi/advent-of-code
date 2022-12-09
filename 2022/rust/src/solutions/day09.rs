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


fn fix_tail(head: (i32, i32), tail: (i32, i32), direction: &Direction) -> Option<(i32, i32)> {
    let diff_x = head.0.abs_diff(tail.0);
    let diff_y = head.1.abs_diff(tail.1);
    match (diff_y > 1, diff_x > 1, direction) {
        (true, _, Direction::U) => Some((head.0, head.1 - 1)),
        (true, _, Direction::D) => Some((head.0, head.1 + 1)),
        (_, true, Direction::L) => Some((head.0 + 1, head.1)),
        (_, true, Direction::R) => Some((head.0 - 1, head.1)),
        (_, _, _) => None,
    }
}

fn tail_visits(data: &[Move]) -> HashSet<(i32, i32)> {
    let mut set = HashSet::from([(0, 0)]);
    let mut head = (0, 0);
    let mut tail = (0, 0);
    for m in data {
        for _ in 0..m.units {
            match m.direction {
                Direction::U => head = (head.0, head.1 + 1),
                Direction::D => head = (head.0, head.1 - 1),
                Direction::L => head = (head.0 - 1, head.1),
                Direction::R => head = (head.0 + 1, head.1),
            }
            if let Some(new) = fix_tail(head, tail, &m.direction) {
                set.insert(new);
                tail = new
            }
        }
    };
    set
}

fn solve1(data: &[Move]) -> usize {
    tail_visits(&data).len()
}

fn solve2(data: &[Move]) -> i32 {
    2
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

    static DATA: &[&str; 8] = &["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(13, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
