use std::fmt::{Display, Formatter};
use std::iter::Cycle;
use std::vec::IntoIter;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(17)));

    println!("Day 17");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


const ROCK_W: [isize; 5] = [4, 3, 3, 1, 2];
const ROCK_H: [usize; 5] = [1, 3, 3, 4, 2];
const ROCKS: [[u8; 4]; 5] = [
    [0b1111, 0b0000, 0b0000, 0b0000],
    [0b0100, 0b1110, 0b0100, 0b0000],
    [0b0010, 0b0010, 0b1110, 0b0000],
    [0b1000, 0b1000, 0b1000, 0b1000],
    [0b1100, 0b1100, 0b0000, 0b0000],
];


#[derive(Clone)]
struct Cave {
    wind: Cycle<IntoIter<Wind>>,
    hole: Vec<u8>,
}

impl Display for Cave {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in (0..self.hole.len()).rev() {
            writeln!(f, "{:#010b}", self.hole[i])?;
        }
        writeln!(f, "")
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Wind {
    L, R
}

impl Cave {
    fn new(wind: &[Wind]) -> Self {
        Cave {
            wind: Vec::from(wind).into_iter().cycle(),
            hole: vec![u8::MAX],
        }
    }

    fn insert(&mut self, rock: usize) {
        self.hole.resize(self.count_occupied() + 3 + ROCK_H[rock], 0);

        let mut offset: isize = 1;
        let mut height: usize = self.hole.len() - 1;

        loop {
            match self.wind.next().unwrap() {
                Wind::L => if offset < 3 && !self.check_collision(rock, offset + 1, height) {
                    offset += 1
                },
                Wind::R => if offset > ROCK_W[rock] - 4 && !self.check_collision(rock, offset - 1, height) {
                    offset -= 1
                },
            }

            if self.check_collision(rock, offset, height - 1) {
                self.add_rock(rock, offset, height);
                break;
            }

            height -= 1;
        }
    }

    fn add_rock(&mut self, rock: usize, offset: isize, height: usize) {
        for i in 0..4 {
            let rock_row = match offset < 0 {
                false => ROCKS[rock][i] << offset,
                true => ROCKS[rock][i] >> -offset,
            };
            if rock_row == 0 { break };
            self.hole[height - i] |= rock_row;
        }
    }

    fn check_collision(&self, rock: usize, offset: isize, height: usize) -> bool {
        for i in 0..4 {
            let rock_row = match offset < 0 {
                false => ROCKS[rock][i] << offset,
                true => ROCKS[rock][i] >> -offset,
            };
            if rock_row == 0 { break };
            if rock_row & self.hole[height - i] > 0 { return true; };
        }
        false
    }

    fn count_free(&self) -> usize {
        let mut free: usize = 0;
        for i in (0..self.hole.len()).rev() {
            if self.hole[i] == 0 { free += 1; }
        }
        free
    }

    fn count_occupied(&self) -> usize {
        self.hole.len() - self.count_free()
    }
}


fn solve1(data: &[Wind]) -> usize {
    let mut rocks = (0..ROCKS.len()).into_iter().cycle();
    let mut cave = Cave::new(data);

    for _ in 0..2022 {
        cave.insert(rocks.next().unwrap());
    };

    cave.count_occupied() - 1 // first row is the bottom
}

fn solve2(data: &[Wind]) -> i32 {
    2
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Wind> {
    data.iter()
        .next()
        .map(|line| line.as_ref().chars()
            .map(|c| match c as u8 {
                b'>' => Wind::R,
                b'<' => Wind::L,
                _ => unreachable!()
            })
            .collect()
        )
        .unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"];

    #[test]
    fn part1() {
        assert_eq!(3068, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
