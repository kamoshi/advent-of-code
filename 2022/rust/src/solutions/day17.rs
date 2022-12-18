use std::collections::HashMap;
use std::fmt::{Display, Formatter};
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


struct Cave {
    wind: Box<dyn Iterator<Item=(usize, Wind)>>,
    hole: Vec<u8>,
    memo: HashMap<(isize, usize), (usize, usize)>,
    size: usize,
    hist: Vec<(usize, usize)>,
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
            wind: Box::new(Vec::from(wind).into_iter().enumerate().cycle()),
            hole: vec![u8::MAX],
            memo: HashMap::new(),
            size: 0,
            hist: vec![]
        }
    }

    fn insert(&mut self, rock: usize) -> Option<((usize, usize), (usize, usize), Vec<(usize, usize)>)> {
        self.hole.resize(self.count_occupied() + 3 + ROCK_H[rock], 0);

        let mut offset: isize = 1;
        let mut height: usize = self.hole.len() - 1;
        let mut offset_xor = 0;
        loop {
            let (wind_index, wind) = self.wind.next().unwrap();

            match wind {
                Wind::L => if offset < 3 && !self.check_collision(rock, offset + 1, height) {
                    offset += 1
                },
                Wind::R => if offset > ROCK_W[rock] - 4 && !self.check_collision(rock, offset - 1, height) {
                    offset -= 1
                },
            }

            offset_xor ^= offset;

            if self.check_collision(rock, offset, height - 1) {
                self.add_rock(rock, offset, height);
                self.size += 1;

                let curr = (self.size, self.count_occupied() - 1);
                self.hist.push(curr);

                return match rock == 0 {
                    true => self.memo.insert((offset_xor, wind_index), curr)
                        .map(|prev| (prev, curr, self.hist.drain(..).collect())),
                    false => None,
                }
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

fn solve2(data: &[Wind]) -> i64 {
    let mut rocks = (0..ROCKS.len()).into_iter().cycle();
    let mut cave = Cave::new(data);

    const BIG: i64 = 1_000_000_000_000;

    for _ in 0..2022 {
        if let Some((prev, curr, hist)) = cave.insert(rocks.next().unwrap()) {
            let (s, e) = (prev.0 - 1, curr.0 - 1);
            let start = &hist[0..s];
            let cycle = &hist[s..e];
            let cycle_h = curr.1 - prev.1;

            let from_start = hist[s-1].1 as i64;
            let rocks_left = BIG - start.len() as i64;

            let from_cycle = (rocks_left / cycle.len() as i64) * cycle_h as i64;
            let rocks_left = rocks_left % cycle.len() as i64;

            // let from_above = match rocks_left > 0 {
            //     true => (cycle[rocks_left as usize - 1].1 - cycle[0].1) as i64,
            //     false => 0,
            // };

            let ok = from_start + from_cycle; // + from_above;
            assert!(ok < 1527906974720);

            return ok;
        };
    };
    -1
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
        assert_eq!(1514285714288, solve2(&parse_data(DATA)));
    }
}
