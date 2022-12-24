use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(24)));

    println!("Day 24");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Copy, Clone, Debug)]
enum Dir {
    U, D, L, R
}

#[derive(Copy, Clone, Debug)]
struct Blizzard {
    dir: Dir,
    pos: (isize, isize),
}


fn offset_blizzards(blizzards: &[Blizzard], (rows, cols): (isize, isize), offset: isize) -> HashSet<(isize, isize)> {
    blizzards.iter()
        .map(|&Blizzard { dir, pos: (row, col) }| match dir {
            Dir::U => ((row - offset).rem_euclid(rows), col),
            Dir::D => ((row + offset) % rows, col),
            Dir::L => (row, (col - offset).rem_euclid(cols)),
            Dir::R => (row, (col + offset) % cols),
        })
        .collect()
}


fn solve1((dims, blizzards): &((isize, isize), Vec<Blizzard>)) -> i32 {
    let o = offset_blizzards(blizzards, *dims, 5);
    println!("{o:?}");
    1
}

fn solve2(data: &((isize, isize), Vec<Blizzard>)) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> ((isize, isize), Vec<Blizzard>) {
    let rows = (data.len() - 2) as isize;
    let cols = (data[0].as_ref().len() - 2) as isize;
    let blizzards = data.iter()
        .skip(1)
        .enumerate()
        .flat_map(|(row, line)| {
            let line = line.as_ref();
            line.chars().skip(1).enumerate().filter_map(move |(col, char)| {
                let pos = (row as isize, col as isize);
                match char {
                    '^' => Some(Blizzard { dir: Dir::U, pos }),
                    'v' => Some(Blizzard { dir: Dir::D, pos }),
                    '<' => Some(Blizzard { dir: Dir::L, pos }),
                    '>' => Some(Blizzard { dir: Dir::R, pos }),
                    _ => None,
                }
            })
        })
        .collect();
    ((rows, cols), blizzards)
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "#.#####",
        "#.....#",
        "#>....#",
        "#.....#",
        "#...v.#",
        "#.....#",
        "#####.#",
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(1, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
