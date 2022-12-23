use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(23)));

    println!("Day 23");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Location = (isize, isize);


fn solve1(data: &HashSet<Location>) -> i32 {
    println!("{:?}", data);
    1
}

fn solve2(data: &HashSet<Location>) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> HashSet<Location> {
    data.iter()
        .enumerate()
        .flat_map(|(row, line)| line.as_ref()
            .char_indices()
            .filter(|&(_, c)| c == '#')
            .map(move |(col, _)| (row as isize, col as isize))
        )
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        ".....",
        "..##.",
        "..#..",
        ".....",
        "..##.",
        ".....",
    ];

    #[test]
    fn part1() {
        assert_eq!(1, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
