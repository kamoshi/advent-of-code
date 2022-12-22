use std::collections::{HashMap, HashSet};
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(22)));

    println!("Day 22");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


enum Tile {
    Empty, Wall
}

enum Action {
    Move(usize), L, R
}

enum Dir {
    U, D, L, R
}

type Point = (usize, usize);
type Map = HashMap<Point, Tile>;
type Commands = Vec<Action>;

struct Dungeon {
    pos: Point,
    dir: Dir,
    map: Map,

}


fn solve1(data: &(Point, Map, Commands)) -> i32 {
    1
}

fn solve2(data: &(Point, Map, Commands)) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> (Point, Map, Commands) {
    let start = data.iter().next()
        .map(|first_row|
            first_row.as_ref().char_indices()
                .skip_while(|&(_, c)| c == ' ')
                .next()
                .map(|(col, _)| (0_usize, col)).unwrap()
        ).unwrap();
    let map = data.iter()
        .take_while(|line| !line.as_ref().is_empty())
        .enumerate()
        .fold(HashMap::new(), |mut acc, (row, line)| {
            for (col, char) in line.as_ref().char_indices() {
                match char {
                    ' ' => continue,
                    '#' => acc.insert((row, col), Tile::Wall),
                    '.' => acc.insert((row, col), Tile::Empty),
                    _ => unreachable!(),
                };
            }
            acc
        });
    let re = Regex::new(r#"(R)|(L)|(\d*)"#).unwrap();
    let commands = data.iter().skip_while(|line| !line.as_ref().is_empty()).nth(1).unwrap().as_ref();
    let commands = re.find_iter(commands)
        .map(|cap| match cap.as_str() {
            "R" => Action::Right,
            "L" => Action::Left,
            _ => Action::Move(cap.as_str().parse().unwrap()),
        })
        .collect();
    (start, map, commands)
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "        ...#",
        "        .#..",
        "        #...",
        "        ....",
        "...#.......#",
        "........#...",
        "..#....#....",
        "..........#.",
        "        ...#....",
        "        .....#..",
        "        .#......",
        "        ......#.",
        "",
        "10R5L5R10L4R5L5",
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
