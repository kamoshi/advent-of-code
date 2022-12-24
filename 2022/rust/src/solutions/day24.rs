#![allow(dead_code)]
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(24)));

    println!("Day 24");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Pos = (isize, isize);

#[derive(Copy, Clone, Debug)]
enum Dir {
    U, D, L, R
}

#[derive(Copy, Clone, Debug)]
struct Blizzard {
    dir: Dir,
    pos: Pos,
}

fn offset_blizzards(blizzards: &[Blizzard], (rows, cols): (isize, isize), offset: isize) -> HashSet<Pos> {
    blizzards.iter()
        .map(|&Blizzard { dir, pos: (row, col) }| match dir {
            Dir::U => ((row - offset).rem_euclid(rows), col),
            Dir::D => ((row + offset) % rows, col),
            Dir::L => (row, (col - offset).rem_euclid(cols)),
            Dir::R => (row, (col + offset) % cols),
        })
        .collect()
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: isize,
    position: (isize, Pos),
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost).then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn neighbours((row, col): Pos, (rows, cols): (isize, isize)) -> impl Iterator<Item=Pos> {
    [(row, col), (row+1, col), (row-1, col), (row, col+1), (row, col-1)].into_iter()
        .filter(move |&(n_row, n_col)|
            (0 <= n_row || n_col == 0) && (n_row < rows || n_col == cols - 1) && 0 <= n_col && n_col < cols)
}

fn manhattan(start: Pos, goal: Pos) -> isize {
    (start.0.abs_diff(goal.0) + start.1.abs_diff(goal.1)) as isize
}

fn a_star(start: Pos, goal: Pos, dims: (isize, isize), blizzards: &[Blizzard], offset: isize) -> Option<isize> {
    let mut frontier: BinaryHeap<State> = BinaryHeap::new();
    let mut cost: HashMap<(isize, Pos), isize> = HashMap::from([((offset, start), 0)]);

    frontier.push(State { cost: 0, position: (offset, start) });
    while let Some(State { position: (cur_t, cur_pos), .. }) = frontier.pop() {
        if cur_pos == goal { return Some(cur_t) };
        let next_t = cur_t + 1;
        let blizzards = offset_blizzards(blizzards, dims, next_t);
        for neighbour in neighbours(cur_pos, dims).filter(|p| !blizzards.contains(p)) {
            let new_cost = cost.get(&(cur_t, cur_pos)).unwrap() + 1;
            if !cost.contains_key(&(next_t, neighbour)) || new_cost < cost[&(next_t, neighbour)] {
                cost.insert((next_t, neighbour), new_cost);
                frontier.push(State {
                    cost: new_cost + manhattan(neighbour, goal),
                    position: (next_t, neighbour),
                });
            }
        }
    }
    None
}

fn solve1((dims, blizzards): &((isize, isize), Vec<Blizzard>)) -> isize {
    let (start, goal) = ((-1_isize, 0_isize), (dims.0, dims.1 - 1));
    a_star(start, goal, *dims, blizzards, 0).unwrap()
}

fn solve2((dims, blizzards): &((isize, isize), Vec<Blizzard>)) -> isize {
    let (start, goal) = ((-1_isize, 0_isize), (dims.0, dims.1 - 1));
    let offset = a_star(start, goal, *dims, blizzards, 0).unwrap();
    let offset = a_star(goal, start, *dims, blizzards, offset).unwrap();
    a_star(start, goal, *dims, blizzards, offset).unwrap()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> ((isize, isize), Vec<Blizzard>) {
    let rows = (data.len() - 2) as isize;
    let cols = (data[0].as_ref().len() - 2) as isize;
    let blizzards = data.iter()
        .skip(1)
        .enumerate()
        .flat_map(|(row, line)| line.as_ref().chars()
            .skip(1)
            .enumerate()
            .filter_map(move |(col, char)| {
                let pos = (row as isize, col as isize);
                match char {
                    '^' => Some(Blizzard { dir: Dir::U, pos }),
                    'v' => Some(Blizzard { dir: Dir::D, pos }),
                    '<' => Some(Blizzard { dir: Dir::L, pos }),
                    '>' => Some(Blizzard { dir: Dir::R, pos }),
                    _ => None,
                }}))
        .collect();
    ((rows, cols), blizzards)
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "#.######",
        "#>>.<^<#",
        "#.<..<<#",
        "#>v.><>#",
        "#<^v^^>#",
        "######.#",
    ];

    #[test]
    fn part1() {
        assert_eq!(18, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(54, solve2(&parse_data(DATA)));
    }
}
