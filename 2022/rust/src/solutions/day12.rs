#![allow(dead_code)]
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use crate::utils;
use crate::utils::matrix::Matrix;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(12)));

    println!("Day 12");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Data = ((usize, usize), (usize, usize), Matrix<u8>);

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: i32,
    position: (usize, usize),
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


fn neighbours((row, col): (usize, usize), (rows, cols): (usize, usize)) -> Vec<(usize, usize)> {
    let mut ns = Vec::with_capacity(4);
    if row + 1 < rows { ns.push((row + 1, col)) };
    if 0 < row { ns.push((row - 1, col)) };
    if col + 1 < cols{ ns.push((row, col + 1)) };
    if 0 < col { ns.push((row, col - 1)) };
    ns
}

fn manhattan(start: (usize, usize), goal: (usize, usize)) -> i32 {
    (start.0.abs_diff(goal.0) + start.1.abs_diff(goal.1)) as i32
}


fn a_star(start: (usize, usize), goal: (usize, usize), grid: &Matrix<u8>) -> Option<Vec<(usize, usize)>> {
    let mut frontier: BinaryHeap<State> = BinaryHeap::new();
    let mut parent: HashMap<(usize, usize), (usize, usize)> = HashMap::new();
    let mut cost: HashMap<(usize, usize), i32> = HashMap::from([(start, 0)]);

    frontier.push(State { cost: 0, position: start });
    while let Some(State { position: current, .. }) = frontier.pop() {
        if current == goal { break };

        for neighbour in neighbours(current, grid.shape()) {
            // unreachable
            let level_diff = grid[neighbour].abs_diff(grid[current]);
            if grid[neighbour] > grid[current] && level_diff > 1 { continue };

            let new_cost = cost.get(&current).unwrap() + 1;
            if !cost.contains_key(&neighbour) || new_cost < *cost.get(&neighbour).unwrap() {
                cost.insert(neighbour, new_cost);
                parent.insert(neighbour, current);
                frontier.push(State {
                    cost: new_cost + manhattan(neighbour, goal),
                    position: neighbour,
                });
            }
        }
    }

    // unwind path
    let mut path = vec![goal];
    while !path.last().unwrap().eq(&start) {
        path.push(*parent.get(path.last().unwrap())?);
    }
    Some(path)
}

fn solve1((start, goal, grid): &Data) -> usize {
    a_star(*start, *goal, grid).unwrap().len() - 1
}


fn solve2((_, goal, grid): &Data) -> usize {
    // TODO: very ineffective! ideally we should execute the algorithm just once
    grid.cell_indices()
        .filter_map(|index| match grid[index] == 0 {
            true => Some(index),
            false => None
        })
        .filter_map(|start|
            a_star(start, *goal, grid).map(|path| path.len() - 1)
        )
        .min()
        .unwrap()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Data {
    const OFFSET: u8 = 'a' as u8;
    let mut s = (0, 0);
    let mut e = (0, 0);
    let map = data.iter()
        .enumerate()
        .flat_map(|(row, line)| {
            let line = line.as_ref();
            line.find("S").and_then(|col| Some(s = (row, col)));
            line.find("E").and_then(|col| Some(e = (row, col)));
            line.chars().map(|char| match char {
                'S' => 'a' as u8 - OFFSET,
                'E' => 'z' as u8 - OFFSET,
                c => c as u8 - OFFSET
            })
        })
        .collect::<Matrix<_>>()
        .reshape_rows(data.len());
    (s, e, map)
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "Sabqponm",
        "abcryxxl",
        "accszExk",
        "acctuvwj",
        "abdefghi",
    ];

    #[test]
    fn part1() {
        assert_eq!(31, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(29, solve2(&parse_data(DATA)));
    }
}
