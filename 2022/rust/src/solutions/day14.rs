use std::fmt::{Display, Formatter};
use regex::Regex;
use crate::utils;
use crate::utils::matrix::Matrix;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(14)));

    println!("Day 14");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[repr(u8)]
#[derive(Copy, Clone, PartialEq)]
enum Tile { Empty, Rock, Sand }

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Empty => write!(f, ".")?,
            Tile::Rock => write!(f, "#")?,
            Tile::Sand => write!(f, "o")?,
        };
        Ok(())
    }
}

fn get_data_bounds(data: &[Vec<(usize, usize)>]) -> (usize, (usize, usize)) {
    data.iter()
        .flatten()
        .fold((usize::MAX, (usize::MIN, usize::MIN)), |(min_c, (max_r, max_c)), &(r, c)| (
            min_c.min(c), (max_r.max(r), max_c.max(c))
        ))
}

fn fill_grid_walls(data: &[Vec<(usize, usize)>], grid: &mut Matrix<Tile>) {
    for path in data.iter() {
        let mut path_iter = path.iter();
        path_iter.next()
            .map(|&start| path_iter
                .fold(start, |(prev_r, prev_c), &(next_r, next_c)| {
                    let (min_r, max_r) = (prev_r.min(next_r), prev_r.max(next_r));
                    let (min_c, max_c) = (prev_c.min(next_c), prev_c.max(next_c));
                    match prev_r == next_r {
                        true => (min_c..=max_c).into_iter()
                            .for_each(|c| grid[(prev_r, c)] = Tile::Rock),
                        false => (min_r..=max_r).into_iter()
                            .for_each(|r| grid[(r, prev_c)] = Tile::Rock),
                    };
                    (next_r, next_c)
                })
            )
            .unwrap();
    }
}


fn drop_sand(grid: &mut Matrix<Tile>, (row, col): (usize, usize)) -> Option<(usize, usize)> {
    if grid[(row, col)] != Tile::Empty {
        return None;
    }

    let ((_, min_c), (max_r, max_c)) = grid.bounds();
    for r in row..max_r {
        if grid[(r, col)] != Tile::Empty {
            return if col == min_c {
                None
            } else if grid[(r, col - 1)] == Tile::Empty {
                drop_sand(grid, (r, col - 1))
            } else if col == max_c {
                None
            } else if grid[(r, col + 1)] == Tile::Empty {
                drop_sand(grid, (r, col + 1))
            } else {
                let index = (r - 1, col);
                grid[index] = Tile::Sand;
                Some(index)
            }
        }
    }
    None
}


fn solve1(data: &[Vec<(usize, usize)>]) -> i32 {
    let (min_c, max) = get_data_bounds(data);
    let mut grid = Matrix::with_bounds((0, min_c), max, Tile::Empty);
    fill_grid_walls(data, &mut grid);

    let mut sand_count = 0;
    while let Some(_) = drop_sand(&mut grid, (0, 500)) {
        sand_count += 1;
    }

    sand_count
}

fn solve2(data: &[Vec<(usize, usize)>]) -> i32 {
    let (min_c, (max_r, max_c)) = get_data_bounds(data);
    let new_max_r = max_r + 2;
    let new_min_c = min_c - (max_r + 2);
    let new_max_c = max_c + (max_r + 2);
    let mut grid = Matrix::with_bounds((0, new_min_c), (new_max_r, new_max_c), Tile::Empty);
    fill_grid_walls(data, &mut grid);

    for c in new_min_c..=new_max_c {
        grid[(new_max_r, c)] = Tile::Rock;
    }

    let mut sand_count = 0;
    while let Some(_) = drop_sand(&mut grid, (0, 500)) {
        sand_count += 1;
    }

    sand_count
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Vec<(usize, usize)>> {
    let re = Regex::new(r#"(\d+,\d+)"#).unwrap();
    data.iter()
        .map(|line| re
            .captures_iter(line.as_ref())
            .map(|s| {
                let mut parts = s.get(1).unwrap().as_str().split(",");
                let c: usize = parts.next().unwrap().parse().unwrap();
                let r: usize = parts.next().unwrap().parse().unwrap();
                (r, c)
            })
            .collect()
        )
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "498,4 -> 498,6 -> 496,6",
        "503,4 -> 502,4 -> 502,9 -> 494,9",
    ];

    #[test]
    fn part1() {
        assert_eq!(24, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(93, solve2(&parse_data(DATA)));
    }
}
