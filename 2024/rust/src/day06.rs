use std::collections::HashSet;

use crate::advent::{day, Error};

day!(6, parse, solve_a, solve_b);

type Point = (usize, usize);
type Cache = Box<[Vec<Point>]>;
type Input = (Point, Point, Cache, Cache);

enum Dir {
    U,
    D,
    L,
    R,
}

impl Dir {
    fn turn_right(&self) -> Self {
        match self {
            Dir::U => Dir::R,
            Dir::D => Dir::L,
            Dir::L => Dir::U,
            Dir::R => Dir::D,
        }
    }
}

fn parse(text: &str) -> Result<Input, Error> {
    let cols = text.lines().next().map(str::len).unwrap();

    let mut guard = (0, 0);
    let mut desks = vec![];
    let mut rows = 0;

    for (row, line) in text.lines().enumerate() {
        rows += 1;

        for (col, char) in line.chars().enumerate() {
            match char {
                '#' => desks.push((row, col)),
                '^' => guard = (row, col),
                _ => (),
            }
        }
    }

    let mut hash_rows: Cache = Box::from(vec![vec![]; rows]);
    let mut hash_cols: Cache = Box::from(vec![vec![]; cols]);

    for point @ (row, col) in desks {
        hash_rows[row].push(point);
        hash_cols[col].push(point);
    }

    Ok((guard, (rows, cols), hash_rows, hash_cols))
}

fn solve_a((guard, (rows, cols), hash_rows, hash_cols): &Input) -> usize {
    let mut pos = *guard;
    let mut dir = Dir::U;

    let mut visited = HashSet::<Point>::from_iter([pos]);

    loop {
        let desks = match dir {
            Dir::L | Dir::R => hash_rows[pos.0].as_slice(),
            Dir::U | Dir::D => hash_cols[pos.1].as_slice(),
        };

        let first = match dir {
            Dir::U => desks.iter().rev().find(|p| p.0 < pos.0),
            Dir::D => desks.iter().find(|p| p.0 > pos.0),
            Dir::L => desks.iter().rev().find(|p| p.1 < pos.1),
            Dir::R => desks.iter().find(|p| p.1 > pos.1),
        };

        if let Some(first) = first {
            let (step_row, step_col) = match dir {
                Dir::U => (-1, 0),
                Dir::D => (1, 0),
                Dir::L => (0, -1),
                Dir::R => (0, 1),
            };

            let mut curr = pos;
            loop {
                let next = (
                    (curr.0 as isize + step_row) as usize,
                    (curr.1 as isize + step_col) as usize,
                );

                if next == (first.0, first.1) {
                    break;
                }

                curr = next;
                visited.insert(curr);
            }

            pos = curr;
            dir = dir.turn_right();
        } else {
            let path = match dir {
                Dir::U => 0..pos.0,
                Dir::D => pos.0..*rows,
                Dir::L => 0..pos.1,
                Dir::R => pos.1..*cols,
            };

            match dir {
                Dir::U | Dir::D => visited.extend(path.map(|row| (row, pos.1))),
                Dir::L | Dir::R => visited.extend(path.map(|col| (pos.0, col))),
            };

            break;
        }
    }

    visited.len()
}

fn solve_b(b: &Input) -> usize {
    2
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        ....#.....
        .........#
        ..........
        ..#.......
        .......#..
        ..........
        .#..^.....
        ........#.
        #.........
        ......#...
    "};

    #[test]
    fn a() {
        let input = parse(SAMPLE).unwrap();
        let result = solve_a(&input);

        assert_eq!(result, 41);
    }
}
