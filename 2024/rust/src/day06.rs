use std::collections::HashSet;

use crate::advent::{day, Error};

day!(6, parse, solve_a, solve_b);

type Point = (usize, usize);
type Casts = Box<[Vec<Point>]>;
type Input = (Point, Point, Casts, Casts);

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

    let mut cast_rows: Casts = Box::from(vec![vec![]; rows]);
    let mut cast_cols: Casts = Box::from(vec![vec![]; cols]);

    for point @ (row, col) in desks {
        cast_rows[row].push(point);
        cast_cols[col].push(point);
    }

    Ok((guard, (rows, cols), cast_rows, cast_cols))
}

fn solve_a((guard, (rows, cols), cast_rows, cast_cols): &Input) -> usize {
    let mut pos = *guard;
    let mut dir = Dir::U;

    let mut visited = HashSet::<Point>::from_iter([pos]);

    loop {
        let casts = match dir {
            Dir::L | Dir::R => cast_rows[pos.0].as_slice(),
            Dir::U | Dir::D => cast_cols[pos.1].as_slice(),
        };

        let hit = match dir {
            Dir::U => casts.iter().rev().find(|p| p.0 < pos.0),
            Dir::D => casts.iter().find(|p| p.0 > pos.0),
            Dir::L => casts.iter().rev().find(|p| p.1 < pos.1),
            Dir::R => casts.iter().find(|p| p.1 > pos.1),
        };

        if let Some(hit) = hit {
            let target = match dir {
                Dir::U => (hit.0 + 1, hit.1),
                Dir::D => (hit.0 - 1, hit.1),
                Dir::L => (hit.0, hit.1 + 1),
                Dir::R => (hit.0, hit.1 - 1),
            };

            match dir {
                Dir::U | Dir::D => {
                    let s = pos.0.min(target.0);
                    let e = pos.0.max(target.0);
                    visited.extend((s..=e).map(|row| (row, pos.1)));
                }
                Dir::L | Dir::R => {
                    let s = pos.1.min(target.1);
                    let e = pos.1.max(target.1);
                    visited.extend((s..=e).map(|col| (pos.0, col)));
                }
            };

            pos = target;
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
