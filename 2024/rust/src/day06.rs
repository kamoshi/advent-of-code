use std::collections::{HashMap, HashSet};

use crate::advent::{day, Error};

day!(6, parse, solve_a, solve_b);

type Point = (usize, usize);
type Casts = Box<[Vec<Point>]>;
type Input = (Point, Point, Casts, Casts);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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

fn find_segments(
    (start, (rows, cols), cast_rows, cast_cols): &Input,
) -> impl Iterator<Item = (Dir, Point, Point)> + '_ {
    let mut pos = *start;
    let mut dir = Dir::U;
    let mut end = false;

    std::iter::from_fn(move || {
        if end {
            return None;
        }

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

            let segment = (dir, pos, target);

            pos = target;
            dir = dir.turn_right();

            Some(segment)
        } else {
            let path_end = match dir {
                Dir::U => (0, pos.1),
                Dir::D => (*rows - 1, pos.1),
                Dir::L => (pos.0, 0),
                Dir::R => (pos.0, *cols - 1),
            };

            end = true;

            Some((dir, pos, path_end))
        }
    })
}

fn solve_a(input: &Input) -> usize {
    let mut visited = HashSet::<Point>::new();
    visited.insert(input.0);

    for (dir, pos, target) in find_segments(input) {
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
    }

    visited.len()
}

fn check_cycle(
    (_, _, cast_rows, cast_cols): &Input,
    before: Point,
    insert: Point,
    dir: Dir,
) -> bool {
    let mut pos = before;
    let mut dir = dir.turn_right();
    let mut targets = HashSet::new();

    // println!("{:?} {:?} {:?}", before, insert, dir);
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

        let insert_valid = match dir {
            Dir::U => insert.0 < pos.0 && insert.1 == pos.1,
            Dir::D => insert.0 > pos.0 && insert.1 == pos.1,
            Dir::L => insert.1 < pos.1 && insert.0 == pos.0,
            Dir::R => insert.1 > pos.1 && insert.0 == pos.0,
        };

        let hit = match hit {
            Some(hit) if insert_valid => {
                if match dir {
                    Dir::U => insert.0 > hit.0,
                    Dir::D => insert.0 < hit.0,
                    Dir::L => insert.1 > hit.1,
                    Dir::R => insert.1 < hit.1,
                } {
                    insert
                } else {
                    *hit
                }
            }
            Some(hit) => *hit,
            None if insert_valid => insert,
            None => return false,
        };

        let target = match dir {
            Dir::U => (hit.0 + 1, hit.1),
            Dir::D => (hit.0 - 1, hit.1),
            Dir::L => (hit.0, hit.1 + 1),
            Dir::R => (hit.0, hit.1 - 1),
        };

        // println!("{} {:?} {:?} {:?}", insert_valid, hit, target, dir);
        // println!("{:?} {:?}", targets_temp.get(&target), Some(&dir));
        if targets.contains(&(target, dir)) {
            return true;
        };

        targets.insert((target, dir));

        pos = target;
        dir = dir.turn_right();
    }
}

fn interpolate(a: usize, b: usize) -> Box<dyn Iterator<Item = usize>> {
    if b < a {
        Box::new((b..=a).rev())
    } else {
        Box::new(a..=b)
    }
}

fn solve_b(input: &Input) -> usize {
    let mut inserts = HashSet::<Point>::new();

    let mut before = input.0;
    for (dir, pos, target) in find_segments(input) {
        match dir {
            Dir::U | Dir::D => {
                for point in interpolate(pos.0, target.0).map(|row| (row, pos.1)) {
                    // assume the point blocks
                    let is_cycle = check_cycle(input, before, point, dir);
                    // println!("checking {:?} {}", before, is_cycle);
                    if is_cycle {
                        inserts.insert(point);
                    }

                    before = point;
                }
            }
            Dir::L | Dir::R => {
                for point in interpolate(pos.1, target.1).map(|col| (pos.0, col)) {
                    // assume the point blocks
                    let is_cycle = check_cycle(input, before, point, dir);
                    // println!("checking {:?} {}", before, is_cycle);

                    if is_cycle {
                        inserts.insert(point);
                    }
                    before = point;
                }
            }
        };
    }

    inserts.len()
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
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_a(&parsed);

        assert_eq!(result, 41);
    }

    #[test]
    fn b() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_b(&parsed);

        assert_eq!(result, 6);
    }
}
