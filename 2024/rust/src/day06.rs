use crate::advent::{day, Error};

day!(6, parse, solve_a, solve_b);

type Point = (usize, usize);
type Casts = Box<[Vec<Point>]>;
type Input = (Point, Point, Casts, Casts);

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Dir {
    U = 1 << 0,
    D = 1 << 1,
    L = 1 << 2,
    R = 1 << 3,
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

fn interpolate(a: usize, b: usize) -> Box<dyn Iterator<Item = usize>> {
    if b < a {
        Box::new((b..=a).rev())
    } else {
        Box::new(a..=b)
    }
}

fn solve_a(input @ (start, (rows, cols), _, _): &Input) -> usize {
    let mut visited = vec![false; rows * cols];
    visited[start.0 * cols + start.1] = true;

    for (dir, (s_row, s_col), (e_row, e_col)) in find_segments(input) {
        let points: Box<dyn Iterator<Item = Point>> = match dir {
            Dir::U | Dir::D => Box::new(interpolate(s_row, e_row).map(|row| (row, s_col))),
            Dir::L | Dir::R => Box::new(interpolate(s_col, e_col).map(|col| (s_row, col))),
        };

        for (row, col) in points {
            visited[row * cols + col] = true;
        }
    }

    visited.into_iter().filter(|b| *b).count()
}

fn check_cycle(
    (_, (_, cols), cast_rows, cast_cols): &Input,
    temp: &mut [u8],
    before: Point,
    insert: Point,
    dir: Dir,
) -> bool {
    temp.fill(0);

    let mut pos = before;
    let mut dir = dir.turn_right();

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

        let cell = &mut temp[target.0 * cols + target.1];

        if *cell & dir as u8 != 0 {
            return true;
        };

        *cell |= dir as u8;

        pos = target;
        dir = dir.turn_right();
    }
}

fn solve_b(input @ (start, (rows, cols), _, _): &Input) -> usize {
    let mut visited = vec![false; rows * cols];
    let mut inserts = vec![false; rows * cols];

    let mut temp = vec![0; rows * cols];
    let mut curr = *start;
    for (dir, (s_row, s_col), (e_row, e_col)) in find_segments(input) {
        let points: Box<dyn Iterator<Item = Point>> = match dir {
            Dir::U | Dir::D => Box::new(interpolate(s_row, e_row).map(|row| (row, s_col))),
            Dir::L | Dir::R => Box::new(interpolate(s_col, e_col).map(|col| (s_row, col))),
        };

        for next in points {
            let visited = &mut visited[next.0 * cols + next.1];
            let inserts = &mut inserts[next.0 * cols + next.1];

            if !*visited {
                // assume the point blocks
                if check_cycle(input, &mut temp, curr, next, dir) {
                    *inserts = true;
                }
            }

            *visited = true;
            curr = next;
        }
    }

    inserts.into_iter().filter(|b| *b).count()
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
