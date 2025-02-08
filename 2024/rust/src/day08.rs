use crate::advent::{day, Error};

day!(8, parse, solve_a, solve_b);

type Point = (usize, usize);
type Input = ((usize, usize), Vec<Vec<Point>>);

fn parse(text: &str) -> Result<Input, Error> {
    const ZERO: usize = '0' as usize;
    let mut data = vec![Vec::default(); 128];
    let mut rows = 0;
    let mut cols = 0;

    for (row, line) in text.lines().enumerate() {
        cols = cols.max(line.len());
        for (col, char) in line.chars().enumerate() {
            match char {
                '.' => (),
                node => data[node as usize - ZERO].push((row, col)),
            }
        }

        rows += 1;
    }

    Ok(((rows, cols), data))
}

fn mirror(n: usize, pivot: usize, diff: usize, max: usize) -> Option<usize> {
    if n <= pivot {
        let next = pivot + diff;
        (next < max).then_some(next) // discard above max
    } else {
        (diff <= pivot).then(|| pivot - diff) // discard below 0
    }
}

fn solve_a(((rows, cols), data): &Input) -> usize {
    let mut set = vec![false; rows * cols];

    for segment in data {
        for (idx, a) in segment.iter().enumerate() {
            for b in &segment[idx..] {
                if a == b {
                    continue;
                }

                let (a_row, a_col) = *a;
                let (b_row, b_col) = *b;

                let diff_row = a_row.abs_diff(b_row);
                let diff_col = a_col.abs_diff(b_col);

                if let Some(row) = mirror(a_row, b_row, diff_row, *rows) {
                    if let Some(col) = mirror(a_col, b_col, diff_col, *cols) {
                        set[row * cols + col] = true; // mark conode
                    }
                }

                if let Some(row) = mirror(b_row, a_row, diff_row, *rows) {
                    if let Some(col) = mirror(b_col, a_col, diff_col, *cols) {
                        set[row * cols + col] = true; // mark conode
                    }
                }
            }
        }
    }

    set.into_iter().filter(|x| *x).count()
}

fn ripple(n: usize, pivot: usize, diff: usize, max: usize) -> Box<dyn Iterator<Item = usize>> {
    let mut next = pivot;

    if n <= pivot {
        Box::new(std::iter::from_fn(move || {
            next += diff;
            (next < max).then_some(next) // discard above max
        }))
    } else {
        Box::new(std::iter::from_fn(move || match diff <= next {
            true => {
                next -= diff;
                Some(next)
            }
            false => None,
        }))
    }
}

fn solve_b(((rows, cols), data): &Input) -> usize {
    let mut set = vec![false; rows * cols];

    for segment in data {
        for (idx, a) in segment.iter().enumerate() {
            set[a.0 * cols + a.1] = true; // mark conode

            for b in &segment[idx..] {
                if a == b {
                    continue;
                }

                let (a_row, a_col) = *a;
                let (b_row, b_col) = *b;

                let diff_row = a_row.abs_diff(b_row);
                let diff_col = a_col.abs_diff(b_col);

                for (row, col) in
                    ripple(a_row, b_row, diff_row, *rows).zip(ripple(a_col, b_col, diff_col, *cols))
                {
                    set[row * cols + col] = true; // mark conode
                }

                for (row, col) in
                    ripple(b_row, a_row, diff_row, *rows).zip(ripple(b_col, a_col, diff_col, *cols))
                {
                    set[row * cols + col] = true; // mark conode
                }
            }
        }
    }

    set.into_iter().filter(|b| *b).count()
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        ............
        ........0...
        .....0......
        .......0....
        ....0.......
        ......A.....
        ............
        ............
        ........A...
        .........A..
        ............
        ............
    "};

    #[test]
    fn a() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_a(&parsed);
        assert_eq!(result, 14);
    }

    #[test]
    fn b() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_b(&parsed);
        assert_eq!(result, 34);
    }
}
