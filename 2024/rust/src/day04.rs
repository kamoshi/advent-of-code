use crate::advent::{day, Error};

day!(4, parse, solve_a, solve_b);

type Input = (usize, usize, Box<[u8]>);

fn parse(text: &str) -> Result<Input, Error> {
    let mut data = Vec::with_capacity(text.as_bytes().len());
    let mut iter = text.lines();

    let first = iter.next().unwrap().as_bytes();
    let cols = first.len();
    data.extend_from_slice(first);

    for line in iter {
        data.extend_from_slice(line.as_bytes());
    }

    let rows = data.len() / cols;
    assert_eq!(cols * rows, data.len());

    Ok((rows, cols, Box::from(data)))
}

#[inline(always)]
fn as_offset(row: usize, col: usize, cols: usize) -> usize {
    row * cols + col
}

fn solve_a(&(rows, cols, ref grid): &Input) -> i32 {
    let rows_pad = rows.saturating_sub(3);
    let cols_pad = cols.saturating_sub(3);
    let mut found = 0;

    for row in 0..rows {
        for col in 0..cols {
            let base = as_offset(row, col, cols);

            // horizontal
            if col < cols_pad {
                match &grid[base..base + 4] {
                    b"XMAS" | b"SAMX" => found += 1,
                    _ => (),
                }
            }

            // vertical
            if row < rows_pad {
                match &[
                    grid[base],
                    grid[base + cols],
                    grid[base + cols + cols],
                    grid[base + cols + cols + cols],
                ] {
                    b"XMAS" | b"SAMX" => found += 1,
                    _ => (),
                }
            }

            // diagonal
            if row < rows_pad && col < cols_pad {
                match &[
                    grid[base],
                    grid[base + cols + 1],
                    grid[base + cols + cols + 2],
                    grid[base + cols + cols + cols + 3],
                ] {
                    b"XMAS" | b"SAMX" => found += 1,
                    _ => (),
                };

                match &[
                    grid[base + 3],
                    grid[base + cols + 2],
                    grid[base + cols + cols + 1],
                    grid[base + cols + cols + cols],
                ] {
                    b"XMAS" | b"SAMX" => found += 1,
                    _ => (),
                }
            }
        }
    }

    found
}

fn solve_b(&(rows, cols, ref grid): &Input) -> i32 {
    let rows_pad = rows.saturating_sub(2);
    let cols_pad = cols.saturating_sub(2);

    let mut found = 0;
    for row in 0..rows_pad {
        for col in 0..cols_pad {
            let base = as_offset(row, col, cols);

            match &[
                grid[base],
                grid[base + cols + 1],
                grid[base + cols + cols + 2],
            ] {
                b"MAS" | b"SAM" => (),
                _ => continue,
            };

            match &[
                grid[base + 2],
                grid[base + cols + 1],
                grid[base + cols + cols],
            ] {
                b"MAS" | b"SAM" => found += 1,
                _ => (),
            };
        }
    }

    found
}

#[cfg(test)]
mod test {

    const SAMPLE: &str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    #[test]
    fn a() {
        use super::*;

        let data = parse(SAMPLE).unwrap();
        assert_eq!(solve_a(&data), 18);
    }

    #[test]
    fn b() {
        use super::*;

        let data = parse(SAMPLE).unwrap();
        assert_eq!(solve_b(&data), 9);
    }
}
