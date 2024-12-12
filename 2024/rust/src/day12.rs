use std::collections::HashSet;

use crate::advent::{day, Error};

day!(12, parse, solve_a, solve_b);

type Input = Vec<Vec<char>>;

fn parse(text: &str) -> Result<Input, Error> {
    Ok(text.lines().map(|line| line.chars().collect()).collect())
}

fn fill(grid: &Input) -> Vec<HashSet<(isize, isize)>> {
    let rows = grid.len();
    let cols = grid[0].len();
    let mut visited = vec![vec![false; cols]; rows];
    let mut shapes = vec![];

    let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)];
    let in_bounds = |r: isize, c: isize| r >= 0 && r < rows as isize && c >= 0 && c < cols as isize;

    for row in 0..rows {
        for col in 0..cols {
            if visited[row][col] {
                continue;
            }

            let mut shape = HashSet::new();

            let plant_type = grid[row][col];
            let mut stack = vec![(row as isize, col as isize)];
            visited[row][col] = true;
            shape.insert((row as isize, col as isize));

            while let Some((cur_row, cur_col)) = stack.pop() {
                for &(dir_row, dir_col) in &directions {
                    let new_row = cur_row + dir_row;
                    let new_col = cur_col + dir_col;

                    if in_bounds(new_row, new_col) {
                        let nr = new_row as usize;
                        let nc = new_col as usize;
                        if grid[nr][nc] == plant_type && !visited[nr][nc] {
                            visited[nr][nc] = true;
                            shape.insert((nr as isize, nc as isize));
                            stack.push((nr as isize, nc as isize));
                        }
                    }
                }
            }

            shapes.push(shape);
        }
    }

    shapes
}

fn price_a(shape: HashSet<(isize, isize)>) -> u64 {
    let mut perimeter = 0;

    for &(row, col) in &shape {
        let u = !shape.contains(&(row - 1, col)) as u64;
        let d = !shape.contains(&(row + 1, col)) as u64;
        let l = !shape.contains(&(row, col - 1)) as u64;
        let r = !shape.contains(&(row, col + 1)) as u64;

        perimeter += u + d + l + r;
    }

    shape.len() as u64 * perimeter
}

fn solve_a(data: &Input) -> u64 {
    fill(data).into_iter().map(price_a).sum()
}

fn is_corner(cells: &[bool]) -> bool {
    matches!(cells, [false, _, false] | [true, false, true])
}

fn price_b(shape: HashSet<(isize, isize)>) -> u64 {
    let mut sides = 0;
    for &(row, col) in &shape {
        let around = [
            shape.contains(&(row - 1, col)),     // ↑
            shape.contains(&(row - 1, col + 1)), // ↗
            shape.contains(&(row, col + 1)),     // →
            shape.contains(&(row + 1, col + 1)), // ↘
            shape.contains(&(row + 1, col)),     // ↓
            shape.contains(&(row + 1, col - 1)), // ↙
            shape.contains(&(row, col - 1)),     // ←
            shape.contains(&(row - 1, col - 1)), // ↖
            shape.contains(&(row - 1, col)),     // ↑
        ];

        let ur = is_corner(&around[0..=2]) as u64; // ↗
        let dr = is_corner(&around[2..=4]) as u64; // ↘
        let dl = is_corner(&around[4..=6]) as u64; // ↙
        let ul = is_corner(&around[6..=8]) as u64; // ↖

        sides += ur + dr + dl + ul;
    }

    shape.len() as u64 * sides
}

fn solve_b(data: &Input) -> u64 {
    fill(data).into_iter().map(price_b).sum()
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE_1: &str = indoc! {"
        AAAA
        BBCD
        BBCC
        EEEC
    "};

    const SAMPLE_2: &str = indoc! {"
        OOOOO
        OXOXO
        OOOOO
        OXOXO
        OOOOO
    "};

    const SAMPLE_3: &str = indoc! {"
        RRRRIICCFF
        RRRRIICCCF
        VVRRRCCFFF
        VVRCCCJFFF
        VVVVCJJCFE
        VVIVCCJJEE
        VVIIICJJEE
        MIIIIIJJEE
        MIIISIJEEE
        MMMISSJEEE
    "};

    const SAMPLE_4: &str = indoc! {"
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
    "};

    const SAMPLE_5: &str = indoc! {"
        AAAAAA
        AAABBA
        AAABBA
        ABBAAA
        ABBAAA
        AAAAAA
    "};

    #[test]
    fn a() {
        let parsed_1 = parse(SAMPLE_1).unwrap();
        let result_1 = solve_a(&parsed_1);
        assert_eq!(result_1, 140);

        let parsed_2 = parse(SAMPLE_2).unwrap();
        let result_2 = solve_a(&parsed_2);
        assert_eq!(result_2, 772);

        let parsed_3 = parse(SAMPLE_3).unwrap();
        let result_3 = solve_a(&parsed_3);
        assert_eq!(result_3, 1930);
    }

    #[test]
    fn b() {
        let parsed_1 = parse(SAMPLE_1).unwrap();
        let result_1 = solve_b(&parsed_1);
        assert_eq!(result_1, 80);

        let parsed_2 = parse(SAMPLE_2).unwrap();
        let result_2 = solve_b(&parsed_2);
        assert_eq!(result_2, 436);

        let parsed_4 = parse(SAMPLE_4).unwrap();
        let result_4 = solve_b(&parsed_4);
        assert_eq!(result_4, 236);

        let parsed_5 = parse(SAMPLE_5).unwrap();
        let result_5 = solve_b(&parsed_5);
        assert_eq!(result_5, 368);

        let parsed_3 = parse(SAMPLE_3).unwrap();
        let result_3 = solve_b(&parsed_3);
        assert_eq!(result_3, 1206);
    }
}
