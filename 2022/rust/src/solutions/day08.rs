use std::cmp::max;
use std::collections::HashMap;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(8)));

    println!("Day 8");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[inline(always)]
fn mark_visible(
    data: &[Vec<i32>],
    visible: &mut Vec<Vec<bool>>,
    last_height: &mut i32,
    row: usize,
    col: usize
) {
    let height = data[row][col];
    if height > *last_height {
        visible[row][col] = true;
        *last_height = height;
    }
}

fn create_mask(data: &[Vec<i32>]) -> Vec<Vec<bool>> {
    let rows = data.len();
    let cols = data[0].len();

    let mut visible: Vec<Vec<bool>> = (0..rows)
        .into_iter()
        .map(|_| (0..cols).into_iter().map(|_| false).collect())
        .collect();

    for row in 0..rows {
        let mut last_height = i32::MIN;
        for col in 0..cols {
            mark_visible(data, &mut visible, &mut last_height, row, col);
        }
        let mut last_height = i32::MIN;
        for col in (0..cols).rev() {
            mark_visible(data, &mut visible, &mut last_height, row, col);
        }
        // Transposed passes
        let mut last_height = i32::MIN;
        for col in 0..cols {
            mark_visible(data, &mut visible, &mut last_height, col, row);
        }
        let mut last_height = i32::MIN;
        for col in (0..cols).rev() {
            mark_visible(data, &mut visible, &mut last_height, col, row);
        }
    };

    visible
}


fn solve1(data: &[Vec<i32>]) -> usize {
    create_mask(data).into_iter().map(|row| row.iter().filter(|&e| *e).count()).sum()
}

#[inline(always)]
fn find_limit(
    data: &[Vec<i32>],
    last_heights: &mut HashMap<i32, usize>,
    row: usize,
    col: usize
) -> usize {
    let mut limit = 0;
    for h in data[row][col]..=9 {
        if let Some(since) = last_heights.get(&h) {
            limit = max(limit, *since);
        }
    };
    limit
}

fn find_scores(data: &[Vec<i32>]) -> Vec<Vec<i32>> {
    let rows = data.len();
    let cols = data[0].len();

    let mut scores: Vec<Vec<i32>> = (0..rows)
        .into_iter()
        .map(|_| (0..cols).into_iter().map(|_| 1).collect())
        .collect();

    for row in 0..rows {
        let mut last_heights: HashMap<i32, usize> = HashMap::new();
        for col in 0..cols {
            let limit = find_limit(data, &mut last_heights, row, col);
            scores[row][col] = scores[row][col] * (col - limit) as i32;
            last_heights.insert(data[row][col], col);
        };
        let mut last_heights: HashMap<i32, usize> = HashMap::new();
        for col in (0..cols).rev() {
            let limit = find_limit(data, &mut last_heights, row, col);
            let limit = if limit == 0 { cols - 1 } else { limit };
            scores[row][col] = scores[row][col] * (limit - col) as i32;
            last_heights.insert(data[row][col], col);
        };
    };

    for col in 0..cols {
        let mut last_heights: HashMap<i32, usize> = HashMap::new();
        for row in 0..rows {
            let limit = find_limit(data, &mut last_heights, row, col);
            scores[row][col] = scores[row][col] * (row - limit) as i32;
            last_heights.insert(data[row][col], row);
        };
        let mut last_heights: HashMap<i32, usize> = HashMap::new();
        for row in (0..cols).rev() {
            let limit = find_limit(data, &mut last_heights, row, col);
            let limit = if limit == 0 { rows - 1 } else { limit };
            scores[row][col] = scores[row][col] * (limit - row) as i32;
            last_heights.insert(data[row][col], row);
        };
    };

    scores
}

fn solve2(data: &[Vec<i32>]) -> i32 {
    let scores = find_scores(data);

    let reducer = |a, b| max(a, b);
    scores.into_iter().map(|row| row.iter().copied().reduce(reducer).unwrap()).reduce(reducer).unwrap()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Vec<i32>> {
    data.iter()
        .map(|line| line.as_ref()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as i32)
            .collect::<Vec<_>>()
        )
        .collect::<Vec<_>>()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str; 5] = &[
        "30373",
        "25512",
        "65332",
        "33549",
        "35390",
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(21, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        let scores = find_scores(&data);
        assert_eq!(4, scores[1][2]);
        assert_eq!(8, scores[3][2]);
        assert_eq!(8, solve2(&data));
    }
}
