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
        .map(|_| (0..cols)
            .into_iter()
            .map(|_| false)
            .collect()
        )
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
    let mask = create_mask(data);

    mask.into_iter()
        .map(|row| row.iter()
            .filter(|&e| *e)
            .count()
        )
        .sum()
}

fn solve2(data: &[Vec<i32>]) -> i32 {
    2
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
        assert_eq!(2, solve2(&data));
    }
}
