use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(18)));

    println!("Day 18");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Point = (isize, isize, isize);


fn solve1(data: &[Point]) -> i32 {
    let points: HashSet<Point> = HashSet::from_iter(data.iter().copied());

    let mut side_sum = 0;
    for &(p_x, p_y, p_z) in data {
        let sides = [
            (p_x - 1, p_y, p_z), (p_x + 1, p_y, p_z),
            (p_x, p_y - 1, p_z), (p_x, p_y + 1, p_z),
            (p_x, p_y, p_z - 1), (p_x, p_y, p_z + 1),
        ];

        for side in sides {
            if !points.contains(&side) {
                side_sum += 1;
            }
        }
    }
    side_sum
}

fn solve2(data: &[Point]) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Point> {
    data.iter()
        .map(|line| {
            let mut coords = line.as_ref().split(",").into_iter();
            (
                coords.next().unwrap().parse().unwrap(),
                coords.next().unwrap().parse().unwrap(),
                coords.next().unwrap().parse().unwrap(),
            )
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "2,2,2", "1,2,2", "3,2,2", "2,1,2", "2,3,2", "2,2,1", "2,2,3",
        "2,2,4", "2,2,6", "1,2,5", "3,2,5", "2,1,5", "2,3,5",
    ];

    #[test]
    fn part1() {
        assert_eq!(64, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
