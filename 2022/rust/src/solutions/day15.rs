use std::collections::HashMap;
use regex::Regex;
use crate::utils;
use crate::utils::matrix::Matrix;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(15)));

    println!("Day 15");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Data = [((usize, usize), (isize, isize))];


fn distance(a: (usize, usize), b: (isize, isize)) -> usize {
    (a.0 as isize).abs_diff(b.0) + (a.1 as isize).abs_diff(b.1)
}

fn find_ranges(data: &Data) -> HashMap<(usize, usize), usize> {
    data.iter()
        .map(|&(sensor, beacon)|
            (sensor, distance(sensor, beacon))
        )
        .collect()
}


fn find_bounds(data: &Data) -> (usize, usize) {
    data.iter()
        .fold((0, 0), |acc, &(sensor, beacon)| {
            (
                acc.0.max(sensor.0.max(beacon.0.max(0) as usize)),
                acc.1.max(sensor.0.max(beacon.1.max(0) as usize)),
            )
        })
}

fn solve1(data: &Data) -> usize {
    let ranges = find_ranges(data);
    let (rows, cols) = find_bounds(data);
    let max_range = *ranges.values().max().unwrap();
    1
}

fn solve2(data: &Data) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<((usize, usize), (isize, isize))> {
    let re = Regex::new(r#"x=(-?\d+), y=(-?\d+)"#).unwrap();
    data.iter()
        .map(|line| {
            let mut iter = re.captures_iter(line.as_ref());
            let sensor = iter.next()
                .map(|c| {
                    let mut cap = c.iter();
                    let x = cap.nth(1).unwrap().unwrap().as_str().parse::<usize>().unwrap();
                    let y = cap.next().unwrap().unwrap().as_str().parse::<usize>().unwrap();
                    (y, x)
                })
                .unwrap();
            let beacon = iter.next()
                .map(|c| {
                    let mut cap = c.iter();
                    let x = cap.nth(1).unwrap().unwrap().as_str().parse::<isize>().unwrap();
                    let y = cap.next().unwrap().unwrap().as_str().parse::<isize>().unwrap();
                    (y, x)
                })
                .unwrap();
            (sensor, beacon)
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
        "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
        "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
        "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
        "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
        "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
        "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
        "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
        "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
        "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
        "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
        "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
        "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
        "Sensor at x=20, y=1: closest beacon is at x=15, y=3",
    ];

    #[test]
    fn part1() {
        assert_eq!(26, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
