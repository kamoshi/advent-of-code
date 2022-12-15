use std::collections::{HashMap, HashSet};
use regex::{CaptureMatches, Regex};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(15)));

    println!("Day 15");
    println!("Part 1: {}", solve1(&data, 2000000));
    println!("Part 2: {}", solve2(&data));
}


type Data = [((isize, isize), (isize, isize))];


fn distance(a: (isize, isize), b: (isize, isize)) -> usize {
    a.0.abs_diff(b.0) + a.1.abs_diff(b.1)
}

fn find_ranges(data: &Data) -> HashMap<(isize, isize), usize> {
    data.iter()
        .map(|&(sensor, beacon)|
            (sensor, distance(sensor, beacon))
        )
        .collect()
}


fn find_bounds(data: &Data) -> (isize, isize) {
    data.iter()
        .fold((0, 0), |acc, &(sensor, beacon)| {
            (
                acc.0.max(sensor.0.max(beacon.0)),
                acc.1.max(sensor.0.max(beacon.1)),
            )
        })
}

fn solve1(data: &Data, row: isize) -> usize {
    let ranges = find_ranges(data);
    let (_, cols) = find_bounds(data);
    let max_range = *ranges.values().max().unwrap();
    let buffer =  (2 * max_range + 1) as isize;
    let occupied = data.iter()
        .fold(HashSet::new(), |mut acc, (a, b)| {
            acc.insert(a);
            acc.insert(b);
            acc
        });

    let mut count = 0;
    for col in (0 - buffer)..(cols + buffer) {
        let index = (row, col);
        if occupied.contains(&index) {
            continue;
        }

        let in_range = ranges.iter()
            .any(|(&sensor, &max_dist)|
                sensor != index && distance(sensor, index) <= max_dist
            );
        if in_range { count += 1; }
    }
    count
}

fn solve2(_data: &Data) -> i32 {
    2
}


fn extract_coord(iter: &mut CaptureMatches) -> (isize, isize) {
    iter.next()
        .map(|c| {
            let mut cap = c.iter();
            let x = cap.nth(1).unwrap().unwrap().as_str().parse::<isize>().unwrap();
            let y = cap.next().unwrap().unwrap().as_str().parse::<isize>().unwrap();
            (y, x)
        })
        .unwrap()
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<((isize, isize), (isize, isize))> {
    let re = Regex::new(r#"x=(-?\d+), y=(-?\d+)"#).unwrap();
    data.iter()
        .map(|line| {
            let mut iter = re.captures_iter(line.as_ref());
            let sensor = extract_coord(&mut iter);
            let beacon = extract_coord(&mut iter);
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
        assert_eq!(26, solve1(&parse_data(DATA), 10));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(56000011, solve2(&data));
    }
}
