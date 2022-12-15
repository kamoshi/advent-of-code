#![allow(dead_code)]
use std::collections::{HashMap, HashSet};
use std::ops::RangeInclusive;
use regex::{CaptureMatches, Regex};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(15)));

    println!("Day 15");
    println!("Part 1: {}", solve1::<2000000>(&data));
    println!("Part 2: {}", solve2::<4000000>(&data));
}


type Data = [((isize, isize), (isize, isize))];
type Range = RangeInclusive<isize>;


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

fn range_for_row((r, c): (isize, isize), range: usize, row: isize) -> Option<Range> {
    let delta_row = r.abs_diff(row);
    match delta_row <= range {
        true => {
            let length = (range - range.min(delta_row)) as isize;
            let (left, right) = (c - length, c + length);
            Some(left..=right)
        }
        false => None
    }
}

fn ranges_for_row(ranges: &HashMap<(isize, isize), usize>, row: isize) -> impl Iterator<Item = Range> + '_ {
    ranges.iter()
        .filter_map(move |(&point, &range)|
            range_for_row(point, range, row)
        )
}

fn merge_ranges(mut ranges: Vec<Range>) -> Vec<Range> {
    ranges.sort_unstable_by(|r1, r2| r1.start().cmp(&r2.start()));
    let mut ranges = ranges.into_iter();
    ranges.next()
        .map(|first| ranges
            .fold(vec![first], |mut acc, next| {
                match next.start() - 1 <= *acc.last().unwrap().end() {
                    true => {
                        let prev = acc.pop().unwrap();
                        let merged_l = *next.start().min(prev.start());
                        let merged_r = *next.end().max(prev.end());
                        acc.push(merged_l..=merged_r);
                    },
                    false => acc.push(next),
                };
                acc
            }))
        .unwrap()
}

fn solve1<const ROW: isize>(data: &Data) -> usize {
    let ranges = find_ranges(data);
    let occupied = data.iter()
        .fold(HashSet::new(), |mut acc, (a, b)| {
            if a.0 == ROW { acc.insert(a); }
            if b.0 == ROW { acc.insert(b); }
            acc
        });

    let sum = merge_ranges(ranges_for_row(&ranges, ROW).collect())
        .iter()
        .map(|range| (*range.end() - *range.start() + 1) as usize)
        .sum::<usize>();

    sum - occupied.len()
}

fn solve2<const SEARCH_SPACE: isize>(data: &Data) -> isize {
    let ranges = find_ranges(data);

    (0..SEARCH_SPACE).into_iter()
        .flat_map(|cur_row| {
            let ranges = ranges_for_row(&ranges, cur_row)
                .filter(|range|
                    -1 <= *range.end() && *range.end() < SEARCH_SPACE || 0 < *range.start() && *range.start() <= SEARCH_SPACE + 1
                )
                .collect();

            let mut ranges = merge_ranges(ranges).into_iter();
            let spots = ranges.next()
                .map(|first| ranges
                    .fold((vec![], first), |(mut acc, prev), next| {
                        match prev.end() + 2 == *next.start() {
                            true => {
                                acc.push(prev.end() + 1);
                                (acc, next)
                            }
                            false => (acc, next)
                        }
                    }))
                .unwrap().0;
            spots.first().map(|&col| col * 4000000 + cur_row)
        })
        .next().unwrap()
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
        assert_eq!(26, solve1::<10>(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(56000011, solve2::<20>(&parse_data(DATA)));
    }
}
