#![allow(dead_code)]
use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(18)));

    println!("Day 18");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Point = (isize, isize, isize);


fn sides_for((x, y, z): Point) -> impl Iterator<Item=Point> {
    [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)].into_iter()
}

fn solve1(data: &[Point]) -> i32 {
    let points: HashSet<Point> = HashSet::from_iter(data.iter().copied());

    let mut side_sum = 0;
    for &point in data {
        for side in sides_for(point) {
            if !points.contains(&side) {
                side_sum += 1;
            }
        }
    }
    side_sum
}

fn solve2(data: &[Point]) -> i32 {
    let points: HashSet<Point> = HashSet::from_iter(data.iter().copied());
    let (x_min, y_min, z_min) = data.iter().copied().reduce(|(x1, y1, z1), (x2, y2, z2)| (x1.min(x2), y1.min(y2), z1.min(z2))).unwrap();
    let (x_max, y_max, z_max) = data.iter().copied().reduce(|(x1, y1, z1), (x2, y2, z2)| (x1.max(x2), y1.max(y2), z1.max(z2))).unwrap();

    let (x_min, y_min, z_min) = (x_min - 1, y_min - 1, z_min - 1);
    let (x_max, y_max, z_max) = (x_max + 1, y_max + 1, z_max + 1);

    let fits = move |&(x, y, z): &Point| {
        x_min <= x && x <= x_max && y_min <= y && y <= y_max && z_min <= z && z <= z_max
    };

    let mut to_check: Vec<Point> = vec![(x_min, y_min, z_min)];
    let mut visited: HashSet<Point> = HashSet::new();
    let mut side_sum = 0;
    while let Some(current) = to_check.pop() {
        match visited.contains(&current) {
            true => continue,
            false => visited.insert(current),
        };
        for neighbour in sides_for(current).filter(fits) {
            match points.contains(&neighbour) {
                true => side_sum += 1,
                false => to_check.push(neighbour),
            }
        }
    }
    side_sum
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
        assert_eq!(58, solve2(&parse_data(DATA)));
    }
}
