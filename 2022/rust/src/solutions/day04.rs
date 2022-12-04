#![allow(dead_code)]
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(4)));

    println!("Day 4");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &Vec<((i32, i32), (i32, i32))>) -> usize {
    data.iter()
        .filter(|((s1, e1), (s2, e2))|
            s1 <= s2 && e2 <= e1 || s2 <= s1 && e1 <= e2
        )
        .count()
}

fn solve2(data: &Vec<((i32, i32), (i32, i32))>) -> usize {
    data.iter()
        .filter(|((s1, e1), (s2, e2))|
            s1 <= e2 && s2 <= e1
        )
        .count()
}


fn parse_data(data: Vec<String>) -> Vec<((i32, i32), (i32, i32))> {
    let re = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)$").unwrap();
    data.iter()
        .map(|s| {
            let c = re.captures(s).unwrap();
            (
                (
                    c.get(1).unwrap().as_str().parse().unwrap(),
                    c.get(2).unwrap().as_str().parse().unwrap(),
                ),
                (
                    c.get(3).unwrap().as_str().parse().unwrap(),
                    c.get(4).unwrap().as_str().parse().unwrap(),
                ),
            )
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    fn data() -> Vec<&'static str> {
        vec!["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"]
    }

    #[test]
    fn part1() {
        let data = parse_data(data().into_iter().map(String::from).collect());
        assert_eq!(2, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data().into_iter().map(String::from).collect());
        assert_eq!(4, solve2(&data));
    }
}
