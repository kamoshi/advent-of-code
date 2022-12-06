use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(6)));

    println!("Day 6");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &Vec<char>) -> usize {
    data.windows(4)
        .enumerate()
        .filter_map(|(idx, window)| {
            let unique = window.iter().copied().collect::<HashSet<char>>().len() == 4;
            match unique {
                true => Some(idx + 4),
                false => None
            }
        })
        .next().unwrap()
}

fn solve2(data: &Vec<char>) -> i32 {
    2
}


fn parse_data(data: Vec<String>) -> Vec<char> {
    data.into_iter()
        .next()
        .map(|char| char
            .chars()
            .collect()
        )
        .unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;

    fn data() -> Vec<String> {
        vec![
           "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        ]
            .into_iter().map(String::from).collect()
    }

    #[test]
    fn part1() {
        let data = parse_data(data());
        assert_eq!(7, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data());
        assert_eq!(2, solve2(&data));
    }
}
