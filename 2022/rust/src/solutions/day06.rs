use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(6)));

    println!("Day 6");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &Vec<char>) -> usize {
    marker_for(&data, 4)
}

fn solve2(data: &Vec<char>) -> usize {
    marker_for(&data, 14)
}

fn marker_for(data: &Vec<char>, window_len: usize) -> usize {
    data.windows(window_len)
        .enumerate()
        .filter_map(|(idx, window)| window.iter()
            .collect::<HashSet<_>>()
            .len().eq(&window_len)
            .then_some(idx + window_len)
        )
        .next().unwrap()
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
        assert_eq!(19, solve2(&data));
    }
}
