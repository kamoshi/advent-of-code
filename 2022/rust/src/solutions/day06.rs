use std::collections::HashSet;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(6)));

    println!("Day 6");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &[char]) -> usize {
    marker_bitwise(data, 4)
}

fn solve2(data: &[char]) -> usize {
    marker_bitwise(data, 14)
}

fn marker_iter(data: &[char], window_len: usize) -> usize {
    data.windows(window_len)
        .enumerate()
        .filter_map(|(idx, window)| window.iter()
            .collect::<HashSet<_>>()
            .len().eq(&window_len)
            .then_some(idx + window_len)
        )
        .next().unwrap()
}

fn marker_bitwise(data: &[char], window_len: usize) -> usize {
    static OFFSET: u8 = 'a' as u8;
    let mut acc: u32 = 0;
    let mut idx: usize = 0;
    for &char in data {
        acc ^= 1 << char as u8 - OFFSET;
        if idx >= window_len {
            acc ^= 1 << data[idx - window_len] as u8 - OFFSET;
            if acc.count_ones() == window_len as u32 {
                return idx + 1;
            }
        }
        idx += 1;
    }
    return usize::MAX;
}


fn parse_data(data: Vec<String>) -> Vec<char> {
    data.into_iter()
        .next()
        .map(|char| char.chars().collect())
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
