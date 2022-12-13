use std::cmp::Ordering;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(13)));

    println!("Day 13");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Debug, Clone, Eq, PartialEq)]
enum Packet {
    List(Vec<Packet>),
    Integer(i32),
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Integer(left), Packet::Integer(right)) => {
                left.cmp(right)
            }
            (Packet::List(left), Packet::List(right)) => {
                for (left, right) in left.iter().zip(right) {
                    match left.cmp(right) {
                        Ordering::Less => return Ordering::Less,
                        Ordering::Greater => return Ordering::Greater,
                        _ => ()
                    }
                }
                // final decision is by length
                left.len().cmp(&right.len())
            }
            (Packet::Integer(left), right) => {
                Packet::List(vec![Packet::Integer(*left)]).cmp(right)
            }
            (left, Packet::Integer(right)) => {
                left.cmp(&Packet::List(vec![Packet::Integer(*right)]))
            }
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


fn solve1(data: &[(Packet, Packet)]) -> usize {
    data.iter()
        .enumerate()
        .flat_map(|(index, (left, right))| match left.cmp(right) {
            Ordering::Less => Some(index + 1),
            _ => None
        })
        .sum()
}

fn solve2(data: &[(Packet, Packet)]) -> i32 {
    2
}


fn find_tokens(s: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut buffer = Vec::new();
    let mut depth = 0;
    for char in s.chars() {
        match (depth, char) {
            (0, '[') => depth += 1,
            (1, ']') => {
                if buffer.len() != 0 {
                    tokens.push(buffer.drain(..).collect());
                }
                return tokens;
            }
            (1, ',') => tokens.push(buffer.drain(..).collect()),
            (_, char) => {
                buffer.push(char);
                match char {
                    '[' => depth += 1,
                    ']' => depth -= 1,
                    _ => ()
                }
            }
        }
    }
    panic!()
}


fn recursive_parse(s: &str) -> Packet {
    match s.starts_with("[") {
        true => {
            Packet::List(find_tokens(s).into_iter().map(|t| recursive_parse(&t)).collect())
        },
        false => Packet::Integer(s.parse().unwrap())
    }
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<(Packet, Packet)> {
    data.chunks(3)
        .map(|chunk| (
            recursive_parse(chunk[0].as_ref()),
            recursive_parse(chunk[1].as_ref()),
        ))
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "[1,1,3,1,1]", "[1,1,5,1,1]", "",
        "[[1],[2,3,4]]", "[[1],4]", "",
        "[9]", "[[8,7,6]]", "",
        "[[4,4],4,4]", "[[4,4],4,4,4]", "",
        "[7,7,7,7]", "[7,7,7]", "",
        "[]", "[3]", "",
        "[[[]]]", "[[]]", "",
        "[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]",
    ];

    #[test]
    fn part1() {
        assert_eq!(13, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
