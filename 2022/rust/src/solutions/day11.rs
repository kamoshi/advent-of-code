#![allow(dead_code)]
use std::collections::{BinaryHeap, HashMap, VecDeque};
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(11)));

    println!("Day 11");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Copy, Clone)]
enum Operation {
    OldTimesOld,
    OldPlus(i64),
    OldTimes(i64),
}

struct Monkey {
    items: Vec<i64>,
    operation: Operation,
    test: i64,
    yeah: usize,
    nope: usize,
}


fn find_inspects(data: &Vec<Monkey>, rounds: i32, divide: bool) -> BinaryHeap<i64> {
    let mut monkeys: Vec<VecDeque<i64>> = data.iter().map(|m| m.items.iter().copied().collect()).collect();
    let mut inspections: HashMap<usize, i64> = HashMap::new();
    let all_tests = data.iter().fold(1, |acc, next| acc * next.test);

    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            while let Some(old) = monkeys[i].pop_front() {
                *inspections.entry(i).or_insert(0) += 1;
                let worry = match data[i].operation {
                    Operation::OldTimesOld => old * old,
                    Operation::OldPlus(num) => old + num,
                    Operation::OldTimes(num) => old * num,
                };
                let worry = if divide { worry / 3 } else { worry };
                let worry = worry % all_tests;
                match worry % data[i].test == 0 {
                    true => monkeys[data[i].yeah].push_back(worry),
                    false => monkeys[data[i].nope].push_back(worry),
                }
            }
        }
    };

    BinaryHeap::from_iter(inspections.iter().map(|(_, &b)| b))
}

fn solve1(data: &Vec<Monkey>) -> i64 {
    find_inspects(&data, 20, true)
        .into_sorted_vec().iter().rev().take(2).product()
}

fn solve2(data: &Vec<Monkey>) -> i64 {
    find_inspects(&data, 10000, false)
        .into_sorted_vec().iter().rev().take(2).product()
}


fn get_next_number<'a, 'b>(re: &Regex, chunk: &'a mut impl Iterator<Item=&'b str>) -> i64 {
    re.find(chunk.next().unwrap()).unwrap()
        .as_str()
        .parse().unwrap()
}

fn get_op(op1: &Regex, op2: &Regex, op3: &Regex, s: &str) -> Operation {
    if op1.is_match(s) {
        Operation::OldTimesOld
    }
    else if let Some(plus) = op2.captures(s) {
        Operation::OldPlus(plus.get(1).unwrap().as_str().parse().unwrap())
    }
    else if let Some(times) = op3.captures(s) {
        Operation::OldTimes(times.get(1).unwrap().as_str().parse().unwrap())
    }
    else {
        panic!()
    }
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Monkey> {
    let number = Regex::new(r#"(\d+)"#).unwrap();
    let op1 = Regex::new(r#"old \* old"#).unwrap();
    let op2 = Regex::new(r#"old \+ (\d+)"#).unwrap();
    let op3 = Regex::new(r#"old \* (\d+)"#).unwrap();
    data.chunks(7)
        .map(|chunk| {
            let mut chunk = chunk.into_iter().map(|s| s.as_ref());
            Monkey {
                items: number.find_iter(chunk.nth(1).unwrap())
                    .map(|n| n.as_str().parse().unwrap())
                    .collect(),
                operation: get_op(&op1, &op2, &op3, chunk.next().unwrap()),
                test: get_next_number(&number, &mut chunk),
                yeah: get_next_number(&number, &mut chunk) as usize,
                nope: get_next_number(&number, &mut chunk) as usize,
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str; 27] = &[
        "Monkey 0:",
        "  Starting items: 79, 98",
        "  Operation: new = old * 19",
        "  Test: divisible by 23",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 3",
        "",
        "Monkey 1:",
        "  Starting items: 54, 65, 75, 74",
        "  Operation: new = old + 6",
        "  Test: divisible by 19",
        "    If true: throw to monkey 2",
        "    If false: throw to monkey 0",
        "",
        "Monkey 2:",
        "  Starting items: 79, 60, 97",
        "  Operation: new = old * old",
        "  Test: divisible by 13",
        "    If true: throw to monkey 1",
        "    If false: throw to monkey 3",
        "",
        "Monkey 3:",
        "  Starting items: 74",
        "  Operation: new = old + 3",
        "  Test: divisible by 17",
        "    If true: throw to monkey 0",
        "    If false: throw to monkey 1",
    ];

    #[test]
    fn part1() {
        assert_eq!(10605, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2713310158, solve2(&parse_data(DATA)));
    }
}
