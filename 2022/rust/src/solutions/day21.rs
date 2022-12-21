#![allow(dead_code)]
use std::collections::HashMap;
use crate::utils;


pub fn run() -> () {
    let data = utils::read_lines(utils::Source::Day(21));
    let data = parse_data(&data);

    println!("Day 21");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Copy, Clone)]
enum Op {
    Add, Sub, Mul, Div
}

impl Op {
    fn apply(&self, a: i64, b: i64) -> i64 {
        match self {
            Op::Add => a + b,
            Op::Sub => a - b,
            Op::Mul => a * b,
            Op::Div => a / b,
        }
    }
}

#[derive(Copy, Clone)]
enum Shout<'data> {
    Number(i64),
    LazyOp(&'data str, Op, &'data str)
}

struct Monkey<'data> {
    name: &'data str,
    shout: Shout<'data>
}


fn chain_eval<'data, 'a>(
    start: &'data str,
    dependees: &'a HashMap<&'data str, Vec<&'data str>>,
    awaiting: &'a mut HashMap<&'data str, Shout>,
    finished: &'a mut HashMap<&'data str, i64>,
) {
    for &dependee in dependees.get(start).unwrap_or(&vec![]) {
        if let Some(&Shout::LazyOp(l, op, r)) = awaiting.get(dependee) {
            match (finished.contains_key(l), finished.contains_key(r)) {
                (true, true) => {
                    finished.insert(dependee, op.apply(finished[l], finished[r]));
                    awaiting.remove(dependee);
                    chain_eval(dependee, dependees, awaiting, finished);
                }
                _ => {},
            };
        }
    }
}


fn solve1(data: &[Monkey]) -> i64 {
    let mut finished = HashMap::new();
    let mut awaiting = HashMap::new();
    let mut dependees = HashMap::new();

    for monkey in data {
        match monkey.shout {
            Shout::Number(n) => {
                finished.insert(monkey.name, n);
                chain_eval(monkey.name, &dependees, &mut awaiting, &mut finished);
            },
            Shout::LazyOp(l, op, r) =>
                match (finished.contains_key(l), finished.contains_key(r)) {
                    (true, true) => {
                        finished.insert(monkey.name, op.apply(finished[l], finished[r]));
                        chain_eval(monkey.name, &dependees, &mut awaiting, &mut finished);
                    }
                    _ => {
                        awaiting.insert(monkey.name, monkey.shout);
                        dependees.entry(l).or_insert(vec![]).push(monkey.name);
                        dependees.entry(r).or_insert(vec![]).push(monkey.name);
                    }
                },
        };
    }

    finished["root"]
}

fn go_back<'data, 'a>(
    finished: &'a HashMap<&'data str, i64>,
    awaiting: &'a HashMap<&'data str, Shout<'data>>,
    blocker: &'data str,
    required: i64,
) -> i64 {
    if blocker == "humn" { return required };
    match awaiting[blocker] {
        Shout::LazyOp(l, op, r) => {
            match (finished.get(l), finished.get(r)) {
                (Some(&l), None) => {
                    let next = match op {
                        Op::Add => required - l,
                        Op::Sub => l - required,
                        Op::Mul => required / l,
                        Op::Div => l / required,
                    };
                    go_back(finished, awaiting, r, next)
                },
                (None, Some(&r)) => {
                    let next = match op {
                        Op::Add => required - r,
                        Op::Sub => required + r,
                        Op::Mul => required / r,
                        Op::Div => required * r,
                    };
                    go_back(finished, awaiting, l, next)
                },
                _ => unreachable!(),
            }
        },
        _ => unreachable!(),
    }
}

fn solve2(data: &[Monkey]) -> i64 {
    let mut finished = HashMap::new();
    let mut awaiting = HashMap::new();
    let mut dependees = HashMap::new();

    for monkey in data {
        if monkey.name == "humn" { continue };
        match monkey.shout {
            Shout::Number(n) => {
                finished.insert(monkey.name, n);
                chain_eval(monkey.name, &dependees, &mut awaiting, &mut finished);
            },
            Shout::LazyOp(l, op, r) =>
                match (finished.contains_key(l), finished.contains_key(r)) {
                    (true, true) => {
                        finished.insert(monkey.name, op.apply(finished[l], finished[r]));
                        chain_eval(monkey.name, &dependees, &mut awaiting, &mut finished);
                    }
                    _ => {
                        awaiting.insert(monkey.name, monkey.shout);
                        dependees.entry(l).or_insert(vec![]).push(monkey.name);
                        dependees.entry(r).or_insert(vec![]).push(monkey.name);
                    }
                },
        };
    }

    match awaiting["root"] {
        Shout::LazyOp(l, _, r)=> {
            match (finished.get(l), finished.get(r)) {
                (Some(l), None) => go_back(&finished, &awaiting, r, *l),
                (None, Some(r)) => go_back(&finished, &awaiting, l, *r),
                _ => unreachable!(),
            }
        },
        _ => unreachable!(),
    }
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Monkey> {
    data.iter()
        .map(|line| {
            let (name, rest) = line.as_ref().split_once(": ").unwrap();
            let shout = match rest.chars().nth(5) {
                None => Shout::Number(rest.parse().unwrap()),
                Some(op) => {
                    let mut operands = rest.split(" ");
                    let op = match op {
                        '+' => Op::Add,
                        '-' => Op::Sub,
                        '*' => Op::Mul,
                        '/' => Op::Div,
                        _ => unreachable!(),
                    };
                    Shout::LazyOp(operands.next().unwrap(), op, operands.nth(1).unwrap())
                }
            };
            Monkey {
                name, shout
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "root: pppw + sjmn",
        "dbpl: 5",
        "cczh: sllz + lgvd",
        "zczc: 2",
        "ptdq: humn - dvpt",
        "dvpt: 3",
        "lfqf: 4",
        "humn: 5",
        "ljgn: 2",
        "sjmn: drzm * dbpl",
        "sllz: 4",
        "pppw: cczh / lfqf",
        "lgvd: ljgn * ptdq",
        "drzm: hmdt - zczc",
        "hmdt: 32",
    ];

    #[test]
    fn part1() {
        assert_eq!(152, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(301, solve2(&parse_data(DATA)));
    }
}
