use std::collections::{HashMap, HashSet};
use crate::utils;


pub fn run() -> () {
    let lines = utils::read_lines(utils::Source::Day(-1));
    let data = parse_data(&lines);

    println!("Day 16");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


struct Valve<'data> {
    name: &'data str,
    rate: u32,
    next: Vec<&'data str>
}


fn build_map<'data, 'a>(valves: &'a [Valve<'data>]) -> HashMap<&'data str, &'a Valve<'data>> {
    valves.iter().map(|valve| (valve.name, valve)).collect()
}

fn released_pressure<'data, 'a>(map: &'a HashMap<&'data str, Valve>, keys: &'a [&'data str]) -> u32 {
    keys.iter()
        .map(|&key| map.get(key).unwrap().rate)
        .sum()
}

fn valve_move<'data, 'a>(
    map: &'a HashMap<&'data str, &'a Valve<'data>>,
    open: HashSet<&'data str>,
    next: &'data str,
    time_left: u8,
    released: u32,
) -> i32 {
    todo!()
}

fn valve_open<'data, 'a>(
    valves: HashMap<&'data str, &'a Valve<'data>>,
    open: HashSet<&'data str>,
    next: &'data str,
    time_left: u8,
    released: u32,
) -> i32 {
    todo!()
}

fn solve1(data: &[Valve]) -> u32 {
    let map = build_map(data);

    valve_move(&map, HashSet::new(), "AA", 30, 0);
    println!("hi");
    1
}

fn solve2(data: &[Valve]) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Valve> {
    data.iter()
        .map(|line| {
            let mut line = line.as_ref().split(" ");;
            let name = line.nth(1).unwrap();
            let rate = line.nth(2).unwrap()
                .rsplit("=")
                .next().unwrap()
                .split(";")
                .next().unwrap()
                .parse().unwrap();
            let next = line.skip(4)
                .map(|str| str.split(",").next().unwrap())
                .collect::<Vec<_>>();
            Valve { name, rate, next }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
        "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
        "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
        "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
        "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
        "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
        "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
        "Valve HH has flow rate=22; tunnel leads to valve GG",
        "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
        "Valve JJ has flow rate=21; tunnel leads to valve II"
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(1, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
