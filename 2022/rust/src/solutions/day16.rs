use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use crate::utils;


pub fn run() -> () {
    let lines = utils::read_lines(utils::Source::Day(16));
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

#[derive(Eq, PartialEq)]
struct State<'data> {
    name: &'data str,
    cost: u32,
}

impl Ord for State<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd<Self> for State<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


fn closed_valves<'data>(data: &'data [Valve<'data>]) -> HashSet<&'data str> {
    data.iter()
        .filter(|valve| valve.rate != 0)
        .map(|valve| valve.name)
        .collect()
}

fn build_map<'data, 'a>(valves: &'a [Valve<'data>]) -> HashMap<&'data str, &'a Valve<'data>> {
    valves.iter().map(|valve| (valve.name, valve)).collect()
}

fn release_pressure<'data, 'a>(map: &'a HashMap<&'data str, &'a Valve<'data>>, keys: &'a HashSet<&'data str>) -> u32 {
    keys.iter()
        .map(|&key| map.get(key).unwrap().rate)
        .sum()
}

fn find_distance<'data, 'a>(map: &'a HashMap<&'data str, &'a Valve<'data>>, start: &'data str, goal: &'data str, ) -> u32 {
    let mut frontier: BinaryHeap<State> = BinaryHeap::new();
    let mut costs: HashMap<&'data str, u32> = HashMap::from([(start, 0)]);

    frontier.push(State { name: start, cost: 0 });
    while let Some(State { name: current, .. }) = frontier.pop() {
        if current == goal { break };

        for &neighbour in map.get(current).unwrap().next.iter() {
            let cost = costs.get(&current).unwrap() + 1;

            if !costs.contains_key(&neighbour) || cost < *costs.get(&neighbour).unwrap() {
                costs.insert(neighbour, cost);
                frontier.push(State { cost, name: neighbour });
            }
        }
    }
    *costs.get(goal).unwrap()
}

fn find_distances<'data, 'a>(map: &'a HashMap<&'data str, &'a Valve<'data>>, data: &'data [Valve]) -> HashMap<(&'data str, &'data str), u32> {
    data.iter()
        .flat_map(|start|
            data.iter()
                .map(|goal| ((start.name, goal.name), find_distance(&map, start.name, goal.name)))
        )
        .collect::<HashMap<_, _>>()
}

struct MoveState<'data> {
    curr: &'data str,
    next: &'data str,
    time_left: u32,
    released: u32,
}

fn move_to_open<'data, 'a>(
    map: &'a HashMap<&'data str, &'a Valve<'data>>,
    distances: &'a HashMap<(&'data str, &'data str), u32>,
    closed: &'a HashSet<&'data str>,
    opened: &'a HashSet<&'data str>,
    state: MoveState,
) -> u32 {
    let distance = *distances.get(&(state.curr, state.next)).unwrap().min(&state.time_left);
    let released = state.released + release_pressure(map, opened) * distance;
    if distance == state.time_left { return released };
    let curr = state.next;

    let released = released + release_pressure(map, opened);
    let closed = { let mut closed = closed.clone(); closed.remove(curr); closed };
    let opened = { let mut opened = opened.clone(); opened.insert(curr); opened };
    let time_left = state.time_left - distance - 1;

    closed.iter()
        .map(|&next| move_to_open(map, distances, &closed, &opened, MoveState { curr, next, time_left, released }))
        .max()
        .unwrap_or_else(|| released + release_pressure(map, &opened) * time_left)
}

fn find_max_for_start(data: &[Valve], start: &str, limit: u32) -> u32 {
    let map = build_map(data);
    let start_state = MoveState { curr: start, next: start, time_left: limit + 1, released: 0 };
    move_to_open(&map, &find_distances(&map, data), &closed_valves(data), &HashSet::new(), start_state)
}

fn solve1(data: &[Valve]) -> u32 {
    find_max_for_start(data, "AA", 30)
}

struct ParallelMoveState<'data> {
    p_curr: &'data str,
    p_next: &'data str,
    p_progress: u32,
    e_curr: &'data str,
    e_next: &'data str,
    e_progress: u32,
    time_left: u32,
    released: u32,
}

fn solve2(data: &[Valve]) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Valve> {
    data.iter()
        .map(|line| {
            let mut line = line.as_ref().split(" ");
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
        assert_eq!(1651, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
