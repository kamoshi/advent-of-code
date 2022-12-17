#![allow(dead_code)]
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


struct Valve {
    name: usize,
    rate: u32,
    next: Vec<usize>,
}

// start, valves, distances
type Data = (usize, Vec<Valve>, HashMap<(usize, usize), u32>);


#[derive(Eq, PartialEq)]
struct State {
    name: usize,
    cost: u32,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd<Self> for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn find_distance(valves: &[Valve], start: usize, goal: usize) -> u32 {
    let mut frontier: BinaryHeap<State> = BinaryHeap::new();
    let mut costs: HashMap<usize, u32> = HashMap::from([(start, 0)]);

    frontier.push(State { name: start, cost: 0 });
    while let Some(State { name: current, .. }) = frontier.pop() {
        if current == goal { break };

        for &neighbour in valves[current].next.iter() {
            let cost = costs.get(&current).unwrap() + 1;

            if !costs.contains_key(&neighbour) || cost < *costs.get(&neighbour).unwrap() {
                costs.insert(neighbour, cost);
                frontier.push(State { cost, name: neighbour });
            }
        }
    }
    *costs.get(&goal).unwrap()
}

fn find_distances(valves: &[Valve]) -> HashMap<(usize, usize), u32> {
    valves.iter()
        .flat_map(|start|
            valves.iter().map(|goal| ((start.name, goal.name), find_distance(valves, start.name, goal.name)))
        )
        .collect()
}

fn closed_valves(valves: &[Valve]) -> HashSet<usize> {
    valves.iter()
        .filter(|valve| valve.rate != 0)
        .map(|valve| valve.name)
        .collect()
}

fn set_to_bitmap(set: &HashSet<usize>) -> u64 {
    let mut map = 0;
    for &index in set {
        map |= 1 << index
    }
    map
}

struct MoveState<'a> {
    current: usize,
    time_left: u32,
    closed: &'a HashSet<usize>,
}

impl<'a> MoveState<'a> {
    fn as_cache_key(&self) -> (usize, u32, u64) {
        (self.current, self.time_left, set_to_bitmap(self.closed))
    }
}

fn move_to_open(
    valves: &[Valve],
    distances: &HashMap<(usize, usize), u32>,
    state: MoveState,
    cache: &mut HashMap<(usize, u32, u64), u32>
) -> u32 {
    if state.time_left < 1 { return 0 };
    let cached = cache.get(&state.as_cache_key());
    if cached.is_some() { return *cached.unwrap() }

    let closed = { let mut closed = state.closed.clone(); closed.remove(&state.current); closed };
    let time_left = state.time_left - 1;
    let released = valves[state.current].rate * time_left;

    let max_next = closed.iter()
        .filter_map(|&current| {
            let distance = distances[&(state.current, current)];
            match time_left > distance + 1 {
                true => Some(move_to_open(valves, distances, MoveState {
                    current,
                    time_left: time_left - distance,
                    closed: &closed
                }, cache)),
                false => None,
            }
        })
        .max()
        .unwrap_or_default();
    let result = released + max_next;
    cache.insert(state.as_cache_key(), result);
    result
}

fn solve1((start, valves, distances): &Data) -> u32 {
    move_to_open(&valves, distances, MoveState {
        current: *start,
        time_left: 31,
        closed: &closed_valves(valves)
    }, &mut HashMap::new())
}

fn bitmap_to_set(valves: &[Valve], bitmap: u16) -> HashSet<usize> {
    let mut set = HashSet::new();
    for valve in valves {
        if bitmap & (1 << valve.name) != 0 { set.insert(valve.name); }
    }
    set
}

fn solve2((start, valves, distances): &Data) -> u32 {
    let mask = valves.iter().fold(0_u16, |acc, v| acc | 1 << v.name);

    let mut top_score: u32 = 0;
    let mut cache = HashMap::new();
    for bitmap in 1..u16::MAX {
        let res_1 = move_to_open(&valves, &distances, MoveState {
            current: *start,
            time_left: 27,
            closed: &bitmap_to_set(valves, bitmap),
        }, &mut cache);
        let res_2 = move_to_open(&valves, &distances, MoveState {
            current: *start,
            time_left: 27,
            closed: &bitmap_to_set(valves, bitmap ^ mask),
        }, &mut cache);

        top_score = top_score.max(res_1 + res_2)
    }

    top_score
}


fn compress(data: &[Valve], preserve: usize) -> (HashMap<usize, usize>, Vec<Valve>) {
    let (translation, filtered) = data.iter()
        .filter(|valve| valve.rate > 0 || valve.name == preserve )
        .enumerate()
        .fold((HashMap::new(), vec![]), |(mut map, mut valves), (new_idx, valve)| {
            map.insert(valve.name, new_idx);
            valves.push(valve);
            (map, valves)
        });
    let valves = filtered.into_iter()
        .enumerate()
        .map(|(name, old)| Valve {
            name, rate: old.rate, next: vec![],
        })
        .collect::<Vec<_>>();
    (translation, valves)
}

fn translate(map: HashMap<&str, usize>, data: Vec<Valve>) -> (usize, Vec<Valve>, HashMap<(usize, usize), u32>) {
    let start = *map.get("AA").unwrap();
    let distances = find_distances(&data);
    let (tl, valves) = compress(&data, start);
    let distances = distances.into_iter()
        .filter_map(|((a, b), dist)| match tl.contains_key(&a) && tl.contains_key(&b) {
            true => Some(((tl[&a], tl[&b]), dist)),
            false => None,
        })
        .collect::<HashMap<_, _>>();

    (tl[&start], valves, distances)
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> (usize, Vec<Valve>, HashMap<(usize, usize), u32>) {
    let valves = data.iter()
        .enumerate()
        .map(|(index, line)| {
            let mut line = line.as_ref().split(" ");
            let name = line.nth(1).unwrap();
            let rate: u32 = line.nth(2).unwrap()
                .rsplit("=")
                .next().unwrap()
                .split(";")
                .next().unwrap()
                .parse().unwrap();
            let next = line.skip(4)
                .map(|str| str.split(",").next().unwrap())
                .collect::<Vec<_>>();
            (index, name, rate, next)
        })
        .collect::<Vec<_>>();
    let map = valves.iter().map(|&(index, name, _, _)| (name, index)).collect::<HashMap<_, _>>();
    let valves = valves.into_iter()
        .map(|(name, _, rate, next)| Valve {
            name, rate, next: next.iter().map(|&next| *map.get(next).unwrap()).collect(),
        })
        .collect();

    translate(map, valves)
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
        assert_eq!(1707, solve2(&parse_data(DATA)));
    }
}
