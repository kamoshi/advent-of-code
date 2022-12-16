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


fn closed_valves(data: &[Valve]) -> HashSet<usize> {
    data.iter()
        .filter(|valve| valve.rate != 0)
        .map(|valve| valve.name)
        .collect()
}

fn build_map(valves: &[Valve]) -> HashMap<usize, &Valve> {
    valves.iter().map(|valve| (valve.name, valve)).collect()
}

fn release_pressure(map: &HashMap<usize, &Valve>, keys: &HashSet<usize>) -> u32 {
    keys.iter()
        .map(|&key| map.get(&key).unwrap().rate)
        .sum()
}

fn find_distance(map: &HashMap<usize, &Valve>, start: usize, goal: usize) -> u32 {
    let mut frontier: BinaryHeap<State> = BinaryHeap::new();
    let mut costs: HashMap<usize, u32> = HashMap::from([(start, 0)]);

    frontier.push(State { name: start, cost: 0 });
    while let Some(State { name: current, .. }) = frontier.pop() {
        if current == goal { break };

        for &neighbour in map.get(&current).unwrap().next.iter() {
            let cost = costs.get(&current).unwrap() + 1;

            if !costs.contains_key(&neighbour) || cost < *costs.get(&neighbour).unwrap() {
                costs.insert(neighbour, cost);
                frontier.push(State { cost, name: neighbour });
            }
        }
    }
    *costs.get(&goal).unwrap()
}

fn find_distances(map: &HashMap<usize, &Valve>, data: &[Valve]) -> HashMap<(usize, usize), u32> {
    data.iter()
        .flat_map(|start|
            data.iter().map(|goal| ((start.name, goal.name), find_distance(&map, start.name, goal.name)))
        )
        .collect()
}

struct MoveState {
    curr: usize,
    next: usize,
    time_left: u32,
    released: u32,
}

fn move_to_open(
    map: &HashMap<usize, &Valve>,
    distances: &HashMap<(usize, usize), u32>,
    closed: &HashSet<usize>,
    opened: &HashSet<usize>,
    state: MoveState,
) -> u32 {
    let distance = state.time_left.min(*distances.get(&(state.curr, state.next)).unwrap());
    let released = state.released + release_pressure(map, opened) * distance;
    if distance == state.time_left { return released };
    let curr = state.next;

    let released = released + release_pressure(map, opened);
    let closed = { let mut closed = closed.clone(); closed.remove(&curr); closed };
    let opened = { let mut opened = opened.clone(); opened.insert(curr); opened };
    let time_left = state.time_left - distance - 1;

    closed.iter()
        .map(|&next| move_to_open(map, distances, &closed, &opened, MoveState { curr, next, time_left, released }))
        .max()
        .unwrap_or_else(|| released + release_pressure(map, &opened) * time_left)
}

fn find_max_for_start(data: &[Valve], start: usize, limit: u32) -> u32 {
    let map = build_map(data);
    let start_state = MoveState { curr: start, next: start, time_left: limit + 1, released: 0 };
    move_to_open(&map, &find_distances(&map, data), &closed_valves(data), &HashSet::new(), start_state)
}

fn solve1((map, data): &(HashMap<&str, usize>, Vec<Valve>)) -> u32 {
    let start = *map.get("AA").unwrap();
    find_max_for_start(data, start, 30)
}

// Pray this doesn't blow the stack
struct ParallelMoveState {
    p_curr: usize,
    p_next: usize,
    p_progress: u32,
    e_curr: usize,
    e_next: usize,
    e_progress: u32,
    time_left: u32,
    released: u32,
}

fn parallel_to_open(
    map: &HashMap<usize, &Valve>,
    distances: &HashMap<(usize, usize), u32>,
    closed: &HashSet<usize>,
    opened: &HashSet<usize>,
    state: ParallelMoveState,
) -> u32 {
    let p_distance_left = *distances.get(&(state.p_curr, state.p_next)).unwrap() - state.p_progress;
    let e_distance_left = *distances.get(&(state.e_curr, state.e_next)).unwrap() - state.e_progress;
    let distance = state.time_left.min(p_distance_left).min(e_distance_left);
    let released = state.released + distance * release_pressure(map, opened);
    if distance == state.time_left { return released };

    // opening
    let released = released + release_pressure(map, opened);
    let (closed, opened) = {
        let mut closed = closed.clone();
        let mut opened = opened.clone();
        if distance == p_distance_left { closed.remove(&state.p_next); opened.insert(state.p_next); };
        if distance == e_distance_left { closed.remove(&state.e_next); opened.insert(state.e_next); };
        (closed, opened)
    };
    let time_left = state.time_left - distance - 1;

    match (distance == p_distance_left, distance == e_distance_left) {
        (true, true) => closed.iter()
            .flat_map(|p_next| closed.iter()
                .filter(move |&e_next| e_next != p_next)
                .map(|e_next| parallel_to_open(map, distances, &closed, &opened, ParallelMoveState {
                    p_curr: state.p_next, p_next: *p_next, p_progress: 0,
                    e_curr: state.e_next, e_next: *e_next, e_progress: 0,
                    time_left, released,
                }))
            )
            .max()
            .unwrap_or_else(|| released + release_pressure(map, &opened) * time_left),
        (true, false) => closed.iter()
            .map(|&next| parallel_to_open(map, distances, &closed, &opened, ParallelMoveState {
                p_curr: state.p_next, p_next: next, p_progress: 0,
                e_curr: state.e_curr, e_next: state.e_next, e_progress: state.e_progress + distance + 1,
                time_left, released,
            }))
            .max()
            .unwrap_or_else(|| released + release_pressure(map, &opened) * time_left),
        (false, true) => closed.iter()
            .map(|&next| parallel_to_open(map, distances, &closed, &opened, ParallelMoveState {
                p_curr: state.p_curr, p_next: state.p_next, p_progress: state.p_progress + distance + 1,
                e_curr: state.e_next, e_next: next, e_progress: 0,
                time_left, released,
            }))
            .max()
            .unwrap_or_else(|| released + release_pressure(map, &opened) * time_left),
        _ => unreachable!()
    }
}

fn parallel_max_for_start(data: &[Valve], start: usize, limit: u32) -> u32 {
    let map = build_map(data);
    let start_state = ParallelMoveState {
        p_curr: start, p_next: start, p_progress: 0,
        e_curr: start, e_next: start, e_progress: 0,
        time_left: limit + 1, released: 0
    };
    parallel_to_open(&map, &find_distances(&map, data), &closed_valves(data), &HashSet::new(), start_state)
}

fn solve2((map, data): &(HashMap<&str, usize>, Vec<Valve>)) -> u32 {
    let start = *map.get("AA").unwrap();
    parallel_max_for_start(data, start, 26)
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> (HashMap<&str, usize>, Vec<Valve>) {
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
    (map, valves)
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
