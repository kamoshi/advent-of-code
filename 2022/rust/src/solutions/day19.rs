#![allow(dead_code)]
use std::collections::{HashMap, HashSet};
use regex::{Captures, Regex};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(19)));

    println!("Day 19");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

type Ore = i32;
type Clay = i32;
type Obsidian = i32;
type Geode = i32;


#[derive(Debug)]
struct Blueprint {
    id: i32,
    ore: Ore,
    clay: Ore,
    obsidian: (Ore, Clay),
    geode: (Ore, Obsidian),
}

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Debug)]
struct State {
    time_left: i32,
    // Bot count
    bot_ore: i32,
    bot_cla: i32,
    bot_obs: i32,
    bot_geo: i32,
    // Inventory count
    inv_ore: Ore,
    inv_cla: Clay,
    inv_obs: Obsidian,
    inv_geo: Geode,
}


fn explore(blueprint: &Blueprint, start: State) -> i32 {
    let mut optimal_geo: HashMap<i32, Geode> = HashMap::new();
    let mut visited = HashSet::new();
    let mut pending: Vec<State> = Vec::new();
    pending.push(start);

    let max_req_ore = blueprint.geode.0.max(blueprint.obsidian.0).max(blueprint.clay).max(blueprint.ore);
    let max_req_cla = blueprint.obsidian.1;
    let max_req_obs = blueprint.geode.1;

    let mut max_geodes = 0;
    while let Some(state) = pending.pop() {
        match visited.contains(&state) {
            true => continue,
            false => visited.insert(state),
        };
        if state.time_left == 0 {
            max_geodes = max_geodes.max(state.inv_geo);
            continue
        };
        match state.inv_geo + 2 < *optimal_geo.entry(state.time_left).or_default() {
            true => continue,
            false => optimal_geo.entry(state.time_left).and_modify(|v| *v = state.inv_geo.max(*v))
        };

        // geode
        if state.inv_ore >= blueprint.geode.0 && state.inv_obs >= blueprint.geode.1 {
            pending.push(State {
                time_left: state.time_left - 1,
                bot_geo: state.bot_geo + 1,
                inv_ore: state.inv_ore + state.bot_ore - blueprint.geode.0,
                inv_cla: state.inv_cla + state.bot_cla,
                inv_obs: state.inv_obs + state.bot_obs - blueprint.geode.1,
                inv_geo: state.inv_geo + state.bot_geo,
                ..state
            });
            continue
        }
        // obsidian
        if state.inv_ore >= blueprint.obsidian.0 && state.inv_cla >= blueprint.obsidian.1 && state.bot_obs <= max_req_obs {
            pending.push(State {
                time_left: state.time_left - 1,
                bot_obs: state.bot_obs + 1,
                inv_ore: state.inv_ore + state.bot_ore - blueprint.obsidian.0,
                inv_cla: state.inv_cla + state.bot_cla - blueprint.obsidian.1,
                inv_obs: state.inv_obs + state.bot_obs,
                inv_geo: state.inv_geo + state.bot_geo,
                ..state
            });
        }
        // clay
        if state.inv_ore >= blueprint.clay && state.bot_cla <= max_req_cla {
            pending.push(State {
                time_left: state.time_left - 1,
                bot_cla: state.bot_cla + 1,
                inv_ore: state.inv_ore + state.bot_ore - blueprint.clay,
                inv_cla: state.inv_cla + state.bot_cla,
                inv_obs: state.inv_obs + state.bot_obs,
                inv_geo: state.inv_geo + state.bot_geo,
                ..state
            })
        }
        // ore
        if state.inv_ore >= blueprint.ore && state.bot_ore <= max_req_ore {
            pending.push(State {
                time_left: state.time_left - 1,
                bot_ore: state.bot_ore + 1,
                inv_ore: state.inv_ore + state.bot_ore - blueprint.ore,
                inv_cla: state.inv_cla + state.bot_cla,
                inv_obs: state.inv_obs + state.bot_obs,
                inv_geo: state.inv_geo + state.bot_geo,
                ..state
            });
        }
        // none
        // why `3 / 2` you may ask? I don't know it's just the lowest multiple that still works ¯\_(ツ)_/¯
        if state.inv_ore <= max_req_ore && state.inv_cla <= max_req_cla * 3 / 2 && state.inv_obs <= max_req_obs {
            pending.push(State {
                time_left: state.time_left - 1,
                inv_ore: state.inv_ore + state.bot_ore,
                inv_cla: state.inv_cla + state.bot_cla,
                inv_obs: state.inv_obs + state.bot_obs,
                inv_geo: state.inv_geo + state.bot_geo,
                ..state
            });
        }
    }
    max_geodes
}

fn solve1(data: &[Blueprint]) -> i32 {
    let start_state = State { time_left: 24, bot_ore: 1, ..Default::default() };
    data.iter()
        .map(|blueprint| blueprint.id * explore(blueprint, start_state))
        .sum()
}

fn solve2(data: &[Blueprint]) -> i32 {
    let start_state = State { time_left: 32, bot_ore: 1, ..Default::default() };
    data.iter()
        .take(3)
        .map(|blueprint| explore(blueprint, start_state))
        .product()
}

fn extract(cap: &Captures, at: usize) -> i32 {
    cap.get(at).unwrap().as_str().parse().unwrap()
}

fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Blueprint> {
    let re = Regex::new(
        r#"Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."#
    ).unwrap();
    data.iter()
        .map(|line| {
            let cap = re.captures(line.as_ref()).unwrap();
            Blueprint {
                id: extract(&cap, 1),
                ore: extract(&cap, 2),
                clay: extract(&cap, 3),
                obsidian: (extract(&cap, 4), extract(&cap, 5)),
                geode: (extract(&cap, 6), extract(&cap, 7)),
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
        "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.",
    ];

    #[test]
    fn part1() {
        assert_eq!(33, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        let s = State { time_left: 32, bot_ore: 1, ..Default::default() };
        assert_eq!(62, parse_data(DATA).iter().take(3).map(|b| explore(b, s)).max().unwrap())
    }
}
