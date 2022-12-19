use regex::{CaptureMatches, Captures, Regex, SubCaptureMatches};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(-1)));

    println!("Day X");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

#[derive(Debug)] struct Ore(i32);
#[derive(Debug)] struct Clay(i32);
#[derive(Debug)] struct Obsidian(i32);

#[derive(Debug)]
struct Blueprint {
    id: usize,
    ore: Ore,
    clay: Ore,
    obsidian: (Ore, Clay),
    geode: (Ore, Obsidian),
}


fn solve1(data: &[Blueprint]) -> i32 {
    println!("{:?}", data);
    1
}

fn solve2(data: &[Blueprint]) -> i32 {
    2
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
            let mut cap = re.captures(line.as_ref()).unwrap();
            Blueprint {
                id: extract(&cap, 1) as usize,
                ore: Ore(extract(&cap, 2)),
                clay: Ore(extract(&cap, 3)),
                obsidian: (Ore(extract(&cap, 4)), Clay(extract(&cap, 5))),
                geode: (Ore(extract(&cap, 6)), Obsidian(extract(&cap, 7))),
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
        assert_eq!(1, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(2, solve2(&parse_data(DATA)));
    }
}
