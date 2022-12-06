#![allow(dead_code)]
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(5)));

    println!("Day 5");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


type Data = (Vec<Vec<char>>, Vec<(usize, usize, usize)>);

fn solve1((stacks, actions): &Data) -> String {
    let mut stacks = stacks.clone();

    for &(n, from, to) in actions {
        for _ in 0..n {
            let char = stacks.get_mut(from).unwrap().pop().unwrap();
            stacks.get_mut(to).unwrap().push(char);
        }
    }

    stacks.into_iter().map(|mut stack| stack.pop().unwrap()).collect()
}

fn solve2((stacks, actions): &Data) -> String {
    let mut stacks = stacks.clone();

    for &(n, from, to) in actions {
        (0..n).map(|_| stacks.get_mut(from).unwrap().pop().unwrap())
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .for_each(|char| stacks.get_mut(to).unwrap().push(char));
    }

    stacks.into_iter().map(|mut stack| stack.pop().unwrap()).collect()
}


fn parse_data(data: Vec<String>) -> Data {
    let stacks = {
        let re = Regex::new("( {3}|[\\[\\w\\]]{3}) ?").unwrap();
        let mut boxes = data.iter()
            .map_while(|s| {
                let cap = re.find_iter(s)
                    .map(|x| x.as_str().trim())
                    .enumerate()
                    .filter_map(|(idx, str)| match str.len() {
                        0 => None,
                        _ => Some((idx, str.chars().nth(1).unwrap()))
                    })
                    .collect::<Vec<_>>();
                if cap.is_empty() { None } else { Some(cap) }
            })
            .collect::<Vec<_>>()
            .into_iter()
            .rev();
        boxes.next()
            .map(|bottom| {
                let stacks = bottom.into_iter()
                    .map(|(_, c)| vec![c])
                    .collect::<Vec<_>>();
                boxes.fold(stacks, |mut acc, next| {
                    next.into_iter().for_each(|(i, c)|
                        acc[i].push(c)
                    );
                    acc
                })
            })
            .unwrap()
    };
    let actions = {
        let re = Regex::new("^move (\\d+) from (\\d+) to (\\d+)$").unwrap();
        data.iter()
            .filter_map(|str| re.captures(str))
            .map(|cap| (
                cap.get(1).unwrap().as_str().parse().unwrap(),
                cap.get(2).unwrap().as_str().parse::<usize>().unwrap() - 1,
                cap.get(3).unwrap().as_str().parse::<usize>().unwrap() - 1,
            ))
            .collect()
    };
    (stacks, actions)
}


#[cfg(test)]
mod tests {
    use super::*;

    fn data() -> Vec<String> {
        vec![
            "    [D]    ",
            "[N] [C]    ",
            "[Z] [M] [P]",
            " 1   2   3 ",
            "",
            "move 1 from 2 to 1",
            "move 3 from 1 to 3",
            "move 2 from 2 to 1",
            "move 1 from 1 to 2",
        ]
            .into_iter().map(String::from).collect()
    }

    #[test]
    fn part1() {
        let data = parse_data(data());
        assert_eq!("CMZ", solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data());
        assert_eq!("MCD", solve2(&data));
    }
}
