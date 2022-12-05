use regex::Regex;
use crate::utils;

type Data = (Vec<Vec<char>>, Vec<(i32, i32, i32)>);

pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Scratch));

    println!("Day 5");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &Data) -> i32 {
    1
}

fn solve2(data: &Data) -> i32 {
    2
}


fn parse_data(data: Vec<String>) -> Data {
    let re = Regex::new("( {3}|[\\[\\w\\]]{3})").unwrap();
    let iter = data.iter();
    let mut boxes = iter
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
    let stacks = boxes.next()
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
        .unwrap();

    let re = Regex::new("^move (\\d+) from (\\d+) to (\\d+)$").unwrap();
    let actions = data.iter()
        .filter_map(|str| re.captures(str))
        .map(|cap| (
            cap.get(1).unwrap().as_str().parse().unwrap(),
            cap.get(2).unwrap().as_str().parse().unwrap(),
            cap.get(3).unwrap().as_str().parse().unwrap(),
        ))
        .collect();

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
        assert_eq!(1, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data());
        assert_eq!(2, solve2(&data));
    }
}
