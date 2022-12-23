use std::collections::{HashMap, HashSet, VecDeque};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(23)));

    println!("Day 23");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct Loc {
    row: isize,
    col: isize
}

impl Loc {
    fn n(&self) -> Self { Self { row: self.row - 1, col: self.col } }
    fn s(&self) -> Self { Self { row: self.row + 1, col: self.col } }
    fn w(&self) -> Self { Self { row: self.row, col: self.col - 1 } }
    fn e(&self) -> Self { Self { row: self.row, col: self.col + 1 } }
    fn nw(&self) -> Self { Self { row: self.row - 1, col: self.col - 1 } }
    fn ne(&self) -> Self { Self { row: self.row - 1, col: self.col + 1 } }
    fn sw(&self) -> Self { Self { row: self.row + 1, col: self.col - 1 } }
    fn se(&self) -> Self { Self { row: self.row + 1, col: self.col + 1 } }
}

fn has_neighbors(occupied: &HashSet<Loc>, loc: Loc) -> bool {
    [loc.n(), loc.s(), loc.w(), loc.e(), loc.nw(), loc.ne(), loc.sw(), loc.se()].into_iter().any(|loc| occupied.contains(&loc))
}

fn move_n(occupied: &HashSet<Loc>, loc: Loc) -> Option<Loc> {
    let nn = !occupied.contains(&loc.n());
    let ne = !occupied.contains(&loc.ne());
    let nw = !occupied.contains(&loc.nw());
    (nn && ne && nw).then_some(loc.n())
}

fn move_s(occupied: &HashSet<Loc>, loc: Loc) -> Option<Loc> {
    let ss = !occupied.contains(&loc.s());
    let se = !occupied.contains(&loc.se());
    let sw = !occupied.contains(&loc.sw());
    (ss && se && sw).then_some(loc.s())
}

fn move_w(occupied: &HashSet<Loc>, loc: Loc) -> Option<Loc> {
    let ww = !occupied.contains(&loc.w());
    let nw = !occupied.contains(&loc.nw());
    let sw = !occupied.contains(&loc.sw());
    (ww && nw && sw).then_some(loc.w())
}

fn move_e(occupied: &HashSet<Loc>, loc: Loc) -> Option<Loc> {
    let ee = !occupied.contains(&loc.e());
    let ne = !occupied.contains(&loc.ne());
    let se = !occupied.contains(&loc.se());
    (ee && ne && se).then_some(loc.e())
}

fn find_bounds(locs: &HashSet<Loc>) -> ((isize, isize), (isize, isize)) {
    locs.iter().fold(((isize::MAX, isize::MIN), (isize::MAX, isize::MIN)), |((r_min, r_max), (c_min, c_max)), loc| {
        ((r_min.min(loc.row), r_max.max(loc.row)), (c_min.min(loc.col), c_max.max(loc.col)))
    })
}

fn draw(elves: &Vec<Loc>) {
    let elves = HashSet::<Loc>::from_iter(elves.iter().copied());
    let ((r_min, r_max), (c_min, c_max)) = find_bounds(&elves);

    for row in r_min..=r_max {
        for col in c_min..=c_max {
            match elves.contains(&Loc { row, col }) {
                true => print!("#"),
                false => print!("."),
            }
        }
        println!();
    }
}

fn count_empty(elves: &Vec<Loc>) -> i32 {
    let elves = HashSet::<Loc>::from_iter(elves.iter().copied());
    let ((r_min, r_max), (c_min, c_max)) = find_bounds(&elves);

    let mut sum = 0;
    for row in r_min..=r_max {
        for col in c_min..=c_max {
            if !elves.contains(&Loc { row, col }) {
                sum += 1;
            }
        }
    }
    sum
}

fn solve1(data: &HashSet<Loc>) -> i32 {
    let mut elves = Vec::from_iter(data.iter().copied());
    let mut moves = VecDeque::from([move_n, move_s, move_w, move_e]);

    for _ in 0..10 {
        let occupied = HashSet::<Loc>::from_iter(elves.iter().copied());
        let mut planned = HashMap::<Loc, Vec<usize>>::new();
        for (index, &loc) in elves.iter().enumerate().filter(|&(_, loc)| has_neighbors(&occupied, *loc)) {
            if let Some(loc) = moves.iter().filter_map(|f| f(&occupied, loc)).next() {
                planned.entry(loc)
                    .and_modify(|v| v.push(index))
                    .or_insert(vec![index]);
            }
        }

        for (loc, planned) in planned.into_iter().filter(|(_, vec)| vec.len() == 1) {
            elves[planned[0]] = loc;
        }

        moves.rotate_left(1);
    }

    count_empty(&elves)
}

fn solve2(data: &HashSet<Loc>) -> i32 {
    let mut elves = Vec::from_iter(data.iter().copied());
    let mut moves = VecDeque::from([move_n, move_s, move_w, move_e]);

    for round in 1..i32::MAX {
        let occupied = HashSet::<Loc>::from_iter(elves.iter().copied());
        let mut planned = HashMap::<Loc, Vec<usize>>::new();
        for (index, &loc) in elves.iter().enumerate().filter(|&(_, loc)| has_neighbors(&occupied, *loc)) {
            if let Some(loc) = moves.iter().filter_map(|f| f(&occupied, loc)).next() {
                planned.entry(loc)
                    .and_modify(|v| v.push(index))
                    .or_insert(vec![index]);
            }
        }

        let changes = planned.into_iter().filter(|(_, vec)| vec.len() == 1).collect::<Vec<_>>();
        if changes.len() == 0 { return round }

        for (loc, planned) in changes {
            elves[planned[0]] = loc;
        }

        moves.rotate_left(1);
    };
    panic!()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> HashSet<Loc> {
    data.iter()
        .enumerate()
        .flat_map(|(row, line)| line.as_ref()
            .char_indices()
            .filter(|&(_, c)| c == '#')
            .map(move |(col, _)| Loc {
                row: row as isize,
                col: col as isize,
            })
        )
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "..............",
        "..............",
        ".......#......",
        ".....###.#....",
        "...#...#.#....",
        "....#...##....",
        "...#.###......",
        "...##.#.##....",
        "....#..#......",
        "..............",
        "..............",
        "..............",
    ];

    #[test]
    fn part1() {
        assert_eq!(110, solve1(&parse_data(DATA)));
    }

    #[test]
    fn part2() {
        assert_eq!(20, solve2(&parse_data(DATA)));
    }
}
