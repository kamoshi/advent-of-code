use std::collections::HashMap;
use regex::Regex;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(22)));

    println!("Day 22");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


#[derive(Copy, Clone)]
enum Tile {
    Empty, Wall
}

#[derive(Copy, Clone)]
enum Action {
    Move(usize), L, R
}

impl Action {
    fn apply(&self, facing: &Facing) -> Facing {
        match (self, facing) {
            (Action::L, Facing::R) => Facing::U,
            (Action::L, Facing::D) => Facing::R,
            (Action::L, Facing::L) => Facing::D,
            (Action::L, Facing::U) => Facing::L,
            (Action::R, Facing::R) => Facing::D,
            (Action::R, Facing::D) => Facing::L,
            (Action::R, Facing::L) => Facing::U,
            (Action::R, Facing::U) => Facing::R,
            _ => panic!(),
        }
    }
}

#[derive(Copy, Clone)]
#[repr(usize)]
enum Facing {
    R = 0,
    D = 1,
    L = 2,
    U = 3,
}

type Point = (usize, usize);
type Map = HashMap<Point, Tile>;
type Commands = Vec<Action>;

trait NextPosProvider {
    fn get_next_pos(&self, pos: Point, dir: Facing) -> Point;
}

struct WrappingPosProvider {
    row_bounds: HashMap<usize, (usize, usize)>,
    col_bounds: HashMap<usize, (usize, usize)>,
}

impl WrappingPosProvider {
    fn new(map: &Map) -> Self {
        let (row_bounds, col_bounds) = map.keys()
            .fold((HashMap::<usize, (usize, usize)>::new(), HashMap::<usize, (usize, usize)>::new()),
                  |(mut rows, mut cols), &(r, c)| {
                      rows.entry(r).and_modify(|x| *x = (x.0.min(c), x.1.max(c))).or_insert((c, c));
                      cols.entry(c).and_modify(|x| *x = (x.0.min(r), x.1.max(r))).or_insert((r, r));
                      (rows, cols)
                  });
        Self { row_bounds, col_bounds }
    }
}

impl NextPosProvider for WrappingPosProvider {
    fn get_next_pos(&self, pos: Point, dir: Facing) -> Point {
        let (row, col) = pos;
        match dir {
            Facing::R => {
                let (min, max) = self.row_bounds[&row];
                match col < max {
                    true => (row, col + 1),
                    false => (row, min),
                }
            },
            Facing::D => {
                let (min, max) = self.col_bounds[&col];
                match row < max {
                    true => (row + 1, col),
                    false => (min, col),
                }
            },
            Facing::L => {
                let (min, max) = self.row_bounds[&row];
                match min < col {
                    true => (row, col - 1),
                    false => (row, max),
                }
            },
            Facing::U => {
                let (min, max) = self.col_bounds[&col];
                match min < row {
                    true => (row - 1, col),
                    false => (max, col),
                }
            },
        }
    }
}

struct Dungeon<'a> {
    map: &'a Map,
    pos: Point,
    dir: Facing,
    npp: Box<dyn NextPosProvider>,
}

impl<'a> Dungeon<'a> {
    fn new<'data>(map: &'data Map, pos: Point, dir: Facing, npp: Box<dyn NextPosProvider>) -> Self where 'data: 'a {
        Self { map, pos, dir, npp }
    }

    fn act(&mut self, action: &Action) {
        match action {
            Action::Move(steps) => self.walk(*steps),
            rotate => self.dir = rotate.apply(&self.dir),
        }
    }

    fn walk(&mut self, steps: usize) {
        for _ in 0..steps {
            let next_pos = self.npp.get_next_pos(self.pos, self.dir);
            match self.map[&next_pos] {
                Tile::Wall => continue,
                Tile::Empty => self.pos = pos,
            }
        }
    }

    fn get_password(&self) -> usize {
        1000 * (self.pos.0 + 1) + 4 * (self.pos.1 + 1) + self.dir as usize
    }
}


fn solve1((start, map, commands): &(Point, Map, Commands)) -> usize {
    let mut dungeon = Dungeon::new(map, *start, Facing::R, Box::new(WrappingPosProvider::new(&map)));
    for action in commands {
        dungeon.act(action)
    };
    dungeon.get_password()
}

fn solve2(data: &(Point, Map, Commands)) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> (Point, Map, Commands) {
    let start = data.iter().next()
        .map(|first_row|
            first_row.as_ref().char_indices()
                .skip_while(|&(_, c)| c == ' ')
                .next()
                .map(|(col, _)| (0_usize, col)).unwrap()
        ).unwrap();
    let map = data.iter()
        .take_while(|line| !line.as_ref().is_empty())
        .enumerate()
        .fold(HashMap::new(), |mut acc, (row, line)| {
            for (col, char) in line.as_ref().char_indices() {
                match char {
                    ' ' => continue,
                    '#' => acc.insert((row, col), Tile::Wall),
                    '.' => acc.insert((row, col), Tile::Empty),
                    _ => unreachable!(),
                };
            }
            acc
        });
    let re = Regex::new(r#"(R)|(L)|(\d*)"#).unwrap();
    let commands = data.iter().skip_while(|line| !line.as_ref().is_empty()).nth(1).unwrap().as_ref();
    let commands = re.find_iter(commands)
        .map(|cap| match cap.as_str() {
            "R" => Action::R,
            "L" => Action::L,
            _ => Action::Move(cap.as_str().parse().unwrap()),
        })
        .collect();
    (start, map, commands)
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "        ...#",
        "        .#..",
        "        #...",
        "        ....",
        "...#.......#",
        "........#...",
        "..#....#....",
        "..........#.",
        "        ...#....",
        "        .....#..",
        "        .#......",
        "        ......#.",
        "",
        "10R5L5R10L4R5L5",
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(6032, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
