#![allow(dead_code)]
use std::collections::HashSet;
use crate::utils;
use crate::utils::matrix::Matrix;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(10)));

    println!("Day 10");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: \n{}", solve2(&data));
}


#[derive(Copy, Clone)]
enum Instruction {
    Noop,
    Addx(i32)
}

struct Cpu {
    cycle: i32,
    register: i32,
    pipeline: [Option<Instruction>; 2],
}

impl Cpu {
    fn new() -> Self {
        Cpu { cycle: 1, register: 1, pipeline: [None; 2] }
    }

    fn pipe(&mut self) {
        for idx in (1..self.pipeline.len()).rev() {
            self.pipeline[idx] = self.pipeline[idx-1];
        };
        self.pipeline[0] = None;
    }

    fn exec_now(&mut self) {
        match self.pipeline[0] {
            Some(Instruction::Noop) => self.pipeline[0] = None,
            _ => {}
        }
        match self.pipeline[1] {
            Some(Instruction::Addx(param)) => {
                self.register += param;
                self.pipeline[1] = None;
            }
            _ => {}
        }
        self.cycle += 1;
    }

    fn is_ready(&self) -> bool {
        self.pipeline.iter().all(|pipe| pipe.is_none())
    }

    fn push(&mut self, instruction: Instruction) {
        match self.is_ready() {
            true => self.pipeline[0] = Some(instruction),
            false => panic!("CPU is not ready"),
        }
    }

    fn get_signal(&self) -> (i32, i32) {
        (self.cycle, self.register)
    }
}

fn get_signal(data: &[Instruction]) -> Vec<(i32, i32)> {
    let mut signal = vec![(1, 1)];
    let mut cpu = Cpu::new();
    for &instruction in data {
        cpu.push(instruction);
        while !cpu.is_ready() {
            cpu.exec_now();
            cpu.pipe();
            signal.push(cpu.get_signal());
        }
    }
    signal
}


fn solve1(data: &[Instruction]) -> i32 {
    let filter = HashSet::from([20, 60, 100, 140, 180, 220]);
    get_signal(data).into_iter()
        .filter(|(cycle, _)| filter.contains(cycle))
        .map(|(cycle, register)| cycle * register)
        .sum()
}

fn solve2(data: &[Instruction]) -> String {
    let mut crt = Matrix::with_shape((6, 40), '.');

    for (cycle, register) in get_signal(&data) {
        let row = (cycle - 1) / 40;
        let col = (cycle - 1) % 40;
        if register - 1 == col || register == col || register + 1 == col {
            crt[(row as usize, col as usize)] = '#';
        }
    }

    crt.iter_rows()
        .map(String::from_iter)
        .collect::<Vec<_>>()
        .join("\n")
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<Instruction> {
    data.iter()
        .map(|s| {
            let s = s.as_ref();
            let mut parts = s.split_whitespace();
            match parts.next().unwrap() {
                "noop" => Instruction::Noop,
                "addx" => Instruction::Addx(parts.next().unwrap().parse().unwrap()),
                _ => unreachable!()
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str; 146] = &[
        "addx 15", "addx -11", "addx 6", "addx -3", "addx 5", "addx -1", "addx -8", "addx 13",
        "addx 4", "noop", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1", "addx 5", "addx -1",
        "addx 5", "addx -1", "addx -35", "addx 1", "addx 24", "addx -19", "addx 1", "addx 16",
        "addx -11", "noop", "noop", "addx 21", "addx -15", "noop", "noop", "addx -3", "addx 9",
        "addx 1", "addx -3", "addx 8", "addx 1", "addx 5", "noop", "noop", "noop", "noop", "noop",
        "addx -36", "noop", "addx 1", "addx 7", "noop", "noop", "noop", "addx 2", "addx 6", "noop",
        "noop", "noop", "noop", "noop", "addx 1", "noop", "noop", "addx 7", "addx 1", "noop",
        "addx -13", "addx 13", "addx 7", "noop", "addx 1", "addx -33", "noop", "noop", "noop",
        "addx 2", "noop", "noop", "noop", "addx 8", "noop", "addx -1", "addx 2", "addx 1", "noop",
        "addx 17", "addx -9", "addx 1", "addx 1", "addx -3", "addx 11", "noop", "noop", "addx 1",
        "noop", "addx 1", "noop", "noop", "addx -13", "addx -19", "addx 1", "addx 3", "addx 26",
        "addx -30", "addx 12", "addx -1", "addx 3", "addx 1", "noop", "noop", "noop", "addx -9",
        "addx 18", "addx 1", "addx 2", "noop", "noop", "addx 9", "noop", "noop", "noop", "addx -1",
        "addx 2", "addx -37", "addx 1", "addx 3", "noop", "addx 15", "addx -21", "addx 22",
        "addx -6", "addx 1", "noop", "addx 2", "addx 1", "noop", "addx -10", "noop", "noop",
        "addx 20", "addx 1", "addx 2", "addx 2", "addx -6", "addx -11", "noop", "noop", "noop"
    ];

    #[test]
    fn part1() {
        assert_eq!(13140, solve1(&parse_data(DATA)));
    }

    // #[test]
    // fn part2() {
    //     println!("{}", solve2(&parse_data(DATA)));
    // }
}
