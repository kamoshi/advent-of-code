#![feature(test)]

mod day01;
mod day02;
mod day03;
mod day06;
mod day10;

use std::time::Instant;

// const I: &str = include_str!("../../.data/06");

fn main() {
    let start = Instant::now();

    // let res = day06::solve(I);

    let duration = start.elapsed();

    // println!("Result: {:?}", res);
    println!("Time elapsed: {:?}", duration);
}
