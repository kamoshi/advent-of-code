#![feature(test)]

use std::time::Instant;
mod day01;

const I1: &str = include_str!("../input");

fn main() {
    let start = Instant::now();

    let res = day01::solve(I1);

    let duration = start.elapsed();

    println!("Result: {:?}", res);
    println!("Time elapsed: {:?}", duration);
}
