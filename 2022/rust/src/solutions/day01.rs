use crate::utils;

pub fn run() -> () {
    let data = utils::read_lines(utils::Source::Scratch);

    println!("Day 1");
    for x in data {
        println!("{}", x);
    }
}
