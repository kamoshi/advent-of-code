use std::fs::read_to_string;
use std::path::Path;

use advent::Day;

mod advent;
mod day01;
mod day02;
mod day04;
mod day05;
mod day06;

fn days() -> impl IntoIterator<Item = Day> {
    [
        day01::day(),
        day02::day(),
        day04::day(),
        day05::day(),
        day06::day(),
        // d
    ]
}

fn read_input(day: usize) -> String {
    let path = format!("../.input/{:02}", day);
    let path = Path::new(&path);

    match read_to_string(Path::new(&path)) {
        Ok(text) => text,
        Err(err) => panic!("{err}"),
    }
}

fn main() {
    for (day, run) in days() {
        println!("Day {}", day);

        let text = read_input(day);

        match run(&text) {
            Err(err) => println!("ERROR: {}", err),
            Ok(solution) => println!("{:?}", solution),
        }
    }
}
