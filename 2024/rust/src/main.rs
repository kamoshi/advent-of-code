use std::fs::read_to_string;
use std::path::Path;

mod advent;

mod day01;

fn read_input(day: usize) -> String {
    let path = format!("../.input/{:02}", day);
    let path = Path::new(&path);

    match read_to_string(Path::new(&path)) {
        Ok(text) => text,
        Err(err) => panic!("{err}"),
    }
}

fn main() {
    let days = [day01::day()];

    for (day, run) in days {
        println!("Day {}", day);

        let text = read_input(day);

        match run(&text) {
            Err(err) => println!("ERROR: {}", err),
            Ok(solution) => println!("{:?}", solution),
        }
    }
}
