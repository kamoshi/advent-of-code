use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;


#[repr(transparent)]
pub struct Day(pub i32);


pub fn read_lines(day: Day) -> Vec<String> {
    assert!(1 <= day.0 && day.0 < 25);
    _read_lines(format!("inputs/day{:0>2}.txt", day.0))
        .expect(format!("Failed to load day {}", day.0).as_str())
}

fn _read_lines<P>(filename: P) -> std::io::Result<Vec<String>>
    where P: AsRef<Path> {
    let file = File::open(filename)?;
    BufReader::new(file).lines().collect()
}
