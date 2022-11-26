use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;


pub enum Source { Scratch, Day(i32) }


pub fn read_lines(source: Source) -> Vec<String> {
    let path = match source {
        Source::Scratch => "inputs/scratch.txt".to_string(),
        Source::Day(day) => {
            assert!(1 <= day && day < 25);
            format!("inputs/day{:0>2}.txt", day)
        }
    };
    _read_lines(&path).expect(format!("Failed to load from {}", &path).as_str())
}

fn _read_lines<P>(filename: P) -> std::io::Result<Vec<String>>
    where P: AsRef<Path> {
    let file = File::open(filename)?;
    BufReader::new(file).lines().collect()
}
