#![allow(dead_code)]
use crate::utils;


pub fn run() -> () {
    let data = utils::read_lines(utils::Source::Day(25));

    println!("Day 25");
    println!("Part 1: {}", solve1(&data));
}


fn snafu2dec<T: AsRef<str>>(str: T) -> i64 {
    str.as_ref().chars()
        .fold(0, |acc, char| {
            let num = match char {
                '0' => 0,
                '1' => 1,
                '2' => 2,
                '-' => -1,
                '=' => -2,
                _ => unreachable!(),
            };
            acc * 5 + num
        })
}

fn dec2snafu(mut num: i64) -> String {
    let mut snafu = String::new();
    while num > 0 {
        let rem = num % 5;
        num = num / 5 + if rem > 2 { 1 } else { 0 };
        snafu.push(match rem {
            0 => '0',
            1 => '1',
            2 => '2',
            3 => '=',
            4 => '-',
            _ => unreachable!(),
        });
    }
    if snafu.is_empty() { snafu.push('0'); }
    snafu.chars().rev().collect()
}


fn solve1<T: AsRef<str>>(data: &[T])-> String {
    dec2snafu(data.iter().map(snafu2dec).sum())
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        "1=-0-2",
        "12111",
        "2=0=",
        "21",
        "2=01",
        "111",
        "20012",
        "112",
        "1=-1=",
        "1-12",
        "12",
        "1=",
        "122",
    ];

    #[test]
    fn part1() {
        assert_eq!(1, snafu2dec("1"));
        assert_eq!(5, snafu2dec("10"));
        assert_eq!(2022, snafu2dec("1=11-2"));
        assert_eq!(12345, snafu2dec("1-0---0"));
        assert_eq!(314159265, snafu2dec("1121-1110-1=0"));
        assert_eq!("1", dec2snafu(1));
        assert_eq!("10", dec2snafu(5));
        assert_eq!("1=11-2", dec2snafu(2022));
        assert_eq!("1-0---0", dec2snafu(12345));
        assert_eq!("1121-1110-1=0", dec2snafu(314159265));
        // test input
        assert_eq!("2=-1=0", solve1(DATA));
    }
}
