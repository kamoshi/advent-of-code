use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(-1)));

    println!("Day X");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &()) -> i32 {
    1
}

fn solve2(data: &()) -> i32 {
    2
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> () {
    ()
}


#[cfg(test)]
mod tests {
    use super::*;

    static DATA: &[&str] = &[
        ""
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(1, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(2, solve2(&data));
    }
}
