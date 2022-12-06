use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Scratch));

    println!("Day 5");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


fn solve1(data: &()) -> i32 {
    1
}

fn solve2(data: &()) -> i32 {
    2
}


fn parse_data(data: Vec<String>) -> () {
    ()
}


#[cfg(test)]
mod tests {
    use super::*;

    fn data() -> Vec<String> {
        vec![""]
            .into_iter().map(String::from).collect()
    }

    #[test]
    fn part1() {
        let data = parse_data(data());
        assert_eq!(1, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data());
        assert_eq!(2, solve2(&data));
    }
}
