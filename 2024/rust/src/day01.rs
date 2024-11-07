use crate::advent::day;

fn parse(text: &str) -> Result<i32, String> {
    Ok(2)
}

fn solve_a(n: &i32) -> i32 {
    n + 1
}

fn solve_b(n: &i32) -> i32 {
    n * 2
}

day!(1, parse, solve_a, solve_b);
