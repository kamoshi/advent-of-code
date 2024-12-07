use crate::advent::{day, Error};

day!(7, parse, solve_a, solve_b);

type Input = Vec<(u64, Vec<u64>)>;

fn parse(text: &str) -> Result<Input, Error> {
    let mut data = vec![];

    for line in text.lines() {
        let (outcome, numbers) = line.split_once(':').ok_or("Malformed data")?;

        let outcome = outcome.parse()?;
        let numbers = numbers
            .split_whitespace()
            .map(str::parse)
            .collect::<Result<_, _>>()?;

        data.push((outcome, numbers))
    }

    Ok(data)
}

#[allow(clippy::match_overlapping_arm)]
fn concat(a: u64, b: u64) -> u64 {
    match b {
        ..10 => 10 * a + b,
        ..100 => 100 * a + b,
        ..1000 => 1000 * a + b,
        ..10000 => 10000 * a + b,
        ..100000 => 100000 * a + b,
        ..1000000 => 1000000 * a + b,
        ..10000000 => 10000000 * a + b,
        ..100000000 => 100000000 * a + b,
        _ => panic!(),
    }
}

fn check<const CONCAT: bool>(res: u64, acc: u64, numbers: &[u64]) -> bool {
    if numbers.is_empty() {
        return res == acc;
    }

    let head = numbers[0];
    let rest = &numbers[1..];

    let add = acc + head;
    if add <= res && check::<CONCAT>(res, add, rest) {
        return true;
    }

    let mul = acc * head;
    if mul <= res && check::<CONCAT>(res, mul, rest) {
        return true;
    }

    CONCAT && {
        let con = concat(acc, head);
        con <= res && check::<CONCAT>(res, con, rest)
    }
}

fn solve_a(data: &Input) -> u64 {
    data.iter()
        .filter_map(
            |&(result, ref numbers)| match check::<false>(result, 0, numbers) {
                true => Some(result),
                false => None,
            },
        )
        .sum()
}

fn solve_b(data: &Input) -> u64 {
    data.iter()
        .filter_map(
            |&(result, ref numbers)| match check::<true>(result, 0, numbers) {
                true => Some(result),
                false => None,
            },
        )
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        190: 10 19
        3267: 81 40 27
        83: 17 5
        156: 15 6
        7290: 6 8 6 15
        161011: 16 10 13
        192: 17 8 14
        21037: 9 7 18 13
        292: 11 6 16 20
    "};

    #[test]
    fn a() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_a(&parsed);

        assert_eq!(result, 3749);
    }

    #[test]
    fn b() {
        assert_eq!(1234, concat(12, 34));

        let parsed = parse(SAMPLE).unwrap();
        let result = solve_b(&parsed);

        assert_eq!(result, 11387);
    }
}
