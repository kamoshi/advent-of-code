use crate::advent::{day, Error};

day!(7, parse, solve_a, solve_b);

type Input = Vec<(i64, Vec<i64>)>;

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

fn check(res: i64, acc: i64, numbers: &[i64]) -> bool {
    if numbers.is_empty() {
        return res == acc;
    }

    let a = {
        let acc = acc + numbers[0];
        acc <= res && check(res, acc, &numbers[1..])
    };

    let b = {
        let acc = acc * numbers[0];
        acc <= res && check(res, acc, &numbers[1..])
    };

    a || b
}

fn solve_a(data: &Input) -> i64 {
    data.iter()
        .filter_map(|&(result, ref numbers)| match check(result, 0, numbers) {
            true => Some(result),
            false => None,
        })
        .sum()
}

fn solve_b(b: &Input) -> usize {
    2
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
}
