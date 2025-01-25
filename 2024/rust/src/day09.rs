use crate::advent::{day, Error};

day!(9, parse, solve_a, solve_b);

type Input = Vec<u32>;

fn parse(text: &str) -> Result<Input, Error> {
    // let text = "2333133121414131402";
    text.trim()
        .chars()
        .map(|c| {
            c.to_digit(10)
                .ok_or_else(|| format!("parse error on {c}").into())
        })
        .collect()
}

fn solve_a(data: &Input) -> u32 {
    // println!("{:?}", data);
    1
}

fn solve_b(data: &Input) -> u32 {
    2
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        2333133121414131402
    "};

    #[test]
    fn a() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_a(&parsed);

        assert_eq!(result, 1928);
    }

    // #[test]
    // fn b() {
    //     let parsed = parse(SAMPLE).unwrap();
    //     let result = solve_b(&parsed);

    //     assert_eq!(result, 11387);
    // }
}
