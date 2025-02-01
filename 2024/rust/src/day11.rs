use std::collections::HashMap;

use crate::advent::{day, Error};

day!(11, parse, solve_a, solve_b);

type Input = Vec<u64>;

enum Transform {
    Mono(u64),
    Pair(u64, u64),
}

fn parse(text: &str) -> Result<Input, Error> {
    let mut data = vec![];

    for item in text.trim().split_whitespace() {
        data.push(item.parse()?);
    }

    Ok(data)
}

// https://lemire.me/blog/2021/06/03/computing-the-number-of-digits-of-an-integer-even-faster/
// https://lemire.me/blog/2025/01/07/counting-the-digits-of-64-bit-integers/
fn fast_digit_count(x: u64) -> u64 {
    #[rustfmt::skip]
    const DIGITS: [u64; 64] = [
        19, 19, 19, 19, 18, 18, 18,
        17, 17, 17, 16, 16, 16, 16,
        15, 15, 15, 14, 14, 14,
        13, 13, 13, 13, 12, 12, 12,
        11, 11, 11, 10, 10, 10, 10,
        9,  9,  9,  8,  8,  8,
        7,  7,  7,  7,  6,  6,  6,
        5,  5,  5,  4,  4,  4,  4,
        3,  3,  3,  2,  2,  2,
        1,  1,  1,  1,
    ];
    #[rustfmt::skip]
    const TABLE: [u64; 64] = [
        18446744073709551615,
        18446744073709551615,
        18446744073709551615,
        18446744073709551615,
        999999999999999999,
        999999999999999999,
        999999999999999999,
        99999999999999999,
        99999999999999999,
        99999999999999999,
        9999999999999999,
        9999999999999999,
        9999999999999999,
        9999999999999999,
        999999999999999,
        999999999999999,
        999999999999999,
        99999999999999,
        99999999999999,
        99999999999999,
        9999999999999,
        9999999999999,
        9999999999999,
        9999999999999,
        999999999999,
        999999999999,
        999999999999,
        99999999999,
        99999999999,
        99999999999,
        9999999999,
        9999999999,
        9999999999,
        9999999999,
        999999999,
        999999999,
        999999999,
        99999999,
        99999999,
        99999999,
        9999999,
        9999999,
        9999999,
        9999999,
        999999,
        999999,
        999999,
        99999,
        99999,
        99999,
        9999,
        9999,
        9999,
        9999,
        999,
        999,
        999,
        99,
        99,
        99,
        9,
        9,
        9,
        9,
    ];

    let leading = x.leading_zeros() as usize;
    (x > TABLE[leading]) as u64 + DIGITS[leading]
}

fn transform(key: u64) -> Transform {
    if key == 0 {
        return Transform::Mono(1);
    }

    let digits = fast_digit_count(key);
    if digits % 2 == 0 {
        let split = 10_u64.pow(digits as u32 / 2);
        return Transform::Pair(key / split, key % split);
    }

    Transform::Mono(key * 2024)
}

fn solve<const N: usize>(data: &Input) -> u64 {
    let mut nums = HashMap::new();

    for &n in data {
        *nums.entry(n).or_default() += 1;
    }

    for _ in 0..N {
        let mut next = HashMap::new();

        for (k, v) in nums.into_iter() {
            match transform(k) {
                Transform::Mono(n) => *next.entry(n).or_default() += v,
                Transform::Pair(a, b) => {
                    *next.entry(a).or_default() += v;
                    *next.entry(b).or_default() += v;
                }
            }
        }

        nums = next;
    }

    nums.values().sum()
}

fn solve_a(data: &Input) -> u64 {
    solve::<25>(data)
}

fn solve_b(data: &Input) -> u64 {
    solve::<75>(data)
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;

    const SAMPLE: &str = indoc! {"
        125 17
    "};

    #[test]
    fn a() {
        let parsed = parse(SAMPLE).unwrap();

        assert_eq!(solve::<6>(&parsed), 22);
        assert_eq!(solve::<25>(&parsed), 55312);
    }
}
