pub fn solve(data: &str) -> (i64, i64) {
    let mut result_a = 0;
    let mut result_b = 0;

    for line in data.as_bytes().split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }

        // part a
        let mut bank = 0;
        let mut best = 0;
        // part b
        let mut stack = [0; 12];
        let mut top = 0;

        for (i, &d) in line.iter().enumerate() {
            let d = (d - b'0') as i64;

            // part a
            {
                let attempt = best * 10 + d;

                if attempt > bank {
                    bank = attempt;
                }

                if d > best {
                    best = d;
                }
            }

            // part b
            {
                let remaining = line.len() - 1 - i;

                while top > 0 && stack[top - 1] < d && top + remaining >= 12 {
                    top -= 1;
                }

                if top < 12 {
                    stack[top] = d;
                    top += 1;
                }
            }
        }

        result_a += bank;
        result_b += stack.iter().fold(0, |acc, &d| acc * 10 + d);
    }

    (result_a, result_b)
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use indoc::indoc;
    use test::Bencher;

    const I: &str = indoc! {"
        987654321111111
        811111111111119
        234234234234278
        818181911112111
    "};

    #[test]
    fn check() {
        assert_eq!(solve(I), (357, 3121910778619));
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            let result = solve(I);
            test::black_box(result);
        });
    }
}
