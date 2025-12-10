pub fn solve(data: &str) -> (u64, u64) {
    let mut lines = vec![];
    let mut acc_a = [(0, false); 1024];

    for line in data.as_bytes().split(|&b| b == b'\n').rev() {
        if line.is_empty() {
            continue;
        }

        lines.push(line);

        for part in line
            .split(|&b| b == b' ')
            .filter(|s| !s.is_empty())
            .enumerate()
        {
            match part {
                (i, [b'+', ..]) => acc_a[i] = (0, false),
                (i, [b'*', ..]) => acc_a[i] = (1, true),
                (i, n) => {
                    let n = n.iter().fold(0, |acc, &b| acc * 10 + (b - b'0') as u64);
                    match acc_a[i].1 {
                        true => acc_a[i].0 *= n,
                        false => acc_a[i].0 += n,
                    }
                }
            }
        }
    }

    let result_a = acc_a.iter().map(|(val, _)| *val).sum();
    let mut result_b = 0;

    {
        let depth = lines.iter().map(|line| line.len()).max().unwrap_or(0);

        let mut temp_add = 0;
        let mut temp_mul = 1;
        let mut mul = false;

        for i in 0..depth {
            let line = lines
                .iter()
                .rev()
                .map(|row| row.get(i).copied().unwrap_or_default())
                .collect::<Vec<_>>();
            let line = line.trim_ascii();

            if line.is_empty() {
                if mul {
                    result_b += temp_mul;
                } else {
                    result_b += temp_add;
                }
                temp_add = 0;
                temp_mul = 1;
                continue;
            }

            let mut number = 0;
            for char in lines
                .iter()
                .rev()
                .map(|row| row.get(i).copied().unwrap_or_default())
            {
                match char {
                    b'+' => mul = false,
                    b'*' => mul = true,
                    b'0'..=b'9' => number = number * 10 + (char - b'0') as u64,
                    _ => {}
                }
            }

            temp_add += number;
            temp_mul *= number;
        }

        if mul {
            result_b += temp_mul;
        } else {
            result_b += temp_add;
        }
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
        123 328  51 64
         45 64  387 23
          6 98  215 314
        *   +   *   +
    "};

    #[test]
    fn check() {
        assert_eq!(solve(I), (4277556, 3263827));
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            let result = solve(I);
            test::black_box(result);
        });
    }
}
