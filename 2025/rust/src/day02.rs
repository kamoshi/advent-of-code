pub fn solve(data: &str) -> (i64, i64) {
    let mut count_a = 0;
    let mut count_b = 0;

    for chunk in data.as_bytes().split(|&b| b == b',') {
        let mut range = chunk.trim_ascii().split(|&b| b == b'-');

        let s_limit = range
            .next()
            .unwrap()
            .iter()
            .fold(0, |acc, &b| acc * 10 + (b - b'0') as i64);

        let e_limit = range
            .next()
            .unwrap()
            .iter()
            .fold(0, |acc, &b| acc * 10 + (b - b'0') as i64);

        count_a += solve_a(s_limit, e_limit);
    }

    (count_a, count_b)
}

fn solve_a(s_limit: i64, e_limit: i64) -> i64 {
    let mut acc = 0;

    // determine digit range to check
    // 998 is 3 digits, 1012 is 4 digits, and so on
    let min_digits = s_limit.checked_ilog10().unwrap_or(0) + 1;
    let max_digits = e_limit.checked_ilog10().unwrap_or(0) + 1;

    for digits in min_digits..=max_digits {
        // only even length numbers can be formed by repeating a sequence twice
        if digits % 2 != 0 {
            continue;
        }

        let half_len = digits / 2;
        let multiplier = 10_i64.pow(half_len) + 1;

        // define the valid domain for the root sequence S
        // for a 4 digit number, S must be 10..99
        let s_domain_min = 10_i64.pow(half_len - 1);
        let s_domain_max = 10_i64.pow(half_len) - 1;

        // bounds for S required to hit the [s_limit, e_limit] range
        // S_min = ceil(s_limit / multiplier)
        // S_max = floor(e_limit / multiplier)
        let calc_s_min = (s_limit + multiplier - 1) / multiplier;
        let calc_s_max = e_limit / multiplier;

        // intersection between the valid domain and the calculated bounds
        let actual_s = calc_s_min.max(s_domain_min);
        let actual_e = calc_s_max.min(s_domain_max);

        if actual_s <= actual_e {
            let count = actual_e - actual_s + 1;

            // sum of arithmetic progression: n/2 * (first + last)
            // sum of the roots S: count * (start + end) / 2
            // sum of the numbers: multiplier * sum_of_roots
            let sum_of_roots = (actual_s + actual_e) * count / 2;
            acc += sum_of_roots * multiplier;
        }
    }

    acc
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use indoc::indoc;
    use test::Bencher;

    const I: &str = indoc! {"
        11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
        1698522-1698528,446443-446449,38593856-38593862,565653-565659,
        824824821-824824827,2121212118-2121212124
    "};

    #[test]
    fn check() {
        assert_eq!(solve(I), (1227775554, 0));
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            let result = solve(I);
            test::black_box(result);
        });
    }
}
