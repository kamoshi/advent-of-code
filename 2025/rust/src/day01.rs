pub fn solve(data: &str) -> (i32, i32) {
    let mut rotation = 50_i32;
    let mut count_a = 0;
    let mut count_b = 0;

    for line in data.as_bytes().split(|b| *b == b'\n') {
        if line.is_empty() {
            continue;
        }

        let direction = line[0];
        let steps = line[1..]
            .iter()
            .fold(0, |acc, &x| acc * 10 + (x - b'0') as i32);

        match direction {
            b'L' => {
                count_b += (rotation - 1).div_euclid(100) - (rotation - steps).div_euclid(100);
                rotation -= steps;
            }
            b'R' => {
                count_b += (rotation + steps - 1) / 100;
                rotation += steps;
            }
            _ => panic!("Invalid direction"),
        }

        // normalize rotation to be within 0..99
        rotation = rotation.rem_euclid(100);

        count_a += (rotation == 0) as i32;
        count_b += (rotation == 0) as i32;
    }

    (count_a, count_b)
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::*;
    use indoc::indoc;
    use test::Bencher;

    const I: &str = indoc! {"
        L68
        L30
        R48
        L5
        R60
        L55
        L1
        L99
        R14
        L82
    "};

    #[test]
    fn check() {
        assert_eq!(solve(I), (3, 6));
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            let result = solve(I);
            test::black_box(result);
        });
    }
}
