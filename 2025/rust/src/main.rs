use indoc::indoc;

const A: &str = indoc! {
    "L68
    L30
    R48
    L5
    R60
    L55
    L1
    L99
    R14
    L82"
};

const I1: &str = include_str!("../input");

fn main() {
    let mut rotation = 50;
    let mut count_a = 0;
    let mut count_b = 0;

    for line in I1.lines() {
        let (direction, steps) = line.trim().split_at(1);
        let steps = steps.parse::<i64>().unwrap();

        count_b += match direction {
            "R" => (rotation + steps - 1) / 100,
            "L" => (rotation - 1).div_euclid(100) - (rotation - steps).div_euclid(100),
            _ => panic!("Invalid direction"),
        };

        match direction {
            "L" => rotation -= steps,
            "R" => rotation += steps,
            _ => panic!("Invalid direction"),
        }

        // normalize rotation to be within 0..99
        rotation %= 100;
        if rotation < 0 {
            rotation += 100;
        }

        count_a += (rotation == 0) as usize;
        count_b += (rotation == 0) as i64;
    }

    println!("a: {}", count_a);
    println!("b: {}", count_b);
}
