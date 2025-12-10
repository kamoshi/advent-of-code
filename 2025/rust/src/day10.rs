use std::collections::VecDeque;

fn parse(line: &[u8]) -> (usize, Vec<usize>, Vec<usize>) {
    let s = line.iter().position(|&b| b == b'[').unwrap_or_default();
    let e = line.iter().position(|&b| b == b']').unwrap_or_default();

    let target =
        line[s + 1..e].iter().enumerate().fold(
            0,
            |acc, (i, &b)| {
                if b == b'#' { acc | (1 << i) } else { acc }
            },
        );

    let mut ptr = e + 1;
    let mut buttons = Vec::new();

    while let Some(s) = line[ptr..].iter().position(|&b| b == b'(')
        && let Some(e) = line[ptr + s + 1..].iter().position(|&b| b == b')')
    {
        let abs_s = ptr + s + 1;
        let abs_e = abs_s + e;

        let mask = line[abs_s..abs_e]
            .split(|&b| b == b',')
            .fold(0, |acc, num| {
                let shift = num.iter().fold(0, |n, &d| n * 10 + (d - b'0') as u16);

                acc | (1 << shift)
            });

        buttons.push(mask);

        ptr = abs_e + 1;
    }

    let s = line.iter().position(|&b| b == b'{').unwrap_or_default();
    let e = line.iter().position(|&b| b == b'}').unwrap_or_default();
    let mut joltage = Vec::new();

    for num in line[s + 1..e].split(|&b| b == b',') {
        let num = num.iter().fold(0, |acc, &d| acc * 10 + (d - b'0') as usize);
        joltage.push(num);
    }

    (target, buttons, joltage)
}

pub fn solve(data: &str) -> (i64, i64) {
    let mut result_a = 0;
    let mut result_b = 0;

    let mut queue = VecDeque::with_capacity(1024); // (state, depth)
    let mut visited = vec![false; 65536];

    for line in data.as_bytes().split(|&b| b == b'\n') {
        if line.is_empty() {
            continue;
        }

        let (target, buttons, joltage) = parse(line);

        // part 1
        {
            queue.clear();
            visited.fill(false);

            queue.push_back((0, 0));
            visited[0] = true;

            let mut presses = 0;

            'bfs: while let Some((state, depth)) = queue.pop_front() {
                for &mask in &buttons {
                    let next_state = state ^ mask;

                    if next_state == target {
                        presses = depth + 1;
                        break 'bfs;
                    }

                    if let Some(cell) = visited.get_mut(next_state)
                        && !*cell
                    {
                        *cell = true;
                        queue.push_back((next_state, depth + 1));
                    }
                }
            }

            result_a += presses;
        }

        // part 2
        {
            use good_lp::{
                Expression, Solution, SolverModel, constraint, default_solver, variable, variables,
            };

            let mut vars = variables!();

            // each variable is non-negative number of presses for each button
            let button_vars = (0..buttons.len())
                .map(|_| vars.add(variable().min(0).integer()))
                .collect::<Vec<_>>();

            // the objective is to minimize the sum of button variables
            let objective = button_vars.iter().sum::<Expression>();

            // the problem is to minimize the objective
            let mut problem = vars.minimise(objective).using(default_solver);

            // for each joltage counter 0, 1, 2...
            for (index, &target) in joltage.iter().enumerate() {
                let mut lhs = Expression::from(0);

                // for each button
                for (button, &mask) in buttons.iter().enumerate() {
                    // check if this button affects this counter
                    if (mask >> index) & 1 == 1 {
                        lhs += button_vars[button];
                    }
                }

                // sum of all button presses has to equal target value
                problem.add_constraint(constraint!(lhs == target as f64));
            }

            // the result is the sum of all button presses in the solution
            let presses = problem
                .solve()
                .unwrap()
                .eval(button_vars.iter().sum::<Expression>());

            result_b += presses.round() as i64;
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
        [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
        [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
        [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
    "};

    #[test]
    fn check() {
        assert_eq!(solve(I), (7, 33));
    }

    #[bench]
    fn bench(b: &mut Bencher) {
        b.iter(|| {
            let result = solve(I);
            test::black_box(result);
        });
    }
}
