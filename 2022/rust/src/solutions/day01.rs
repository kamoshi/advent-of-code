use crate::utils;

pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(1)));

    println!("Day 1");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}

fn solve1(data: &Vec<Vec<i32>>) -> i32 {
    data.iter().map(|xs| xs.iter().sum::<i32>()).max().unwrap()
}

#[inline]
fn insert(arr: &mut [i32; 3], to_insert: i32) {
    if to_insert > arr[2] {
        for idx in (0..3).rev() {
            if idx != 0 && arr[idx - 1] < to_insert {
                arr[idx] = arr[idx - 1];
            }
            else {
                arr[idx] = to_insert;
                break;
            }
        }
    }
}

fn solve2(data: &Vec<Vec<i32>>) -> i32 {
    let mut three = [0; 3];

    for x in data {
        let sum = x.iter().sum::<i32>();
        insert(&mut three, sum);
    }

    three.iter().sum()
}


fn parse_data(data: Vec<String>) -> Vec<Vec<i32>> {
    data.iter()
        .fold(vec![vec![]], | mut acc, next| {
            if next.len() == 0 {
                acc.push(Vec::new())
            }
            else {
                let number: i32 = next.parse().expect("Parse error");
                acc.last_mut().and_then(|last| Some(last.push(number)));
            }
            acc
        })
}
