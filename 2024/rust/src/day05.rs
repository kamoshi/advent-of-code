use std::cmp::Ordering;

use crate::advent::{day, Error};

day!(5, parse, solve_a, solve_b);

type Input = (Box<[bool; 10000]>, Box<[Box<[usize]>]>);

fn parse(text: &str) -> Result<Input, Error> {
    let mut graph = Box::new([false; 10000]);
    let mut lines = text.lines();

    for line in &mut lines {
        if line.is_empty() {
            break;
        }

        let a = line[0..2].parse::<usize>()?;
        let b = line[3..5].parse::<usize>()?;
        graph[a * 100 + b] = true;
    }

    let mut updates = Vec::new();
    for line in lines {
        let update = line
            .split(",")
            .map(str::parse::<usize>)
            .collect::<Result<_, _>>()?;

        updates.push(update);
    }

    Ok((graph, Box::from(updates)))
}

fn is_update_valid(graph: &[bool; 10000], update: &[usize]) -> bool {
    let mut position = [None; 100];

    for (idx, &num) in update.iter().enumerate() {
        position[num] = Some(idx);
    }

    for &a in update {
        for &b in update {
            if graph[a * 100 + b] && position[a].unwrap() > position[b].unwrap() {
                return false;
            }
        }
    }

    true
}

fn solve_a((graph, updates): &Input) -> usize {
    updates
        .iter()
        .filter(|update| is_update_valid(graph, update))
        .map(|update| update[update.len() / 2])
        .sum()
}

fn solve_b((graph, updates): &Input) -> usize {
    updates
        .iter()
        .filter(|update| !is_update_valid(graph, update))
        .map(|update| {
            let mut update = update.to_vec();
            update.sort_unstable_by(|a, b| {
                if graph[a * 100 + b] {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            update[update.len() / 2]
        })
        .sum()
}
