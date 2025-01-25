use std::collections::HashSet;

use crate::advent::{day, Error};

day!(9, parse, solve_a, solve_b);

type Input = Vec<Node>;

#[derive(Clone, Copy)]
enum Node {
    File(u64, u64),
    Hole(u64),
}

const CHECKSUM_LUT: [u64; 11] = [
    0,
    0,
    1,
    2 + 1,
    3 + 2 + 1,
    4 + 3 + 2 + 1,
    5 + 4 + 3 + 2 + 1,
    6 + 5 + 4 + 3 + 2 + 1,
    7 + 6 + 5 + 4 + 3 + 2 + 1,
    8 + 7 + 6 + 5 + 4 + 3 + 2 + 1,
    9 + 8 + 7 + 6 + 5 + 4 + 3 + 2 + 1,
];

fn calc_checksum(offset: u64, size: u64, id: u64) -> u64 {
    (offset * size + CHECKSUM_LUT[size as usize]) * id
}

fn parse(text: &str) -> Result<Input, Error> {
    let mut buffer = Vec::with_capacity(text.len());

    for (index, char) in text.trim().chars().enumerate() {
        let parsed = char
            .to_digit(10)
            .ok_or_else(|| format!("parse error on {char}"))? as u64;

        buffer.push(match index % 2 == 0 {
            true => Node::File(parsed, index as u64 / 2),
            false => Node::Hole(parsed),
        })
    }

    Ok(buffer)
}

fn solve_a(data: &Input) -> u64 {
    let mut iter = data.iter().copied();
    let mut checksum = 0;
    let mut leftover = None;
    let mut idx = 0;

    while let Some(next) = iter.next() {
        match next {
            Node::File(size, id) => {
                checksum += calc_checksum(idx, size, id);
                idx += size;
            }
            Node::Hole(mut hole) => {
                while hole > 0 {
                    let (left, id) = match leftover {
                        // if there are leftover blocks for old files
                        Some(blocks) => blocks,
                        // if not we need to grab new blocks from the back
                        None => loop {
                            match iter.next_back() {
                                Some(Node::File(size, id)) => break (size, id),
                                Some(Node::Hole(_)) => continue,
                                None => return checksum,
                            }
                        },
                    };

                    // if there is more blocks than hole
                    if left > hole {
                        checksum += calc_checksum(idx, hole, id);
                        leftover = Some((left - hole, id));
                        idx += hole;
                        hole = 0;
                        continue;
                    }

                    // fill hole with whatever we have right now
                    checksum += calc_checksum(idx, left, id);
                    leftover = None;
                    idx += left;
                    hole -= left;
                }
            }
        }
    }

    // flush leftover blocks
    if let Some((left, id)) = leftover {
        checksum += calc_checksum(idx, left, id);
    }

    checksum
}

fn find_fitting(list: &[Node], skip: &HashSet<u64>, hole: u64) -> Option<(u64, u64, usize)> {
    for (offset, node) in list.iter().rev().enumerate() {
        match node {
            Node::File(size, id) if *size <= hole && !skip.contains(id) => {
                return Some((*size, *id, offset))
            }
            _ => continue,
        }
    }

    None
}

fn solve_b(data: &Input) -> u64 {
    let mut checksum = 0;
    let mut skip = HashSet::new();
    let mut lims = [data.len(); 10];
    let mut idx = 0;

    for (limit_s, node) in data.iter().enumerate() {
        match *node {
            Node::File(size, id) => {
                if skip.contains(&id) {
                    idx += size;
                    continue;
                }

                checksum += calc_checksum(idx, size, id);
                idx += size;
            }
            Node::Hole(mut hole) => {
                while hole > 0 {
                    let limit_e = &mut lims[hole as usize];

                    if *limit_e < limit_s {
                        break;
                    }

                    // search area going from the end
                    let slice = &data[limit_s..*limit_e];

                    match find_fitting(slice, &skip, hole) {
                        Some((size, id, offset)) => {
                            checksum += calc_checksum(idx, size, id);
                            hole -= size;
                            idx += size;
                            skip.insert(id);
                            *limit_e -= offset;
                        }
                        None => {
                            *limit_e = 0;
                            break;
                        }
                    };
                }

                idx += hole;
            }
        }
    }

    checksum
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

    #[test]
    fn b() {
        let parsed = parse(SAMPLE).unwrap();
        let result = solve_b(&parsed);

        assert_eq!(result, 2858);
    }
}
