use crate::advent::{day, Error};

day!(9, parse, solve_a, solve_b);

type Input = Vec<Node>;

#[derive(Clone, Copy)]
enum Node {
    File(u64, u64),
    Hole(u64),
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
                for ptr in idx..idx + size {
                    checksum += ptr * id;
                }

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
                        for ptr in idx..idx + hole {
                            checksum += ptr * id;
                        }

                        leftover = Some((left - hole, id));
                        idx += hole;
                        hole = 0;
                        continue;
                    }

                    // fill hole with whatever we have right now
                    for ptr in idx..idx + left {
                        checksum += ptr * id;
                    }

                    leftover = None;
                    idx += left;
                    hole -= left;
                }
            }
        }
    }

    // flush leftover blocks
    if let Some((left, id)) = leftover {
        for ptr in idx..idx + left {
            checksum += ptr * id;
        }
    }

    checksum
}

fn solve_b(data: &Input) -> u32 {
    2
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

    // #[test]
    // fn b() {
    //     let parsed = parse(SAMPLE).unwrap();
    //     let result = solve_b(&parsed);

    //     assert_eq!(result, 11387);
    // }
}
