use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use crate::utils;


pub fn run() -> () {
    let data = parse_data(utils::read_lines(utils::Source::Day(7)));

    println!("Day 7");
    println!("Part 1: {}", solve1(&data));
    println!("Part 2: {}", solve2(&data));
}


enum CliEntry {
    Cd(String),
    Ls,
    Dir(String),
    File(String, i32),
}

struct FileNode<'data> {
    name: &'data str,
    size: i32,
}

struct Node<'data> {
    name: &'data str,
    parent: Option<*mut Node<'data>>,
    files: Vec<FileNode<'data>>,
    subdirs: Vec<Box<Node<'data>>>,
}

fn cli_to_tree(data: &[CliEntry]) -> Box<Node> {
    let mut root = Box::new(Node {
        name: "/",
        parent: None,
        files: vec![],
        subdirs: vec![],
    });

    let mut current: &mut Node = root.as_mut();
    for command in data {
        match command {
            CliEntry::Cd(name) => {
                if name == ".." {
                    // We don't care, input has to be valid
                    unsafe {
                        current = &mut *current.parent.unwrap()
                    };
                }
                else {
                    current = current.subdirs.iter_mut()
                        .find(|next| next.name == name)
                        .unwrap()
                        .deref_mut();
                }
            },
            CliEntry::Ls => {},
            CliEntry::Dir(name) => {
                let current_ptr = current as *mut Node;
                current.subdirs.push(Box::new(Node {
                    name,
                    parent: Some(current_ptr),
                    files: vec![],
                    subdirs: vec![]
                }));
            },
            CliEntry::File(name, size) => {
                current.files.push(FileNode { name, size: *size })
            },
        }
    };
    root
}

fn find_sizes(root: &Box<Node>) -> (i32, Vec<i32>) {
    let mut acc: i32 = root.files.iter().map(|f| f.size).sum();
    let mut sizes: Vec<i32> = vec![];
    root.subdirs.iter().for_each(|subdir| {
        let (sub_size, sub_sizes) = find_sizes(subdir);
        sizes.extend_from_slice(&sub_sizes);
        acc += sub_size;
    });
    sizes.push(acc);
    (acc, sizes)
}

fn solve1(data: &[CliEntry]) -> i32 {
    let tree = cli_to_tree(&data[1..]);
    let (_, sizes) = find_sizes(&tree);

    sizes.into_iter()
        .filter_map(|size| size
            .le(&100000)
            .then_some(size)
        )
        .sum()
}

fn solve2(data: &[CliEntry]) -> i32 {
    2
}


fn parse_data(data: Vec<String>) -> Vec<CliEntry> {
    data.into_iter()
        .map(|s| {
            let mut parts = s.split_whitespace().into_iter();
            if s.starts_with('$') {
                match parts.nth(1).unwrap() {
                    "cd" => { CliEntry::Cd(parts.next().unwrap().into()) },
                    "ls" => { CliEntry::Ls },
                    _ => panic!(),
                }
            }
            else {
                match parts.next().unwrap() {
                    "dir" => CliEntry::Dir(parts.next().unwrap().into()),
                    other => CliEntry::File(parts.next().unwrap().into(), other.parse().unwrap())
                }
            }
        })
        .collect()
}


#[cfg(test)]
mod tests {
    use super::*;

    fn data() -> Vec<String> {
        vec![
            "$ cd /",
            "$ ls", "dir a", "14848514 b.txt", "8504156 c.dat", "dir d",
            "$ cd a",
            "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst",
            "$ cd e",
            "$ ls", "584 i",
            "$ cd ..",
            "$ cd ..",
            "$ cd d",
            "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k",
        ]
            .into_iter().map(String::from).collect()
    }

    #[test]
    fn part1() {
        let data = parse_data(data());
        assert_eq!(95437, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(data());
        assert_eq!(2, solve2(&data));
    }
}
