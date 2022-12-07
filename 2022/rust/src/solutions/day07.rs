#![allow(dead_code)]
use std::collections::BinaryHeap;
use crate::utils;


pub fn run() -> () {
    let data = parse_data(&utils::read_lines(utils::Source::Day(7)));

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

struct FileNode {
    size: i32,
}

#[derive(Default)]
struct Node<'data> {
    name: &'data str,
    // Backref pointer
    parent: Option<*mut Node<'data>>,
    files: Vec<FileNode>,
    subdirs: Vec<Box<Node<'data>>>,
}

fn cli_to_tree(data: &[CliEntry]) -> Box<Node> {
    let mut root = Box::new(Node { name: "/", ..Default::default() });

    let mut current: &mut Node = root.as_mut();
    for command in data {
        match command {
            CliEntry::Cd(name) => {
                if name == ".." {
                    // We don't care, backref has to be valid
                    unsafe {
                        current = &mut *current.parent.unwrap()
                    };
                }
                else {
                    current = &mut *current.subdirs.iter_mut()
                        .find(|next| next.name == name)
                        .unwrap();
                }
            },
            CliEntry::Ls => {},
            CliEntry::Dir(name) => {
                let current_ptr = current as *mut Node;
                current.subdirs.push(Box::new(Node { name, parent: Some(current_ptr), ..Default::default() }));
            },
            CliEntry::File(_, size) => {
                current.files.push(FileNode { size: *size })
            },
        }
    };
    root
}

fn find_sizes(node: &Box<Node>, heap: &mut BinaryHeap<i32>) -> i32 {
    let mut acc: i32 = 0;
    for file in &node.files {
        acc += file.size;
    }
    for subdir in &node.subdirs {
        acc += find_sizes(subdir, heap);
    }
    heap.push(acc);
    acc
}

fn solve1(data: &[CliEntry]) -> i32 {
    let tree = cli_to_tree(&data[1..]);
    let mut heap: BinaryHeap<i32> = BinaryHeap::new();
    find_sizes(&tree, &mut heap);

    heap.into_iter()
        .filter_map(|size| size
            .le(&100000)
            .then_some(size)
        )
        .sum()
}

fn solve2(data: &[CliEntry]) -> i32 {
    static DISK_SPACE: i32 = 70000000;
    static REQUIRED: i32 = 30000000;
    let tree = cli_to_tree(&data[1..]);
    let mut heap: BinaryHeap<i32> = BinaryHeap::new();
    let size = find_sizes(&tree, &mut heap);

    let threshold = REQUIRED - (DISK_SPACE - size);
    heap.into_sorted_vec()
        .into_iter()
        .filter(|&x| x > threshold)
        .next()
        .unwrap()
}


fn parse_data<T: AsRef<str>>(data: &[T]) -> Vec<CliEntry> {
    data.into_iter()
        .map(|str_ref| {
            let s = str_ref.as_ref();
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

    static DATA: &[&str; 23] = &[
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
    ];

    #[test]
    fn part1() {
        let data = parse_data(DATA);
        assert_eq!(95437, solve1(&data));
    }

    #[test]
    fn part2() {
        let data = parse_data(DATA);
        assert_eq!(24933642, solve2(&data));
    }
}
