use std::collections::{BinaryHeap, HashMap};

use crate::{size, Vec2, CLOCK};

pub const INPUT: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";
extern crate nalgebra as na;

type Ix = (usize, usize);

fn parse(input: &str) -> (Ix, Ix, na::DMatrix<u8>) {
    let (mut start, mut end) = (None, None);
    let data: Vec<Vec<_>> = input
        .trim()
        .lines()
        .enumerate()
        .map(|(i, line)| {
            line.as_bytes()
                .iter()
                .enumerate()
                .map(|(j, b)| match *b {
                    b'S' => {
                        start = Some((i, j));
                        0
                    }
                    b'E' => {
                        end = Some((i, j));
                        25
                    }
                    b => b - b'a',
                })
                .collect()
        })
        .collect();
    let (m, n) = size(&data);
    (
        start.unwrap(),
        end.unwrap(),
        na::DMatrix::from_row_iterator(m, n, data.into_iter().flat_map(|r| r.into_iter())),
    )
}

pub fn part_one(input: &str) -> usize {
    let (start, end, map) = parse(input);
    let path = bfs(&map, start, end).unwrap();
    show(&map, &path);
    path.len() - 1
}

pub fn part_two(input: &str) -> usize {
    let (_, end, map) = parse(input);
    let (m, n) = map.shape();
    let map = &map;
    let path = (0..m)
        .flat_map(|i| {
            (0..n).filter_map(move |j| {
                if map[(i, j)] == 0 {
                    bfs(map, (i, j), end)
                } else {
                    None
                }
            })
        })
        .min_by_key(|p| p.len())
        .unwrap();
    show(&map, &path);
    path.len() - 1
}

fn show(map: &na::DMatrix<u8>, path: &Vec<Ix>) {
    let (m, n) = map.shape();
    let mat = na::DMatrix::from_fn(m, n, |i, j| {
        if path.contains(&(i, j)) {
            '#'
        } else {
            (map[(i, j)] + b'a') as char
        }
    });
    println!("{}", mat);
}
#[derive(PartialEq, Eq)]
struct Path {
    path: Vec<Ix>,
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.path.len().partial_cmp(&self.path.len())
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.path.len().cmp(&self.path.len())
    }
}

fn bfs(map: &na::DMatrix<u8>, start: Ix, end: Ix) -> Option<Vec<Ix>> {
    let mut best = HashMap::new();
    let mut front = BinaryHeap::from([Path { path: vec![start] }]);
    let (m, n) = (map.shape().0 as i64, map.shape().1 as i64);
    while let Some(Path { path }) = front.pop() {
        let node = path[path.len() - 1];
        if node == end {
            return Some(path);
        }
        if best.contains_key(&node) {
            continue;
        }
        best.insert(node, path.clone());
        let height = map[node];
        for dir in CLOCK.iter() {
            let Vec2(i, j) = *dir + Vec2(node.0 as i64, node.1 as i64);
            if 0 <= i && i < m && 0 <= j && j < n {
                let next = (i as usize, j as usize);
                if map[next] <= height + 1 && !best.contains_key(&next) {
                    let mut path = path.clone();
                    path.push(next);
                    front.push(Path { path });
                }
            }
        }
    }
    None
}
