use std::iter::zip;

use crate::{rotclck, size};

pub const INPUT: &str = "30373
25512
65332
33549
35390";

pub fn parse(input: &str) -> Vec<Vec<u32>> {
    input
        .trim()
        .lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let mut map: Vec<Vec<u32>> = parse(input);
    let mut seen = see(&map);
    for _ in 0..3 {
        map = rotclck(&map);
        seen = zip(rotclck(&seen), see(&map))
            .map(|(a, b)| zip(a, b).map(|(a, b)| a || b).collect())
            .collect();
    }
    prt(&map, &seen);
    seen.into_iter()
        .map(|r| r.into_iter().map(|b| b as usize).sum::<usize>())
        .sum()
}

fn see(map: &Vec<Vec<u32>>) -> Vec<Vec<bool>> {
    map.iter()
        .map(|row| {
            let mut tree = None;
            row.iter()
                .map(|&val| {
                    let visible = tree.map(|tree| tree < val).unwrap_or(true);
                    if visible {
                        tree = Some(val);
                    }
                    visible
                })
                .collect()
        })
        .collect()
}

fn prt(map: &Vec<Vec<u32>>, seen: &Vec<Vec<bool>>) {
    zip(rotclck(&map), rotclck(&seen)).for_each(|(row, seen)| {
        println!(
            "{}",
            zip(row, seen)
                .map(|(d, seen)| {
                    if seen {
                        d.to_string().chars().next().unwrap()
                    } else {
                        ' '
                    }
                })
                .collect::<String>()
        );
    })
}

pub fn part_two(input: &str) -> usize {
    let map = parse(input);
    let (m, n) = size(&map);
    (0..m)
        .map(|row| {
            (0..n)
                .map(|col| {
                    let height = map[row][col];
                    [
                        (0..row).rev().map(|row| (row, col)).collect(),
                        (row + 1..m).map(|row| (row, col)).collect(),
                        (0..col).rev().map(|col| (row, col)).collect(),
                        (col + 1..n).map(|col| (row, col)).collect(),
                    ]
                    .map(|v: Vec<_>| {
                        v.iter()
                            .position(|idx| height <= map[idx.0][idx.1])
                            .map(|dist| dist + 1)
                            .unwrap_or(v.len())
                    })
                    .iter()
                    .product::<usize>()
                })
                .max()
                .unwrap()
        })
        .max()
        .unwrap()
}
