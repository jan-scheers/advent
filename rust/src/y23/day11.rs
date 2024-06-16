use crate::{norm1, size, transpose, Vec2};
pub const INPUT: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....";

pub fn solve(input: &str, e: i64) -> i64 {
    let map: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect();
    let (m, n) = size(&map);
    let mut pts = vec![];
    for i in 0..m {
        for j in 0..n {
            if map[i][j] == '#' {
                pts.push((Vec2(i as i64, j as i64), Vec2(0, 0)))
            }
        }
    }

    pts.sort_by_key(|a| a.0 .0);
    for (i, line) in map.iter().enumerate() {
        if line.iter().all(|c| *c == '.') {
            let Some(k) = (0..pts.len()).find(|k| pts[*k].0 .0 > i as i64) else {
                continue;
            };
            for k in k..pts.len() {
                pts[k].1 .0 += e;
            }
        }
    }

    pts.sort_by_key(|a| a.0 .1);
    for (j, line) in transpose(&map).iter().enumerate() {
        if line.iter().all(|c| *c == '.') {
            let Some(k) = (0..pts.len()).find(|k| pts[*k].0 .1 > j as i64) else {
                continue;
            };
            for k in k..pts.len() {
                pts[k].1 .1 += e;
            }
        }
    }

    let pts: Vec<Vec2<i64>> = pts.into_iter().map(|(pt, d)| pt + d).collect();
    (0..pts.len())
        .map(|i| {
            (i + 1..pts.len())
                .map(|j| norm1(pts[i] - pts[j]))
                .sum::<i64>()
        })
        .sum()
}

pub fn part_one(input: &str) -> i64 {
    solve(input, 1)
}

pub fn part_two(input: &str) -> i64 {
    solve(input, 999999)
}
