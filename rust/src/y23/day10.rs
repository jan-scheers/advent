use crate::{pretty, Vec2};
use std::collections::HashSet;

pub const INPUT1: &str = "
-L|F7
7S-7|
L|7||
-L-J|
L|-JF";

pub const INPUT2: &str = "..F7.
.FJ|.
SJ.L7
|F--J
LJ...";

pub const INPUT3: &str = "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........";

pub const INPUT4: &str = ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...";

pub const INPUT5: &str = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L";

const NORTH: Vec2<i64> = Vec2(-1, 0);
const SOUTH: Vec2<i64> = Vec2(1, 0);
const WEST: Vec2<i64> = Vec2(0, -1);
const EAST: Vec2<i64> = Vec2(0, 1);

fn pipe(pos: Vec2<i64>, map: &Vec<Vec<char>>) -> Vec<Vec2<i64>> {
    let (m, n) = (map.len() as i64, map[0].len() as i64);
    let Vec2(i, j) = pos;
    let x = match map[i as usize][j as usize] {
        '|' => vec![NORTH, SOUTH],
        '-' => vec![EAST, WEST],
        'L' => vec![NORTH, EAST],
        'J' => vec![NORTH, WEST],
        '7' => vec![SOUTH, WEST],
        'F' => vec![SOUTH, EAST],
        _ => vec![],
    }
    .into_iter()
    .filter_map(|dir| {
        let Vec2(i, j) = pos + dir;
        if 0 <= i && i < m && 0 <= j && j <= n {
            Some(Vec2(i, j))
        } else {
            None
        }
    })
    .collect();
    return x;
}

pub fn parse(input: &str) -> Vec<Vec<char>> {
    let mut map: Vec<Vec<char>> = input.trim().lines().map(|l| l.chars().collect()).collect();
    let (m, n) = (map.len() as i64, map[0].len() as i64);

    let mut start: Vec2<i64> = Vec2(-2, -2);
    'find: for i in 0..m {
        for j in 0..n {
            if map[i as usize][j as usize] == 'S' {
                start = Vec2(i as i64, j as i64);
                break 'find;
            }
        }
    }

    let next = [NORTH, SOUTH, EAST, WEST]
        .iter()
        .map(|dir| start + *dir)
        .find(|dir| {
            (dir.0 >= 0 && dir.0 < m)
                && (dir.1 >= 0 && dir.1 < n)
                && pipe(*dir, &map).iter().any(|rev| *rev == start)
        })
        .unwrap();

    let mut path = vec![start, next];
    loop {
        let dirs = pipe(path[path.len() - 1], &map);
        if dirs.len() == 0 {
            break;
        }
        path.push(*dirs.iter().find(|d| **d != path[path.len() - 2]).unwrap())
    }
    path.pop();
    let prev = path[path.len() - 1];
    map[start.0 as usize][start.1 as usize] = match (prev - start, next - start) {
        (NORTH, SOUTH) => '|',
        (SOUTH, NORTH) => '|',
        (EAST, WEST) => '-',
        (WEST, EAST) => '-',
        (NORTH, EAST) => 'L',
        (EAST, NORTH) => 'L',
        (NORTH, WEST) => 'J',
        (WEST, NORTH) => 'J',
        (SOUTH, WEST) => '7',
        (WEST, SOUTH) => '7',
        (SOUTH, EAST) => 'F',
        (EAST, SOUTH) => 'F',
        _ => '.',
    };
    let path: HashSet<_> = path.into_iter().collect();
    for i in 0..m {
        for j in 0..n {
            if !path.contains(&Vec2(i, j)) {
                map[i as usize][j as usize] = '.';
            }
        }
    }
    return map;
}

pub fn part_one(input: &str) -> usize {
    parse(input)
        .iter()
        .map(|row| row.iter().filter(|c| **c != '.').count())
        .sum::<usize>()
        / 2
}

pub fn part_two(input: &str) -> usize {
    let show: Vec<Vec<char>> = parse(input)
        .iter()
        .map(|row| {
            let (mut inside, mut from_up) = (false, false);
            row.iter()
                .map(|c| {
                    match c {
                        'L' => from_up = true,
                        'F' => from_up = false,
                        '|' => inside = !inside,
                        '7' => {
                            if from_up {
                                inside = !inside
                            }
                        }
                        'J' => {
                            if !from_up {
                                inside = !inside
                            }
                        }
                        _ => {}
                    }

                    return if *c == '.' {
                        if inside {
                            '#'
                        } else {
                            ' '
                        }
                    } else {
                        '.'
                    };
                })
                .collect()
        })
        .collect();
    pretty(&show);
    return show
        .iter()
        .map(|row| row.iter().filter(|c| **c == '#').count())
        .sum();
}
