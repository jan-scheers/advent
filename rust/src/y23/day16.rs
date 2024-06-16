use crate::{size, Vec2};

pub const INPUT: &str = ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....";

pub fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

const NORTH: u8 = 0b0001;
const EAST: u8 = 0b0010;
const SOUTH: u8 = 0b0100;
const WEST: u8 = 0b1000;
const DIRS: &[Vec2<i32>] = &[
    Vec2(0, 0),
    Vec2(-1, 0),
    Vec2(0, 1),
    Vec2(0, 0),
    Vec2(1, 0),
    Vec2(0, 0),
    Vec2(0, 0),
    Vec2(0, 0),
    Vec2(0, -1),
];

fn calc(map: &Vec<Vec<char>>, start: (u8, Vec2<i32>)) -> usize {
    let (m, n) = size(map);
    let mut energy = vec![vec![0u8; n]; m];
    let mut beams = vec![start];
    while let Some((dir, curr)) = beams.pop() {
        let Vec2(i, j) = curr + DIRS[dir as usize];
        if i < 0 || i >= m as i32 || j < 0 || j >= n as i32 {
            continue;
        }
        let (i, j) = (i as usize, j as usize);
        if energy[i][j] & dir > 0 {
            continue;
        }
        energy[i][j] |= dir;
        let dirs = match map[i][j] {
            '|' => {
                if dir == NORTH || dir == SOUTH {
                    vec![dir]
                } else {
                    vec![NORTH, SOUTH]
                }
            }
            '-' => {
                if dir == EAST || dir == WEST {
                    vec![dir]
                } else {
                    vec![EAST, WEST]
                }
            }
            '/' => vec![match dir {
                NORTH => EAST,
                EAST => NORTH,
                SOUTH => WEST,
                WEST => SOUTH,
                _ => 0,
            }],
            '\\' => vec![match dir {
                NORTH => WEST,
                WEST => NORTH,
                SOUTH => EAST,
                EAST => SOUTH,
                _ => 0,
            }],
            _ => vec![dir],
        };

        for dir in dirs.into_iter() {
            beams.push((dir, Vec2(i as i32, j as i32)));
        }
    }
    energy
        .iter()
        .map(|row| row.iter().filter(|b| **b > 0).count())
        .sum()
}

pub fn part_one(input: &str) -> usize {
    let map = parse(input);
    calc(&map, (EAST, Vec2(0, -1)))
}

pub fn part_two(input: &str) -> usize {
    let map = parse(input);
    let (m, n) = size(&map);
    let mut starts = vec![];
    for row in 0..m as i32 {
        starts.push((EAST, Vec2(row, -1)));
        starts.push((WEST, Vec2(row, n as i32)));
    }
    for col in 0..n as i32 {
        starts.push((SOUTH, Vec2(-1, col)));
        starts.push((NORTH, Vec2(m as i32, col)));
    }
    starts.iter().map(|start| calc(&map, *start)).max().unwrap()
}
