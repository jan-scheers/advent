use crate::{size, Vec2};
use std::cmp;

pub const INPUT: &str = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)";

const NORTH: usize = 0b0001;
const EAST: usize = 0b0010;
const SOUTH: usize = 0b0100;
const WEST: usize = 0b1000;
const DIRS: &[Vec2<i64>] = &[
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

pub fn parse_short(input: &str) -> Vec<(usize, usize)> {
    input
        .trim()
        .lines()
        .map(|line| {
            let mut chars = line.chars();
            let dir = match chars.next().unwrap() {
                'U' => NORTH,
                'R' => EAST,
                'D' => SOUTH,
                'L' => WEST,
                _ => 0,
            };
            chars.next();
            let rest: String = chars.collect();
            let (length, _) = rest.split_once(" ").unwrap();
            return (dir, length.parse().unwrap());
        })
        .collect()
}

pub fn parse_long(input: &str) -> Vec<(usize, usize)> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (_, color) = line.split_once('#').unwrap();
            let dir = match color.chars().nth(5).unwrap() {
                '0' => EAST,
                '1' => SOUTH,
                '2' => WEST,
                '3' => NORTH,
                _ => 0,
            };
            let length = usize::from_str_radix(&color[..5], 16).unwrap();
            return (dir, length);
        })
        .collect()
}

pub fn pretty_dig(commands: Vec<(usize, usize)>) -> Vec<Vec<char>> {
    let mut curr = Vec2(0, 0);
    let mut trench = vec![];
    for (dir, length) in commands.into_iter() {
        for _ in 0..length {
            curr = curr + DIRS[dir];
            trench.push((curr, dir))
        }
    }
    let top_left = trench.iter().fold(Vec2(0, 0), |a, (b, _)| {
        Vec2(cmp::min(a.0, b.0), cmp::min(a.1, b.1))
    });
    let bottom_right = trench.iter().fold(Vec2(0, 0), |a, (b, _)| {
        Vec2(cmp::max(a.0, b.0), cmp::max(a.1, b.1))
    });
    let Vec2(m, n) = bottom_right - top_left + Vec2(1, 1);

    let mut map = vec![vec!['.'; n as usize]; m as usize];
    trench = trench
        .iter()
        .map(|&(pos, dir)| {
            let abs = pos - top_left;
            map[abs.0 as usize][abs.1 as usize] = match dir {
                NORTH => '^',
                EAST => '>',
                SOUTH => 'v',
                WEST => '<',
                _ => '.',
            };
            (abs, dir)
        })
        .collect();

    println!();
    let (m, n) = size(&map);
    let map: Vec<Vec<char>> = (0..m)
        .map(|i| {
            let mut inside = false;
            (0..n)
                .map(|j| {
                    let char = map[i][j];
                    if char == '.' {
                        return if inside { '#' } else { '.' };
                    }
                    match char {
                        '^' => inside = true,
                        'v' => inside = false,
                        '>' => {
                            let curr = trench
                                .iter()
                                .position(|(a, _)| a.0 as usize == i && a.1 as usize == j)
                                .unwrap();
                            let next = trench[(curr + 1) % trench.len()].0;
                            let diff = next - trench[curr].0;
                            if diff == DIRS[SOUTH] {
                                inside = false;
                            } else if diff == DIRS[NORTH] {
                                inside = true;
                            };
                        }
                        _ => {}
                    };
                    char
                })
                .collect()
        })
        .collect();
    map
}

struct Column {
    col: i64,
    min: i64,
    max: i64,
    up: bool,
    index: usize,
}

fn is_edge((a, b): (&Column, Option<&Column>), row: i64, len: usize) -> bool {
    if b.is_none() {
        return true;
    }
    let b = b.unwrap();

    if a.index == (b.index + 2) % len && row == a.min && row == b.min {
        return false;
    }

    if (a.index + 2) % len == b.index && row == a.max && row == b.max {
        return false;
    }

    return !a.up && b.up;
}

pub fn count_dig(commands: Vec<(usize, usize)>) -> usize {
    let mut curr = Vec2(0, 0);
    let mut trench = vec![];
    let len = commands.len();
    for (index, (dir, length)) in commands.into_iter().enumerate() {
        let next = curr + length as i64 * DIRS[dir];
        if dir == SOUTH {
            trench.push(Column {
                col: curr.1,
                min: curr.0,
                max: next.0,
                up: false,
                index,
            });
        } else if dir == NORTH {
            trench.push(Column {
                col: curr.1,
                min: next.0,
                max: curr.0,
                up: true,
                index,
            });
        }
        curr = next;
    }
    let (i, j) = trench.iter().fold((0, 0), |(i, j), col| {
        (cmp::min(i, col.min), cmp::min(j, col.col))
    });

    trench = trench
        .into_iter()
        .map(|col| Column {
            col: col.col - j,
            min: col.min - i,
            max: col.max - i,
            ..col
        })
        .collect();
    trench.sort_unstable_by_key(|c| c.col);

    let m = trench.iter().map(|c| c.max).max().unwrap() + 1;
    let mut sum = 0;
    for row in 0..m {
        let mut cols = trench.iter().filter(|c| c.min <= row && row <= c.max);
        let mut start = cols.next();
        while start.is_some() {
            let mut curr = (start.unwrap(), cols.next());

            while !is_edge(curr, row, len) {
                curr = (curr.1.unwrap(), cols.next())
            }
            sum += curr.0.col - start.unwrap().col + 1;
            start = curr.1;
        }
    }
    return sum as usize;
}

pub fn part_one(input: &str) -> usize {
    count_dig(parse_short(input))
}

pub fn part_two(input: &str) -> usize {
    count_dig(parse_long(input))
}
