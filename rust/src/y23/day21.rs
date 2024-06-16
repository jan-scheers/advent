use std::collections::HashSet;

use crate::{size, Vec2, DIRS};

pub const INPUT: &str = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........";

#[derive(Hash, Debug, PartialEq, Eq, Clone, Default)]
struct Diamond {
    wh_even: i64,
    wh_odd: i64,
    bl_center: i64,
    wh_border: Vec<i64>,
    bl_border: Vec<i64>,
    corners: Vec<i64>,
}

impl Diamond {
    fn new() -> Self {
        Diamond {
            wh_even: 0,
            wh_odd: 0,
            bl_center: 0,
            wh_border: vec![],
            bl_border: vec![],
            corners: vec![],
        }
    }
}

pub fn parse(input: &str) -> (Vec2<i64>, Vec<Vec<bool>>) {
    let mut start = Vec2(-1, -1);
    let map = input
        .trim()
        .lines()
        .enumerate()
        .map(|(i, line)| {
            line.chars()
                .enumerate()
                .map(|(j, c)| match c {
                    '.' => true,
                    'S' => {
                        start = Vec2(i as i64, j as i64);
                        true
                    }
                    _ => false,
                })
                .collect()
        })
        .collect();
    (start, map)
}

pub fn part_one(input: &str) -> usize {
    let nsteps = 64;
    let (start, map) = parse(input);
    let (m, n) = size(&map);
    let (m, n) = (m as i64, n as i64);
    let mut frontier: HashSet<_> = HashSet::from([start]);
    for _ in 0..nsteps {
        let mut next: HashSet<Vec2<i64>> = HashSet::new();
        frontier.iter().for_each(|square| {
            next.extend(DIRS.iter().filter_map(|d| {
                let step = *square + *d;
                if 0 <= step.0
                    && step.0 < m
                    && 0 <= step.1
                    && step.1 < n
                    && map[step.0 as usize][step.1 as usize]
                {
                    Some(step)
                } else {
                    None
                }
            }))
        });
        frontier = next;
    }
    frontier.len()
}

pub fn part_two(input: &str) -> i64 {
    let nsteps = 26501365;
    let (start, map) = parse(input);
    let m = map.len() as i64;
    let mut frontier: HashSet<_> = HashSet::from([start]);
    let mut diam = Diamond::new();
    for st in 1.. {
        let mut next: HashSet<Vec2<i64>> = HashSet::new();
        frontier.iter().for_each(|square| {
            next.extend(DIRS.iter().filter_map(|d| {
                let step = *square + *d;
                if map[step.0.rem_euclid(m) as usize][step.1.rem_euclid(m) as usize] {
                    Some(step)
                } else {
                    None
                }
            }))
        });
        frontier = next;
        let n = (st - m / 2 - 1) / m;
        if n == 1 {
            let (white, black) = count(&frontier, m, st);
            diam.wh_even = white[2][2 + (st + 1) as usize % 2];
            diam.wh_odd = white[2][2 + st as usize % 2];
            diam.bl_center = black[1][1] + black[1][2];
            diam.wh_border
                .push(white[1][1] + white[3][1] + white[1][3] + white[3][3]);
            diam.bl_border
                .push(black[0][1] + black[0][2] + black[3][1] + black[3][2]);
            diam.corners
                .push(white[0][2] + white[2][0] + white[4][2] + white[2][4]);
        }
        if n > 1 {
            break;
        }
    }
    let (m, half) = (m as usize, m as usize / 2);
    let formula = |st: usize| {
        let n = (st - half - 1).div_euclid(m) as i64;
        let i = (st - half - 1).rem_euclid(m);
        (n + 1) * n * diam.bl_center
            + diam.wh_even * (n + (i as i64) % 2).pow(2)
            + diam.wh_odd * (n + (i as i64 + 1) % 2).pow(2)
            + n * diam.wh_border[i]
            + (n + 1) * diam.bl_border[i]
            + diam.corners[i]
    };
    formula(nsteps)
}

fn count(frontier: &HashSet<Vec2<i64>>, m: i64, st: i64) -> (Vec<Vec<i64>>, Vec<Vec<i64>>) {
    let half = m / 2;
    let bottom = (half - st).div_euclid(m);
    let to_coord = |v: &Vec2<i64>| {
        let delta = (v.0.rem_euclid(m) - half).abs() + (v.1.rem_euclid(m) - half).abs();
        let inside = delta <= m / 2;
        (
            inside,
            ((if inside { v.0 } else { v.0 - half }).div_euclid(m) - bottom) as usize,
            ((if inside { v.1 } else { v.1 - half }).div_euclid(m) - bottom) as usize,
        )
    };
    let n = ((half + st).div_euclid(m) - bottom + 1) as usize;
    let mut incnt = vec![vec![0; n]; n];
    let mut outcnt = vec![vec![0; n - 1]; n - 1];
    for v in frontier {
        let (inside, i, j) = to_coord(v);
        if inside {
            incnt[i][j] += 1;
        } else {
            outcnt[i][j] += 1;
        }
    }
    (incnt, outcnt)
}
