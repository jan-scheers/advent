pub const INPUT: &str = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3";
use crate::{pretty, Vec2, CLOCK, EAST, NORTH, SOUTH, WEST};

use std::collections::{HashMap, HashSet};
fn area(a: &Vec2<i64>, b: &Vec2<i64>, xs: &HashMap<i64, i64>, ys: &HashMap<i64, i64>) -> i64 {
    let x1 = *xs.get(&a.0).unwrap();
    let x2 = *xs.get(&b.0).unwrap();
    let y1 = *ys.get(&a.1).unwrap();
    let y2 = *ys.get(&b.1).unwrap();
    ((x2 - x1).abs() + 1) * ((y2 - y1).abs() + 1)
}

fn dir(a: &Vec2<i64>, b: &Vec2<i64>) -> usize {
    let d = Vec2((b.0 - a.0).signum(), (b.1 - a.1).signum());
    CLOCK.iter().position(|c| c == &d).unwrap()
}

fn vertex(a: &Vec2<i64>, b: &Vec2<i64>) -> Vec<Vec2<usize>> {
    match dir(a, b) {
        EAST => (a.1..=b.1)
            .map(|x| Vec2(a.0 as usize, x as usize))
            .collect(),
        WEST => (b.1..=a.1)
            .rev()
            .map(|x| Vec2(a.0 as usize, x as usize))
            .collect(),
        SOUTH => (a.0..=b.0)
            .map(|y| Vec2(y as usize, a.1 as usize))
            .collect(),
        NORTH => (b.0..=a.0)
            .rev()
            .map(|y| Vec2(y as usize, a.1 as usize))
            .collect(),
        _ => panic!("Invalid direction"),
    }
}

fn parse(input: &str) -> (Vec<Vec2<i64>>, HashMap<i64, i64>, HashMap<i64, i64>) {
    let mut xs: HashSet<i64> = HashSet::new();
    let mut ys: HashSet<i64> = HashSet::new();
    let points: Vec<Vec2<i64>> = input
        .trim()
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(",").unwrap();
            let p = Vec2(x.parse().unwrap(), y.parse().unwrap());
            xs.insert(p.0);
            ys.insert(p.1);
            p
        })
        .collect();
    let mut xs = xs.into_iter().collect::<Vec<_>>();
    xs.sort_unstable();
    let xs = xs
        .into_iter()
        .enumerate()
        .map(|(i, x)| (x, i as i64 + 1))
        .collect::<HashMap<_, _>>();
    let mut ys = ys.into_iter().collect::<Vec<_>>();
    ys.sort_unstable();
    let ys = ys
        .into_iter()
        .enumerate()
        .map(|(i, y)| (y, i as i64 + 1))
        .collect::<HashMap<_, _>>();
    let points: Vec<Vec2<i64>> = points
        .into_iter()
        .map(|p| Vec2(*xs.get(&p.0).unwrap(), *ys.get(&p.1).unwrap()))
        .collect();
    let xs = xs
        .into_iter()
        .map(|(x, i)| (i, x))
        .collect::<HashMap<_, _>>();
    let ys = ys
        .into_iter()
        .map(|(y, i)| (i, y))
        .collect::<HashMap<_, _>>();
    (points, xs, ys)
}

pub fn part_one(input: &str) -> i64 {
    let (points, xs, ys) = parse(input);
    (0..points.len())
        .flat_map(|i| {
            (0..i)
                .map(|j| area(&points[i], &points[j], &xs, &ys))
                .collect::<Vec<_>>()
        })
        .max()
        .unwrap()
}

pub fn part_two(input: &str) -> i64 {
    let (points, xs, ys) = parse(input);
    let mut clockwise = 0;
    let mut anticlockwise = 0;
    for i in 0..points.len() {
        let j = (i + 1) % points.len();
        let k = (i + 2) % points.len();
        let d1 = dir(&points[i], &points[j]);
        let d2 = dir(&points[j], &points[k]);
        if CLOCK[(d1 + 1) % 4] == CLOCK[d2] {
            clockwise += 1;
        } else if CLOCK[(d1 + 3) % 4] == CLOCK[d2] {
            anticlockwise += 1;
        } else {
            panic!("Invalid direction");
        }
    }
    println!("Clockwise: {}, Anticlockwise: {}", clockwise, anticlockwise);
    let mut grid = vec![vec!['.'; xs.len() + 2]; ys.len() + 2];
    for i in 0..points.len() {
        let j = (i + 1) % points.len();
        let (a, b) = (&points[i], &points[j]);
        for p in vertex(a, b) {
            grid[p.1][p.0] = 'O';
        }
    }
    let rot = if clockwise > anticlockwise { 1 } else { 3 };
    for i in 0..points.len() {
        let j = (i + 1) % points.len();
        let (a, b) = (&points[i], &points[j]);
        let cross = CLOCK[(dir(a, b) + rot) % 4];
        for p in vertex(a, b) {
            let mut ptr = Vec2(p.0 as i64, p.1 as i64) + cross;
            while grid[ptr.1 as usize][ptr.0 as usize] != 'O' {
                grid[ptr.1 as usize][ptr.0 as usize] = '#';
                ptr = ptr + cross;
            }
        }
    }
    let mut best = 0;
    let mut best_vec = vec![];
    for i in 0..points.len() {
        for j in 0..i {
            let (a, c) = (&points[i], &points[j]);
            if a.0 == c.0 || a.1 == c.1 {
                continue;
            }

            let ps = vec![a.clone(), Vec2(a.0, c.1), c.clone(), Vec2(c.0, a.1)];
            let invalid = (0..ps.len()).any(|p| {
                let (a, b) = (ps[p], ps[(p + 1) % ps.len()]);
                vertex(&a, &b).into_iter().any(|p| grid[p.1][p.0] == '.')
            });

            if !invalid {
                let area = area(&a, &c, &xs, &ys);
                if area > best {
                    best = area;
                    best_vec = ps;
                }
            }
        }
    }
    for i in 0..best_vec.len() {
        let (a, b) = (best_vec[i], best_vec[(i + 1) % best_vec.len()]);
        for p in vertex(&a, &b) {
            grid[p.1 as usize][p.0 as usize] = 'X';
        }
    }
    pretty(&grid);
    best
}
