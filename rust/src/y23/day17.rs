use std::collections::HashMap;

use crate::{pretty, size, Vec2};

pub const INPUT: &str = "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533";

pub fn parse(input: &str) -> Vec<Vec<usize>> {
    input
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

const DIRS: &[Vec2<i32>] = &[Vec2(-1, 0), Vec2(0, 1), Vec2(1, 0), Vec2(0, -1)];

pub fn ucs(map: &Vec<Vec<usize>>) -> HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> {
    let (m, n) = size(map);
    let (m, n) = (m as i32, n as i32);
    let mut frontier: HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> =
        HashMap::from_iter((0..4).map(|i| ((0, 0, i), ((0, 0, i), 0))));
    let mut solution: HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> = HashMap::new();

    while let Some(head) = frontier.iter().min_by(|(_, (_, a)), (_, (_, b))| a.cmp(b)) {
        let (key, value) = (*head.0, *head.1);
        frontier.remove(&key);
        solution.insert(key, value);

        for (face, delta) in DIRS.iter().enumerate() {
            if face % 2 == key.2 % 2 {
                continue;
            }

            let mut cost = 0;
            for jump in 1..4 {
                let next = Vec2(key.0, key.1) + jump * *delta;
                if next.0 < 0 || next.0 >= m || next.1 < 0 || next.1 >= n {
                    break;
                }

                cost += map[next.0 as usize][next.1 as usize];
                let cand = (next.0, next.1, face);
                if solution.contains_key(&cand) {
                    continue;
                }
                let curr = frontier.get(&cand);
                let candcost = value.1 + cost;
                if curr.is_none() || curr.unwrap().1 > candcost {
                    frontier.insert(cand, (key, candcost));
                }
            }
        }
    }

    solution
}

pub fn ucs2(map: &Vec<Vec<usize>>) -> HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> {
    let (m, n) = size(map);
    let (m, n) = (m as i32, n as i32);
    let mut frontier: HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> =
        HashMap::from_iter((0..4).map(|i| ((0, 0, i), ((0, 0, i), 0))));
    let mut solution: HashMap<(i32, i32, usize), ((i32, i32, usize), usize)> = HashMap::new();

    while let Some(head) = frontier.iter().min_by(|(_, (_, a)), (_, (_, b))| a.cmp(b)) {
        let (key, value) = (*head.0, *head.1);
        frontier.remove(&key);
        solution.insert(key, value);

        for (face, delta) in DIRS.iter().enumerate() {
            if face % 2 == key.2 % 2 {
                continue;
            }

            let mut cost = 0;
            for jump in 1..11 {
                let next = Vec2(key.0, key.1) + jump * *delta;
                if next.0 < 0 || next.0 >= m || next.1 < 0 || next.1 >= n {
                    break;
                }
                cost += map[next.0 as usize][next.1 as usize];
                if jump < 4 {
                    continue;
                }
                let cand = (next.0, next.1, face);
                if solution.contains_key(&cand) {
                    continue;
                }
                let curr = frontier.get(&cand);
                let candcost = value.1 + cost;
                if curr.is_none() || curr.unwrap().1 > candcost {
                    frontier.insert(cand, (key, candcost));
                }
            }
        }
    }

    solution
}

pub fn draw(
    map: &Vec<Vec<usize>>,
    solution: &HashMap<(i32, i32, usize), ((i32, i32, usize), usize)>,
    key: &(i32, i32, usize),
) {
    let mut draw: Vec<Vec<char>> = map
        .iter()
        .map(|row| row.iter().map(|_| ' ').collect())
        .collect();

    let mut node = solution.get_key_value(key).unwrap();
    while !(node.0 .0 == 0 && node.0 .1 == 0) {
        let (&(i, j, d), (prev, _)) = node;
        let c = match d {
            0 => '^',
            1 => '>',
            2 => 'v',
            _ => '<',
        };
        let mut pos = Vec2(i, j);
        for _ in 0..(i.abs_diff(prev.0) + j.abs_diff(prev.1)) {
            draw[pos.0 as usize][pos.1 as usize] = c;
            pos = pos - DIRS[d];
        }
        node = solution.get_key_value(prev).unwrap();
    }
    pretty(&draw);
}

pub fn part_one(input: &str) -> usize {
    let map = parse(input);
    let solution = ucs(&map);
    let best = solution
        .iter()
        .filter(|(k, _)| k.0 == map.len() as i32 - 1 && k.1 == map[0].len() as i32 - 1)
        .min_by_key(|(_, (_, cost))| cost)
        .unwrap();
    draw(&map, &solution, best.0);
    best.1 .1
}

pub fn part_two(input: &str) -> usize {
    let map = parse(input);
    let solution = ucs2(&map);
    let best = solution
        .iter()
        .filter(|(k, _)| k.0 == map.len() as i32 - 1 && k.1 == map[0].len() as i32 - 1)
        .min_by_key(|(_, (_, cost))| cost)
        .unwrap();
    draw(&map, &solution, best.0);
    best.1 .1
}
