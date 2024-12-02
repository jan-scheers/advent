use std::collections::HashSet;

use crate::{Vec2, CLOCK, EAST, NORTH, SOUTH, WEST};

pub const INPUT: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

pub fn parse(input: &str) -> Vec<(Vec2<i64>, usize)> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (cmd, steps) = line.split_once(" ").unwrap();
            (
                CLOCK[match cmd {
                    "U" => NORTH,
                    "R" => EAST,
                    "D" => SOUTH,
                    _ => WEST,
                }],
                steps.parse().unwrap(),
            )
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let mut pos = HashSet::new();
    parse(input)
        .into_iter()
        .fold((Vec2(0, 0), Vec2(0, 0)), |(head, tail), (cmd, steps)| {
            (0..steps).fold((head, tail), |(mut head, mut tail), _| {
                head = head + cmd;
                tail = match tail - head {
                    Vec2(-2, _) => head + Vec2(-1, 0),
                    Vec2(2, _) => head + Vec2(1, 0),
                    Vec2(_, -2) => head + Vec2(0, -1),
                    Vec2(_, 2) => head + Vec2(0, 1),
                    _ => tail,
                };
                pos.insert(tail);
                (head, tail)
            })
        });
    pos.len()
}

pub fn part_two(input: &str) -> usize {
    let mut pos = HashSet::new();
    parse(input)
        .into_iter()
        .fold(vec![Vec2(0, 0); 10], |snake, (cmd, steps)| {
            (0..steps).fold(snake, |mut snake, _| {
                let mut last: Option<Vec2<i64>> = None;
                snake = snake
                    .into_iter()
                    .map(|curr| {
                        let moved = last
                            .map(|prev| {
                                let Vec2(dx, dy) = prev - curr;
                                if dx.abs() > 1 || dy.abs() > 1 {
                                    Vec2(dx.signum(), dy.signum())
                                } else {
                                    Vec2(0, 0)
                                }
                            })
                            .unwrap_or(cmd)
                            + curr;
                        last = Some(moved);
                        moved
                    })
                    .collect();
                pos.insert(last.unwrap());
                snake
            })
        });
    pos.len()
}
