use num::Integer;
extern crate nalgebra as na;

pub const INPUT: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

fn parse(input: &str) -> Vec<Option<i32>> {
    input
        .trim()
        .lines()
        .map(|line| match &line[..4] {
            "addx" => Some(line[5..].parse().unwrap()),
            _ => None,
        })
        .collect()
}

pub fn part_one(input: &str) -> i32 {
    parse(input)
        .into_iter()
        .flat_map(|add| {
            if let Some(v) = add {
                vec![0, v]
            } else {
                vec![0]
            }
        })
        .enumerate()
        .fold((1, 0), |(x, mut out), (i, v)| {
            let cycle = (i + 1) as i32;
            if cycle % 40 == 20 {
                println!("cycle {} {}", cycle, x);
                out += cycle * x
            }
            (x + v, out)
        })
        .1
}

pub fn part_two(input: &str) {
    let mut a = na::DMatrix::from_fn(6, 40, |_, _| '.');
    parse(input)
        .into_iter()
        .flat_map(|add| {
            if let Some(v) = add {
                vec![0, v]
            } else {
                vec![0]
            }
        })
        .enumerate()
        .fold(1, |x, (i, v)| {
            let (r, c) = i.div_rem(&40);
            if x - 1 <= c as i32 && c as i32 <= x + 1 {
                a[(r, c)] = '#'
            }
            x + v
        });
    println!("{}", a);
}
