pub const INPUT: &str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45";

pub fn parse(input: &str) -> Vec<Vec<i64>> {
    input
        .trim()
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse().unwrap())
                .collect()
        })
        .collect()
}

fn deltas(delta: &Vec<i64>) -> Vec<Vec<i64>> {
    let mut deltas = vec![delta.clone()];
    loop {
        let last = &deltas[deltas.len() - 1];
        let mut delta = Vec::with_capacity(last.len() - 1);
        for i in 0..last.len() - 1 {
            delta.push(last[i + 1] - last[i])
        }
        if delta.iter().all(|d| *d == 0) {
            return deltas;
        }
        deltas.push(delta);
    }
}

pub fn part_one(input: &str) -> i64 {
    parse(input)
        .iter()
        .map(|row| {
            deltas(row)
                .iter()
                .map(|delta| delta[delta.len() - 1])
                .sum::<i64>()
        })
        .sum()
}

pub fn part_two(input: &str) -> i64 {
    parse(input)
        .iter()
        .map(|row| {
            deltas(row)
                .iter()
                .rev()
                .fold(0, |acc, delta| delta[0] - acc)
        })
        .sum()
}
