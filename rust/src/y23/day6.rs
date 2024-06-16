pub const INPUT: &str = "
Time:      7  15   30
Distance:  9  40  200";

fn race((time, dist): (u64, u64)) -> u64 {
    (0..time).fold(0, |cnt, spd| {
        if (spd * (time - spd)) > dist {
            cnt + 1
        } else {
            cnt
        }
    })
}

pub fn part_one(input: &str) -> u64 {
    parse(input).into_iter().map(race).product()
}

pub fn parse(input: &str) -> Vec<(u64, u64)> {
    let mut lines = input.trim().lines().map(|line| {
        line.split_once(":")
            .unwrap()
            .1
            .split_whitespace()
            .map(|n| n.parse().unwrap())
            .collect::<Vec<u64>>()
    });
    std::iter::zip(lines.next().unwrap(), lines.next().unwrap()).collect()
}

pub fn part_two(input: &str) -> u64 {
    race(parse2(input))
}

pub fn parse2(input: &str) -> (u64, u64) {
    let mut lines = input.trim().lines().map(|line| {
        line.split_once(":")
            .unwrap()
            .1
            .split_whitespace()
            .collect::<String>()
            .parse::<u64>()
            .unwrap()
    });
    return (lines.next().unwrap(), lines.next().unwrap());
}
