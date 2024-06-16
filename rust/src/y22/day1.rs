pub const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

pub fn parse(input: &str) -> Vec<Vec<i64>> {
    input
        .trim()
        .split("\n\n")
        .map(|s| s.lines().map(|l| l.parse().unwrap()).collect())
        .collect()
}

pub fn part_one(input: &str) -> i64 {
    let elves = parse(input);
    elves
        .into_iter()
        .map(|elf| elf.into_iter().sum())
        .max()
        .unwrap()
}

pub fn part_two(input: &str) -> i64 {
    let mut elves: Vec<i64> = parse(input)
        .into_iter()
        .map(|e| e.into_iter().sum())
        .collect();
    elves.sort_unstable();
    elves[elves.len() - 3..].iter().sum()
}
