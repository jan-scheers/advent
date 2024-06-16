pub const INPUT: &str = "A Y
B X
C Z";

pub fn parse(input: &str) -> impl Iterator<Item = (i32, i32)> + '_ {
    input.trim().lines().map(|line| {
        let line = line.as_bytes();
        ((line[0] - b'A') as i32, (line[2] - b'X') as i32)
    })
}

pub fn part_one(input: &str) -> i32 {
    parse(input)
        .map(|(opp, you)| 1 + you + (you - opp + 1).rem_euclid(3) * 3)
        .sum()
}

pub fn part_two(input: &str) -> i32 {
    parse(input)
        .map(|(opp, win)| 1 + (opp + win - 1).rem_euclid(3) + win * 3)
        .sum()
}
