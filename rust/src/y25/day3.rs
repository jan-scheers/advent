pub const INPUT: &str = "987654321111111
811111111111119
234234234234278
818181911112111
";

fn joltage(line: &str, n: usize) -> usize {
    let bytes = line.as_bytes();
    let mut res: Vec<u8> = vec![];
    let mut start = 0;
    for i in 0..n {
        let (item, index) = bytes[start as usize..=bytes.len() + i - n]
            .iter()
            .enumerate()
            .map(|(i, &b)| (b, -(i as i64)))
            .max()
            .unwrap();
        res.push(item);
        start = start - index + 1
    }
    let joltage = res.into_iter().map(|b| b as char).collect::<String>();
    return joltage.parse::<usize>().unwrap();
}

pub fn part_one(input: &str) -> usize {
    input.trim().lines().map(|line| joltage(line, 2)).sum()
}

pub fn part_two(input: &str) -> usize {
    input.trim().lines().map(|line| joltage(line, 12)).sum()
}
