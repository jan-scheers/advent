fn parse(input: &str) -> Vec<(i64, i64)> {
    input
        .trim()
        .split(",")
        .map(|line| {
            let (left, right) = line.split_once("-").unwrap();
            (left.parse().unwrap(), right.parse().unwrap())
        })
        .collect()
}

pub fn rule_one(num: &i64) -> bool {
    let digits = num.to_string();
    let digits = digits.as_bytes();
    digits.len() % 2 == 0 && digits[..digits.len() / 2] == digits[digits.len() / 2..]
}

pub fn part_one(input: &str) -> i64 {
    parse(input)
        .iter()
        .map(|(left, right)| (*left..=*right).filter(rule_one).sum::<i64>())
        .sum()
}

pub fn rule_two(num: &i64) -> bool {
    let digits = num.to_string();
    let digits = digits.as_bytes();
    for len in 1..=digits.len() / 2 {
        if digits.len() % len != 0 {
            continue;
        }
        let seq = digits[..len].repeat(digits.len() / len);
        if seq == digits {
            return true;
        }
    }
    false
}

pub fn part_two(input: &str) -> i64 {
    parse(input)
        .iter()
        .map(|(left, right)| (*left..=*right).filter(rule_two).sum::<i64>())
        .sum()
}
