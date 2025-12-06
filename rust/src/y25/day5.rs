pub const INPUT: &str = "3-5
10-14
16-20
12-18

1
5
8
11
17
32";

type Range = (i64, i64);

pub fn parse(input: &str) -> (Vec<Range>, Vec<i64>) {
    let (ranges, values) = input.trim().split_once("\n\n").unwrap();
    let ranges = ranges
        .lines()
        .map(|line| {
            let (left, right) = line.split_once("-").unwrap();
            (left.parse().unwrap(), right.parse().unwrap())
        })
        .collect();
    let values = values.lines().map(|line| line.parse().unwrap()).collect();
    (join_ranges(ranges), values)
}

pub fn join_ranges(mut ranges: Vec<Range>) -> Vec<Range> {
    ranges.sort();
    let mut result = Vec::new();
    for range @ (next_start, next_end) in ranges.iter() {
        let Some(last @ (last_start, last_end)) = result.pop() else {
            result.push(*range);
            continue;
        };
        if last_end + 1 < *next_start {
            result.push(last);
            result.push(*range);
        } else {
            result.push((last_start, last_end.max(*next_end)));
        }
    }
    result
}

pub fn in_range(ranges: &Vec<Range>, value: i64) -> bool {
    ranges.iter().any(|(a, b)| *a <= value && value <= *b)
}

pub fn part_one(input: &str) -> usize {
    let (ranges, values) = parse(input);
    values
        .iter()
        .filter(|&value| in_range(&ranges, *value))
        .count()
}

pub fn part_two(input: &str) -> i64 {
    let (ranges, _) = parse(input);
    ranges
        .into_iter()
        .fold(0, |acc, (start, end)| acc + end - start + 1)
}
