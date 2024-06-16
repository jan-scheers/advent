pub const INPUT: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

pub fn parse(input: &str) -> impl Iterator<Item = ((i32, i32), (i32, i32))> + '_ {
    input.trim().lines().map(|line| {
        let mut ranges = line.split(",").map(|r| {
            let mut digits = r.split("-").map(|d| d.parse().unwrap());
            (digits.next().unwrap(), digits.next().unwrap())
        });
        (ranges.next().unwrap(), ranges.next().unwrap())
    })
}

pub fn part_one(input: &str) -> usize {
    parse(input)
        .filter(|(a, b)| a.0 <= b.0 && b.1 <= a.1 || b.0 <= a.0 && a.1 <= b.1)
        .count()
}

pub fn part_two(input: &str) -> usize {
    parse(input)
        .filter(|(a, b)| !(a.1 < b.0 || b.1 < a.0))
        .count()
}
