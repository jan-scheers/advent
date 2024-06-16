use std::collections::HashSet;
pub const INPUT: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

pub fn parse(input: &str) -> impl Iterator<Item = Vec<u8>> + '_ {
    input.trim().lines().map(|line| {
        line.as_bytes()
            .into_iter()
            .map(|b| {
                if b - b'A' > 26 {
                    b - b'a' + 1
                } else {
                    b - b'A' + 27
                }
            })
            .collect()
    })
}

pub fn part_one(input: &str) -> usize {
    parse(input)
        .map(|line| {
            let (a, b) = (&line[..line.len() / 2], &line[line.len() / 2..]);
            *a.iter().find(|p| b.contains(*p)).unwrap() as usize
        })
        .sum()
}

pub fn part_two(input: &str) -> usize {
    parse(input)
        .collect::<Vec<_>>()
        .chunks(3)
        .map(|chunk| {
            let trio: Vec<HashSet<u8>> = chunk
                .into_iter()
                .map(|elf| HashSet::from_iter(elf.iter().map(|u| *u)))
                .collect();
            *trio[0]
                .intersection(&trio[1])
                .filter(|&u| trio[2].contains(u))
                .next()
                .unwrap() as usize
        })
        .sum()
}
