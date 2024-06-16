use std::collections::HashSet;

pub const INPUT: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

fn parse(input: &str) -> Vec<(HashSet<usize>, Vec<usize>)> {
    input
        .trim()
        .split("\n")
        .filter_map(|line| {
            let Some((_, line)) = line.split_once(":") else {
                return None;
            };
            let Some((win, have)) = line.split_once("|") else {
                return None;
            };
            let mut card = [have, win]
                .iter()
                .map(|card| {
                    card.trim()
                        .split_whitespace()
                        .map(|num| num.parse::<usize>().unwrap())
                        .collect()
                })
                .collect::<Vec<Vec<_>>>();
            return Some((
                HashSet::from_iter(card.pop().unwrap().into_iter()),
                card.pop().unwrap(),
            ));
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    parse(input).iter().fold(0, |sum, (win, have)| {
        let count = have.iter().fold(
            -1,
            |count, n| if win.contains(n) { count + 1 } else { count },
        );
        return if count < 0 { sum } else { sum + (1 << count) };
    })
}

pub fn part_two(input: &str) -> usize {
    let input = parse(input);
    let mut cards = vec![1; input.len()];
    for (i, (win, have)) in input.iter().enumerate() {
        let count = have.iter().fold(
            0,
            |count, n| if win.contains(n) { count + 1 } else { count },
        );
        let curr = cards[i];
        for i in i + 1..i + 1 + count {
            cards[i] += curr;
        }
    }
    cards.iter().sum()
}
