pub const INPUT: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";

const RANKS1: &[char] = &[
    '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A',
];

const RANKS2: &[char] = &[
    'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A',
];

pub fn part_one(input: &str) -> usize {
    let mut hands = input
        .trim()
        .lines()
        .map(|line| {
            let (hand, bid) = line.split_once(" ").unwrap();
            let bid = bid.parse::<usize>().unwrap();

            let mut count = vec![0; 13];
            let hand: Vec<usize> = hand
                .chars()
                .map(|c| {
                    let r = RANKS1.iter().enumerate().find(|(_, r)| c == **r).unwrap().0;
                    count[r] += 1;
                    return r;
                })
                .collect();

            count = count.into_iter().filter(|c| *c > 0).collect();
            count.sort();
            count.reverse();
            return ((count, hand), bid);
        })
        .collect::<Vec<_>>();
    hands.sort();
    return hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) * bid)
        .sum();
}

pub fn part_two(input: &str) -> usize {
    let mut hands = input
        .trim()
        .lines()
        .map(|line| {
            let (hand, bid) = line.split_once(" ").unwrap();
            let bid = bid.parse::<usize>().unwrap();

            let mut count = vec![0; 13];
            let hand: Vec<usize> = hand
                .chars()
                .map(|c| {
                    let r = RANKS2.iter().enumerate().find(|(_, r)| c == **r).unwrap().0;
                    count[r] += 1;
                    return r;
                })
                .collect();
            let jokers = count[0];
            count = count[1..]
                .iter()
                .filter_map(|c| if *c > 0 { Some(*c) } else { None })
                .collect();
            count.sort();
            count.reverse();
            if count.len() > 0 {
                count[0] += jokers;
            } else {
                count = vec![jokers]
            }
            return ((count, hand), bid);
        })
        .collect::<Vec<_>>();
    hands.sort();
    return hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) * bid)
        .sum();
}
