use std::collections::HashMap;

use crate::lcm;
pub const INPUT: &str = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";

pub const INPUT2: &str = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";

pub const INPUT3: &str = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";

pub fn parse(input: &str) -> (Vec<char>, HashMap<String, (String, String)>) {
    let mut lines = input.trim().lines();
    let seq = lines.next().unwrap().chars().collect();
    lines.next();
    let map = lines
        .map(|line| {
            let a: String = line[0..3].chars().collect();
            let b: String = line[7..10].chars().collect();
            let c: String = line[12..15].chars().collect();
            return (a, (b, c));
        })
        .collect();
    return (seq, map);
}

fn follow(node: &String, seq: &Vec<char>, map: &HashMap<String, (String, String)>) -> usize {
    let mut node = &node.clone();
    let mut count = 0;
    while node.as_bytes()[2] as char != 'Z' {
        let dir = seq[count % seq.len()];
        let (left, right) = map.get(node).unwrap();
        node = if dir == 'L' { left } else { right };
        count += 1;
    }
    return count;
}

pub fn part_one(input: &str) -> usize {
    let (seq, map) = parse(input);
    let mut node = &String::from("AAA");
    let mut count = 0;
    while node != "ZZZ" {
        let dir = seq[count % seq.len()];
        let (left, right) = map.get(node).unwrap();
        node = if dir == 'L' { left } else { right };
        count += 1;
    }
    return count;
}

pub fn part_two(input: &str) -> usize {
    let (seq, map) = parse(input);
    let lengths: Vec<_> = map
        .keys()
        .filter_map(|node| {
            if node.as_bytes()[2] as char == 'A' {
                Some(follow(node, &seq, &map))
            } else {
                None
            }
        })
        .collect();
    dbg!(&lengths);
    lcm(&lengths)
}
