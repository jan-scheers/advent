pub const INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
use std::collections::{hash_map::RandomState, HashSet};

pub fn part_one(input: &str) -> usize {
    input
        .as_bytes()
        .windows(4)
        .position(|w| HashSet::<u8, RandomState>::from_iter(w.iter().map(|u| *u)).len() == w.len())
        .unwrap()
        + 4
}

pub fn part_two(input: &str) -> usize {
    input
        .as_bytes()
        .windows(14)
        .position(|w| HashSet::<u8, RandomState>::from_iter(w.iter().map(|u| *u)).len() == w.len())
        .unwrap()
        + 14
}
