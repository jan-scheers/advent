use std::cmp::{max, min};

pub const INPUT: &str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";

pub fn parse(input: &str) -> (Vec<i64>, Vec<Vec<(i64, i64, i64)>>) {
    let mut maps = input.trim().split("\n\n");

    let Some((_, seeds)) = maps.next().and_then(|s| s.split_once(": ")) else {
        panic!("no seeds")
    };

    let seeds: Vec<i64> = seeds
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();

    let maps: Vec<_> = maps
        .map(|map| {
            let mut lines = map.lines();
            lines.next();
            let mut full: Vec<_> = lines
                .map(|line| {
                    let [dst, src, len] = line
                        .split_whitespace()
                        .map(|i| i.parse().unwrap())
                        .collect::<Vec<i64>>()[..]
                    else {
                        panic!();
                    };
                    (src, src + len, dst - src)
                })
                .collect();
            full.sort();
            let (mut maps, mut prev) = (vec![], i64::MIN);
            for a in full.into_iter() {
                maps.push((prev, a.0, 0));
                maps.push(a);
                prev = a.1;
            }
            maps.push((prev, i64::MAX, 0));
            return maps;
        })
        .collect();
    return (seeds, maps);
}

fn calculate(seeds: Vec<(i64, i64)>, maps: Vec<Vec<(i64, i64, i64)>>) -> i64 {
    maps.iter()
        .fold(seeds, |seeds, map| {
            seeds
                .into_iter()
                .flat_map(|(st, en)| {
                    map.iter()
                        .filter_map(move |&(src, end, dlt)| {
                            return if en < src || end < st {
                                None
                            } else {
                                Some((max(src, st) + dlt, min(end, en) + dlt))
                            };
                        })
                        .filter(|(a, b)| a != b)
                })
                .collect()
        })
        .into_iter()
        .min()
        .unwrap()
        .0
}

pub fn part_one(input: &str) -> i64 {
    let (seeds, maps) = parse(input);
    let seeds = seeds.iter().map(|&s| (s, s + 1)).collect::<Vec<_>>();
    calculate(seeds, maps)
}

pub fn part_two(input: &str) -> i64 {
    let (seeds, maps) = parse(input);
    let seeds: Vec<_> = (0..seeds.len() / 2)
        .map(|i| (seeds[i * 2], seeds[i * 2] + seeds[i * 2 + 1]))
        .collect();
    calculate(seeds, maps)
}
