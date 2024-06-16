pub const INPUT: &str = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

extern crate nalgebra as na;

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
pub struct Sensor {
    pub p: na::Vector2<i64>,
    pub b: na::Vector2<i64>,
}

pub fn parse(input: &str) -> Vec<Sensor> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (p, b) = line.split_once(":").unwrap();
            let [p, b] = [p, b]
                .iter()
                .map(|s| {
                    na::Vector2::from_iterator(
                        s.split_once("x=")
                            .unwrap()
                            .1
                            .split(", y=")
                            .map(|d| d.parse().unwrap()),
                    )
                })
                .collect::<Vec<_>>()[..]
            else {
                panic!();
            };
            Sensor { p, b }
        })
        .collect()
}

use std::collections::HashSet;
pub fn part_one(input: &str, row: i64) -> i64 {
    let sensors = parse(input);
    let beacons: HashSet<i64> =
        HashSet::from_iter(sensors.iter().filter(|s| s.b[1] == row).map(|s| s.b[0]));
    row_range(&sensors, row)
        .into_iter()
        .map(|r| r.1 - r.0 + 1 - beacons.iter().filter(|&&b| r.0 <= b && b <= r.1).count() as i64)
        .sum()
}

pub fn part_two(input: &str, search: i64) -> i64 {
    let sensors = parse(input);
    show(&sensors, (2628200, 2628240, 2939023, 2939063));
    (0..search)
        .find_map(|row| {
            let rs = row_range(&sensors, row);
            if rs.len() == 1 {
                None
            } else {
                let col = rs[0].1 + 1;
                println!("{},{}", row, col);
                Some(row + col * 4000000)
            }
        })
        .unwrap()
}

fn show(sensors: &Vec<Sensor>, bd: (i64, i64, i64, i64)) {
    let mut ps = HashSet::new();
    let mut bs = HashSet::new();
    for s in sensors.iter() {
        ps.insert(s.p);
        bs.insert(s.b);
    }
    let (m, n) = ((bd.1 - bd.0 + 1) as usize, (bd.3 - bd.2 + 1) as usize);
    let mat = na::DMatrix::from_row_iterator(
        m,
        n,
        (bd.0..=bd.1).flat_map(|row| {
            let ranges = row_range(&sensors, row);
            (bd.2..=bd.3)
                .map(|col| {
                    let pos = na::Vector2::from([row, col]);
                    if ps.contains(&pos) {
                        return 'S';
                    }
                    if bs.contains(&pos) {
                        return 'B';
                    }
                    if ranges.iter().any(|range| range.0 <= col && col <= range.1) {
                        return '#';
                    }
                    return '.';
                })
                .collect::<Vec<_>>()
        }),
    );

    println!("{}", mat.transpose());
}

fn row_range(sensors: &Vec<Sensor>, row: i64) -> Vec<(i64, i64)> {
    sensors
        .iter()
        .filter_map(|Sensor { p, b }| {
            let range = (b - p).abs().sum() - (p[1] - row).abs();
            if range < 0 {
                None
            } else {
                Some((p[0] - range, p[0] + range))
            }
        })
        .fold(vec![], insert_range)
}

fn insert_range(ranges: Vec<(i64, i64)>, (a, b): (i64, i64)) -> Vec<(i64, i64)> {
    let s = ranges.partition_point(|r| r.1 + 2 <= a);
    let e = ranges.partition_point(|r| r.0 <= b + 1);
    let mut res = ranges[0..s].to_vec();
    if s == e {
        res.push((a, b))
    } else {
        res.push((a.min(ranges[s].0), b.max(ranges[e - 1].1)))
    }
    res.extend_from_slice(&ranges[e..]);
    res
}
