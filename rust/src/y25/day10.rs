pub const INPUT: &str = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}";
use faer::{Col, ColRef, Mat};
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug)]
struct Input {
    lights: u32,
    buttons: Vec<u32>,
    joltage: Vec<u32>,
}

fn parse_line(line: &str) -> Input {
    let (lights, line) = line.trim().split_once(' ').unwrap();
    let (buttons, joltage) = line.rsplit_once(' ').unwrap();
    let lights: Vec<u32> = lights
        .chars()
        .filter_map(|c| match c {
            '.' => Some(0_u32),
            '#' => Some(1_u32),
            _ => None,
        })
        .collect();
    let buttons: Vec<u32> = buttons
        .split(' ')
        .map(|b| {
            b[1..b.len() - 1]
                .split(',')
                .fold(vec![0; lights.len()], |mut acc, b| {
                    acc[b.parse::<usize>().unwrap()] = 1;
                    acc
                })
        })
        .map(to_bits)
        .collect();

    let joltage: Vec<u32> = joltage[1..joltage.len() - 1]
        .split(',')
        .map(|j| j.parse().unwrap())
        .collect();
    Input {
        lights: to_bits(lights),
        buttons,
        joltage,
    }
}

fn to_bits(lights: Vec<u32>) -> u32 {
    lights.iter().enumerate().map(|(i, &l)| (1 << i) * l).sum()
}

fn from_bits(bits: &u32, len: usize) -> String {
    (0..len)
        .map(|i| if bits & (1 << i) != 0 { '#' } else { '.' })
        .collect()
}

#[derive(Debug, Eq, PartialEq)]
struct Search {
    count: u32,
    lights: u32,
}

impl Ord for Search {
    fn cmp(&self, other: &Self) -> Ordering {
        other.count.cmp(&self.count)
    }
}

impl PartialOrd for Search {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.count.partial_cmp(&self.count)
    }
}
fn solve_one(lights: u32, buttons: &[u32], len: usize) -> u32 {
    let mut queue = BinaryHeap::new();
    queue.push(Search {
        lights: 0,
        count: 0,
    });
    let mut iterations = 0;
    let progress = ProgressBar::new(1_000_000).with_style(
        ProgressStyle::default_bar()
            .template("{msg} {wide_bar} [{pos}/{len}]")
            .unwrap(),
    );
    for button in buttons.iter() {
        println!("{}", from_bits(button, len));
    }
    while let Some(s) = queue.pop() {
        iterations += 1;
        if iterations % 100_000 == 0 {
            progress.inc(100_000);
            progress.set_message(format!(
                "[{}, {}, {}]",
                from_bits(&lights, len),
                from_bits(&s.lights, len),
                s.count
            ));
        }
        if s.lights == lights {
            return s.count;
        }
        for button in buttons.iter() {
            queue.push(Search {
                lights: s.lights ^ button,
                count: s.count + 1,
            });
        }
    }
    unreachable!()
}

pub fn part_one(input: &str) -> u32 {
    let mut sum = 0;
    let lines: Vec<_> = input.trim().lines().collect();
    for line in lines {
        let input = parse_line(line);
        let result = solve_one(input.lights, &input.buttons);
        sum += result;
    }
    sum
}

pub fn part_two(input: &str) -> u32 {
    let mut sum = 0;
    let lines: Vec<_> = input.trim().lines().collect();
    let pb = ProgressBar::new(lines.len() as u64);
    for line in &lines {
        pb.inc(1);
        let input = parse_line(line);
        let result = solve_two(&input.buttons, &input.joltage);
        pb.println(format!("{:?}\t{}", input.joltage, result));
        sum += result;
    }
    pb.finish();
    sum
}

fn solve_two(buttons: &[u32], joltage: &[u32]) -> u32 {
    let (even, odd): (Vec<u32>, Vec<u32>) = joltage.iter().enumerate().fold(
        (vec![0; joltage.len()], vec![0; joltage.len()]),
        |(mut even, mut odd), (i, j)| {
            let (div, rem) = (j / 2, j % 2);
            even[i] = div;
            odd[i] = rem;
            (even, odd)
        },
    );
    let (odd_count, even_count) = (odd.iter().sum::<u32>(), even.iter().sum::<u32>());
    let count = if odd_count == 0 {
        0
    } else {
        solve_one(to_bits(odd.clone()), buttons)
    };
    dbg!(joltage, &odd, &even);
    return if even_count == 0 {
        count
    } else {
        count + 2 * solve_two(buttons, &even)
    };
}

#[derive(Clone, Eq, PartialEq)]
struct State {
    xcost: isize,
    rcost: isize,
    x: Vec<isize>,
    r: Vec<isize>,
}

// The priority queue depends on `Ord`.
// Search first for feasability, then for minimum cost.
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .rcost
            .cmp(&self.rcost)
            .then(other.xcost.cmp(&self.xcost))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other
            .rcost
            .partial_cmp(&self.rcost)
            .or(other.xcost.partial_cmp(&self.xcost))
    }
}

fn bfs(a: &Mat<f64>, b: &ColRef<f64>, sol: Vec<isize>) -> Vec<Vec<isize>> {
    let (m, n) = a.shape();
    let residue = {
        let x = Col::from_fn(n, |i| sol[i] as f64);
        let r = a * x - b;
        r.iter().map(|r| r.round() as isize).collect::<Vec<isize>>()
    };
    let to_state = |x: Vec<isize>, xcost: isize, r: Vec<isize>| {
        let rcost = r.iter().map(|r| r.abs()).sum::<isize>();
        State { xcost, rcost, x, r }
    };
    let mut queue = BinaryHeap::new();
    let xcost = sol.iter().sum::<isize>();
    queue.push(to_state(sol.clone(), xcost, residue));
    let mut feasable = vec![];
    let mut visited: HashSet<Vec<isize>> = HashSet::new();
    let progress = ProgressBar::new(1_000_000).with_style(
        ProgressStyle::default_bar()
            .template("{msg} {wide_bar} [{pos}/{len}]")
            .unwrap(),
    );
    let a: Vec<Vec<isize>> = a
        .col_iter()
        .map(|col| {
            col.iter()
                .map(|v| v.round() as isize)
                .collect::<Vec<isize>>()
        })
        .collect::<Vec<Vec<isize>>>();
    while let Some(State { x, r, xcost, rcost }) = queue.pop() {
        if visited.contains(&x) {
            continue;
        }
        visited.insert(x.clone());
        if visited.len() % 100_000 == 0 {
            progress.inc(100_000);
            progress.set_message(format!("[x: {}, r: {}]", xcost, rcost));
            if visited.len() % 1_000_000 == 0 && feasable.len() > 0 {
                return feasable;
            }
            if visited.len() >= 10_000_000 {
                return feasable;
            }
        }
        if r.iter().sum::<isize>() == 0 {
            feasable.push(x.clone());
        }
        for col in 0..n {
            let mut x_plus = x.clone();
            x_plus[col] += 1;
            if !visited.contains(&x_plus) {
                let mut r_plus = r.clone();
                (0..m).for_each(|row| r_plus[row] += a[col][row]);
                queue.push(to_state(x_plus, xcost + 1, r_plus));
            }
        }
    }
    unreachable!()
}
