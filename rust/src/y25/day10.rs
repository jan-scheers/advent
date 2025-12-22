pub const INPUT: &str = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}";
use crate::lp;
use faer::prelude::SolveLstsq;
use faer::ColRef;
use faer::{concat, Col, Mat};
use indicatif::ProgressBar;
use indicatif::ProgressStyle;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug)]
struct Input {
    lights: Vec<u16>,
    buttons: Vec<Vec<u16>>,
    joltage: Vec<u16>,
}

fn parse_line(line: &str) -> Input {
    let (lights, line) = line.trim().split_once(' ').unwrap();
    let (buttons, joltage) = line.rsplit_once(' ').unwrap();
    let lights: Vec<u16> = lights
        .chars()
        .filter_map(|c| match c {
            '.' => Some(0_u16),
            '#' => Some(1_u16),
            _ => None,
        })
        .collect();

    let buttons: Vec<Vec<u16>> = buttons
        .split(' ')
        .map(|b| {
            b[1..b.len() - 1]
                .split(',')
                .fold(vec![0; lights.len()], |mut acc, b| {
                    acc[b.parse::<usize>().unwrap()] = 1;
                    acc
                })
        })
        .collect();
    let joltage: Vec<u16> = joltage[1..joltage.len() - 1]
        .split(',')
        .map(|j| j.parse().unwrap())
        .collect();
    Input {
        lights,
        buttons,
        joltage,
    }
}

fn to_bits(lights: &Vec<u16>) -> u16 {
    lights
        .iter()
        .enumerate()
        .map(|(i, &l)| 2_u16.pow(i as u32) * l)
        .sum()
}

struct StateOne {
    lights: u16,
    count: u16,
}

fn solve(input: &Input) -> u16 {
    let target: u16 = to_bits(&input.lights);
    let buttons: Vec<u16> = input.buttons.iter().map(to_bits).collect();
    let mut queue = VecDeque::from(vec![StateOne {
        lights: 0,
        count: 0,
    }]);
    while let Some(StateOne { lights, count }) = queue.pop_front() {
        if lights == target {
            return count;
        }
        for button in buttons.iter() {
            queue.push_back(StateOne {
                lights: lights ^ button,
                count: count + 1,
            });
        }
    }
    unreachable!()
}

pub fn part_one(input: &str) -> u16 {
    let mut sum = 0;
    let lines: Vec<_> = input.trim().lines().collect();
    let pb = ProgressBar::new(lines.len() as u64);
    for line in lines {
        let input = parse_line(line);
        let result = solve(&input);
        sum += result;
    }
    pb.finish();
    sum
}

pub fn part_two(input: &str) -> usize {
    let mut sum = 0;
    let lines: Vec<_> = input.trim().lines().collect();
    for line in &lines {
        let input = parse_line(line);
        let a = Mat::from_fn(input.buttons[0].len(), input.buttons.len(), |i, j| {
            input.buttons[j][i] as f64
        });
        let b = Mat::from_fn(input.joltage.len(), 1, |i, _| input.joltage[i] as f64);
        let c = Mat::from_fn(input.buttons.len(), 1, |_, _| 1.0);
        let (m, n) = a.shape();
        let (a_, c_) = if m > n {
            print!("Augmented\t");
            let diff = m - n;
            let u: Mat<f64> = Mat::ones(m, diff);
            let v: Mat<f64> = Mat::full(m, diff, -1.0);
            (
                concat![[a, u, v]],
                concat![[c], [Mat::full(diff * 2, 1, 1000.0)]],
            )
        } else {
            print!("Regular..\t");
            (a.clone(), c.clone())
        };
        let sol: Col<f64> = match lp::simplex(&a_, &b, &c_) {
            Ok(sol) => {
                println!("Optimal");
                sol
            }
            Err(e) => {
                println!("{}", e);
                if m >= n {
                    a.col_piv_qr().solve_lstsq(b.col(0).as_ref())
                } else {
                    lp::min_norm_qr(&a, &b).col(0).to_owned()
                }
            }
        };
        let sol = sol
            .iter()
            .map(|x| (x.round() as isize).max(0).min(400))
            .collect::<Vec<isize>>();
        let cost = sol.iter().sum::<isize>();
        println!("init\t{}\t{:?}", cost, sol);
        let sols = bfs(&a, &b.col(0), sol[..n].to_vec());
        if sols.len() == 0 {
            panic!("No feasible solutions found for {}", line);
        }
        let argmin = (0..sols.len())
            .min_by_key(|i| sols[*i].iter().sum::<isize>())
            .unwrap();
        let cost = sols[argmin].iter().sum::<isize>();
        println!("sol\t{}\t{:?}", cost, sols[argmin]);
        sum += cost as usize;
    }
    sum
}

#[derive(Clone, Eq, PartialEq)]
struct State {
    dist: isize,
    xcost: isize,
    rcost: isize,
    x: Vec<isize>,
    r: Vec<isize>,
}

// The priority queue depends on `Ord`.
// Explicitly implement the trait so the queue becomes a min-heap
// instead of a max-heap.
impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        // Notice that we flip the ordering on costs.
        // In case of a tie we compare positions - this step is necessary
        // to make implementations of `PartialEq` and `Ord` consistent.
        other
            .rcost
            .cmp(&self.rcost)
            .then(other.xcost.cmp(&self.xcost))
            .then(other.dist.cmp(&self.dist))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other
            .rcost
            .partial_cmp(&self.rcost)
            .or(other.xcost.partial_cmp(&self.xcost))
            .or(other.dist.partial_cmp(&self.dist))
    }
}

fn bfs(a: &Mat<f64>, b: &ColRef<f64>, sol: Vec<isize>) -> Vec<Vec<isize>> {
    let (m, n) = a.shape();
    let residue = {
        let x = Col::from_fn(n, |i| sol[i] as f64);
        let r = a * x - b;
        r.iter().map(|r| r.round() as isize).collect::<Vec<isize>>()
    };
    let to_state = |x: Vec<isize>, r: Vec<isize>| {
        let dist = (0..n).map(|i| (x[i] - sol[i]).abs()).sum::<isize>();
        let xcost = x.iter().sum::<isize>();
        let rcost = r.iter().map(|r| r.abs()).sum::<isize>();
        State {
            dist,
            xcost,
            rcost,
            x,
            r,
        }
    };
    let mut queue = BinaryHeap::new();
    queue.push(to_state(sol.clone(), residue));
    let mut sols = vec![];
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
    while let Some(State {
        x,
        r,
        dist,
        xcost,
        rcost,
    }) = queue.pop()
    {
        if visited.contains(&x) {
            continue;
        }
        visited.insert(x.clone());
        if visited.len() % 100_000 == 0 {
            progress.inc(100_000);
            progress.set_message(format!("[d: {}, x: {}, r: {}]", dist, xcost, rcost));
            if visited.len() % 1_000_000 == 0 && sols.len() > 0 {
                return sols;
            }
            if visited.len() >= 10_000_000 {
                return sols;
            }
        }
        if r.iter().sum::<isize>() == 0 {
            sols.push(x.clone());
        }
        for col in 0..n {
            let mut x_plus = x.clone();
            x_plus[col] += 1;
            if !visited.contains(&x_plus) {
                let mut r_plus = r.clone();
                (0..m).for_each(|row| r_plus[row] += a[col][row]);
                queue.push(to_state(x_plus, r_plus));
            }
            let mut x_minus = x.clone();
            x_minus[col] -= 1;
            if x_minus[col] >= 0 && !visited.contains(&x_minus) {
                let mut r_minus = r.clone();
                (0..m).for_each(|row| r_minus[row] -= a[col][row]);
                queue.push(to_state(x_minus, r_minus));
            }
        }
    }
    unreachable!()
}
