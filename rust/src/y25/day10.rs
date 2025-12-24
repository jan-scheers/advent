pub const INPUT: &str = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}";
use std::collections::HashMap;

#[derive(Debug)]
struct Input {
    lights: u16,
    buttons: Vec<u16>,
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
    let buttons: Vec<u16> = buttons
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

    let joltage: Vec<u16> = joltage[1..joltage.len() - 1]
        .split(',')
        .map(|j| j.parse().unwrap())
        .collect();
    Input {
        lights: to_bits(lights),
        buttons,
        joltage,
    }
}

fn to_bits(lights: Vec<u16>) -> u16 {
    lights.iter().enumerate().map(|(i, &l)| (1 << i) * l).sum()
}

fn from_bits(bits: &u16, len: usize) -> Vec<u16> {
    (0..len)
        .map(|i| if bits & (1 << i) == 0 { 0 } else { 1 })
        .collect()
}

pub fn part_one(input: &str) -> u16 {
    input
        .trim()
        .lines()
        .map(|line| {
            let input = parse_line(line);
            solve_one(input.lights, &input.buttons)
                .iter()
                .map(|s| s.cost)
                .min()
                .unwrap()
        })
        .sum()
}

fn solve_one(lights: u16, masks: &[u16]) -> Vec<Solution> {
    let mut solutions = vec![];
    if lights == 0 {
        solutions.push(Solution {
            cost: 0,
            pressed: vec![false; masks.len()],
        });
    };
    let mut prev_gray: u16 = 0;
    let mut curr_lights: u16 = 0;
    let mut curr_cost: u16 = 0;
    let mut curr_bits: Vec<bool> = vec![false; masks.len()];

    for i in 1..(1 << masks.len()) {
        let curr_gray = i ^ (i >> 1);
        let bit = (curr_gray ^ prev_gray).trailing_zeros();
        curr_lights ^= masks[bit as usize];
        if curr_gray >> bit & 1 == 1 {
            curr_cost += 1;
            curr_bits[bit as usize] = true;
        } else {
            curr_cost -= 1;
            curr_bits[bit as usize] = false;
        }
        if lights == curr_lights {
            solutions.push(Solution {
                cost: curr_cost,
                pressed: curr_bits.clone(),
            });
        }
        prev_gray = curr_gray;
    }
    solutions
}

#[derive(Debug)]
struct Solution {
    cost: u16,
    pressed: Vec<bool>,
}

pub fn part_two(input: &str) -> usize {
    input
        .trim()
        .lines()
        .map(|line| {
            let input = parse_line(line);
            solve_two(input).unwrap()
        })
        .sum::<usize>()
}

fn solve_two(input: Input) -> Option<usize> {
    let button_vecs: Vec<Vec<u16>> = input
        .buttons
        .iter()
        .map(|b| from_bits(b, input.joltage.len()))
        .collect();
    let mut cache = HashMap::new();
    solve(&mut cache, &button_vecs, &input.buttons, &input.joltage)
}

fn solve(
    cache: &mut HashMap<Vec<u16>, Option<usize>>,
    button_vecs: &[Vec<u16>],
    button_masks: &[u16],
    joltage: &Vec<u16>,
) -> Option<usize> {
    if let Some(result) = cache.get(joltage) {
        return *result;
    }
    if joltage.iter().sum::<u16>() == 0 {
        return Some(0);
    }
    let parity: Vec<u16> = joltage.iter().map(|j| j % 2).collect();
    solve_one(to_bits(parity), button_masks)
        .into_iter()
        .filter_map(|solution| {
            (0..button_vecs.len())
                .filter(|i| solution.pressed[*i])
                .fold(Some(joltage.to_vec()), |j, i| {
                    j.and_then(|j| subtract(j, &button_vecs[i]))
                })
                .and_then(|mut joltage| {
                    assert!(joltage.iter().all(|j| *j % 2 == 0));
                    for j in joltage.iter_mut() {
                        *j /= 2;
                    }
                    let cost = solve(cache, button_vecs, button_masks, &joltage);
                    cache.insert(joltage, cost);
                    cost
                })
                .map(|cost| 2 * cost + solution.cost as usize)
        })
        .min()
}

#[inline]
fn subtract(mut joltage: Vec<u16>, button: &[u16]) -> Option<Vec<u16>> {
    for i in 0..joltage.len() {
        if joltage[i] < button[i] {
            return None;
        }
        joltage[i] -= button[i];
    }
    Some(joltage)
}

/*
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
*/
