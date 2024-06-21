pub const INPUT: &str = "Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.";

type Robot = Vec<usize>;
type Rocks = Vec<usize>;
type Blueprint = Vec<Robot>;

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
struct State {
    time: usize,
    robots: Robot,
    rocks: Rocks,
    path: Vec<usize>,
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.robots.partial_cmp(&other.robots) {
            Some(std::cmp::Ordering::Equal) => {}
            _ => return None,
        }

        match self.time.cmp(&other.time) {
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Less => return Some(std::cmp::Ordering::Greater),
            std::cmp::Ordering::Greater => return Some(std::cmp::Ordering::Less),
        }

        std::iter::zip(self.rocks.iter(), other.rocks.iter())
            .rev()
            .fold(Some(core::cmp::Ordering::Equal), |ord, (a, b)| match ord {
                Some(core::cmp::Ordering::Equal) => a.partial_cmp(b),
                _ => ord,
            })
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

fn to_index(s: &str) -> usize {
    match s {
        "clay" => 1,
        "obsidian" => 2,
        "geode" => 3,
        _ => 0,
    }
}

fn to_str(i: usize) -> &'static str {
    match i {
        1 => "clay",
        2 => "obsidian",
        3 => "geode",
        _ => "ore",
    }
}

fn to_robot(i: usize) -> &'static str {
    match i {
        1 => "clay-collecting",
        2 => "obsidian-collecting",
        3 => "geode-cracking",
        _ => "ore-collecting",
    }
}

pub fn parse(input: &str) -> Vec<Blueprint> {
    input["Blueprint".len()..]
        .trim()
        .split("Blueprint")
        .map(|s| {
            s.trim()
                .split_once(':')
                .unwrap()
                .1
                .split('.')
                .take(4)
                .map(|s| {
                    let mut recipe = vec![0; 4];
                    let mut c = s.split_once("costs ").unwrap().1.split_ascii_whitespace();
                    let (a, t) = (c.next().unwrap().parse().unwrap(), c.next().unwrap());
                    recipe[to_index(t)] = a;
                    let and = c.next();
                    if and.is_some() {
                        let (b, t) = (c.next().unwrap().parse().unwrap(), c.next().unwrap());
                        recipe[to_index(t)] = b;
                    }
                    recipe
                })
                .collect()
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    parse(input)
        .iter()
        .enumerate()
        .map(|(i, recipe)| {
            let best = optimize(recipe, 24);
            (i + 1) * best.rocks[3]
        })
        .sum()
}

pub fn part_two(input: &str) -> usize {
    parse(input)
        .iter()
        .map(|recipe| optimize(recipe, 32).rocks[3])
        .product()
}

fn optimize(recipe: &Blueprint, time: usize) -> State {
    let mut frontier = vec![start()];
    let mut best = vec![start()];
    while let Some(s) = frontier.pop() {
        let dt = time - s.time;
        if dt.pow(2) / 2 + s.robots[3] * dt + s.rocks[3] < best[0].rocks[3] {
            continue;
        }
        let paths = paths(recipe, &s, time);
        if paths.len() == 0 {
            let s = tick(s, dt);
            match s.rocks[3].cmp(&best[0].rocks[3]) {
                std::cmp::Ordering::Greater => {
                    //println!("{}", s.rocks[3]);
                    best = vec![s]
                }
                std::cmp::Ordering::Equal => best.push(s),
                std::cmp::Ordering::Less => (),
            }
        } else {
            frontier.extend(paths);
        }
    }
    println!(
        "{} {}",
        best.iter()
            .all(|b| *b == simulate(recipe, &b.path, false, time)),
        best.last().unwrap().rocks[3],
    );
    best.last().unwrap().clone()
}

fn paths(recipe: &Blueprint, s: &State, time: usize) -> Vec<State> {
    recipe
        .iter()
        .enumerate()
        .rev()
        .filter_map(|(i, req)| {
            let Some(dt) = deltat(req, s) else {
                return None;
            };

            let Some(time_left) = (time + i - 4).checked_sub(s.time) else {
                return None;
            };

            if dt > time_left {
                return None;
            }

            if i < 3 {
                let needed = recipe[i + 1][i];
                let prod_left = needed - s.robots[i];
                if prod_left * time_left < needed + dt {
                    return None;
                }
            }

            let mut s = tick(s.clone(), dt);
            for i in 0..4 {
                s.rocks[i] -= req[i];
            }
            s.robots[i] += 1;
            s.path.push(i);
            Some(s)
        })
        .collect()
}

fn start() -> State {
    State {
        time: 0,
        robots: vec![1, 0, 0, 0],
        rocks: vec![0; 4],
        path: vec![],
    }
}

fn deltat(req: &Rocks, s: &State) -> Option<usize> {
    (0..4)
        .map(|i| {
            if req[i] == 0 {
                Some(0)
            } else if s.robots[i] == 0 {
                None
            } else {
                Some(req[i].saturating_sub(s.rocks[i]).div_ceil(s.robots[i]))
            }
        })
        .fold(Some(0), |a, b| match (a, b) {
            (Some(a), Some(b)) => Some(a.max(b)),
            _ => None,
        })
        .map(|t| t + 1)
}

fn tick(s: State, dt: usize) -> State {
    State {
        time: s.time + dt,
        rocks: (0..4).map(|i| s.robots[i] * dt + s.rocks[i]).collect(),
        ..s
    }
}

fn simulate(recipe: &Blueprint, path: &Vec<usize>, show: bool, time: usize) -> State {
    let mut s = start();
    s.path = path.clone();
    let mut path = path.iter();
    let mut get_next = |s: &State| {
        path.next()
            .and_then(|r| deltat(&recipe[*r], s).map(|t| (*r, s.time + t)))
    };
    let mut next = get_next(&s);

    for t in 1..=time {
        let mut built = None;
        if show {
            println!("== Minute {} ==", t);
        }
        if let Some((r, _)) = next.filter(|n| n.1 == t) {
            let req: Vec<_> = recipe[r]
                .iter()
                .enumerate()
                .filter(|r| *r.1 > 0)
                .map(|r| format!("{} {}", r.1, to_str(r.0)))
                .collect();
            if show {
                println!(
                    "Spend {} to start building {} {} robot.",
                    req.join(" and "),
                    if r % 2 == 0 { "an" } else { "a" },
                    to_robot(r)
                );
            }
            s.rocks = std::iter::zip(s.rocks.iter(), recipe[r].iter())
                .map(|(r, req)| r - req)
                .collect();
            built = Some(r);
        }
        s = tick(s, 1);
        for (r, n) in s.robots.iter().enumerate().filter(|r| *r.1 > 0) {
            let plural = if *n > 1 { ("s", "") } else { ("", "s") };
            let rock = if r == 3 {
                format!("open geode{}", if s.rocks[r] > 1 { "s" } else { "" })
            } else {
                to_str(r).to_string()
            };
            if show {
                println!(
                    "{} {} robot{} {}{} {} {}{}; you now have {} {}.",
                    n,
                    to_robot(r),
                    plural.0,
                    if r == 3 { "crack" } else { "collect" },
                    plural.1,
                    n,
                    to_str(r),
                    if r == 3 && *n > 1 { "s" } else { "" },
                    s.rocks[r],
                    rock
                );
            }
        }
        if let Some(r) = built {
            s.robots[r] += 1;
            next = get_next(&s);
            if show {
                println!(
                    "The new {} robot is ready; you now have {} of them.",
                    to_robot(r),
                    s.robots[r],
                )
            }
        }
        if show {
            println!();
        }
    }
    s
}
