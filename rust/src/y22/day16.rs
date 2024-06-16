pub const INPUT: &str = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";

use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
struct Valve {
    flow: usize,
    tunnels: Vec<usize>,
}

fn parse(input: &str) -> (Vec<usize>, Vec<Vec<usize>>) {
    let graph: HashMap<String, (usize, Vec<String>)> = input
        .trim()
        .lines()
        .map(|line| {
            let (a, b) = line.split_once("; ").unwrap();
            let mut a = a.split_whitespace();
            let name = a.nth(1).unwrap().to_owned();
            let rate = a
                .nth(2)
                .and_then(|s| s.split_once("="))
                .and_then(|d| d.1.parse().ok())
                .unwrap();
            let tuns = b
                .split_once("valve")
                .and_then(|s| s.1.split_once(" "))
                .unwrap()
                .1
                .split(", ")
                .map(|s| s.to_owned())
                .collect();
            (name, (rate, tuns))
        })
        .collect();
    let mut idx: Vec<_> = graph.iter().collect();
    idx.sort_by_key(|k| k.0 != "AA");
    let graph: Vec<Valve> = idx
        .iter()
        .map(|(_, valve)| Valve {
            flow: valve.0,
            tunnels: valve
                .1
                .iter()
                .map(|t| idx.iter().position(|a| a.0 == t).unwrap())
                .collect(),
        })
        .collect();
    (graph.iter().map(|v| v.flow).collect(), explore(&graph))
}

fn explore(valves: &Vec<Valve>) -> Vec<Vec<usize>> {
    (0..valves.len())
        .map(|i| {
            let mut distance = vec![usize::MAX; valves.len()];
            distance[i] = 1;
            let mut stack = VecDeque::from([i]);
            while let Some(node) = stack.pop_front() {
                for &next in valves[node]
                    .tunnels
                    .iter()
                    .filter(|&&t| distance[t] == usize::MAX)
                    .collect::<Vec<_>>()
                {
                    distance[next] = distance[node] + 1;
                    stack.push_back(next);
                }
            }
            distance
        })
        .collect()
}

struct Entry {
    path: Vec<(usize, usize, usize)>,
    seen: HashSet<usize>,
    tail: usize,
    flow: usize,
    time: usize,
    pres: usize,
}

pub fn part_one(input: &str) -> usize {
    let (flows, dist) = parse(input);
    let good: Vec<_> = (0..flows.len()).filter(|i| flows[*i] > 0).collect();
    let mut stack = vec![Entry {
        path: vec![],
        seen: HashSet::from([0]),
        tail: 0,
        flow: flows[0],
        time: 0,
        pres: 0,
    }];
    let mut best = (0, vec![]);
    while let Some(e) = stack.pop() {
        let options: Vec<_> = good
            .iter()
            .filter(|&next| !e.seen.contains(next) && dist[e.tail][*next] <= 30 - e.time)
            .collect();
        if options.len() == 0 {
            let mut path = e.path.clone();
            path.push((e.tail, e.time, e.pres));
            best = best.max((e.pres + (30 - e.time) * e.flow, path));
        }
        for &tail in options {
            let mut path = e.path.clone();
            path.push((e.tail, e.time, e.pres));
            let mut seen = e.seen.clone();
            seen.insert(tail);
            stack.push(Entry {
                path,
                seen,
                tail,
                flow: e.flow + flows[tail],
                time: e.time + dist[e.tail][tail],
                pres: e.pres + e.flow * dist[e.tail][tail],
            });
        }
    }
    println!("{:?}", best.1);
    simulate(&flows, &dist, &best.1.into_iter().map(|b| b.0).collect());
    best.0
}

struct Elephant {
    path: Vec<(usize, usize, usize)>,
    seen: HashSet<usize>,
    one: usize,
    two: usize,
    flow: usize,
    time: usize,
    pres: usize,
}

struct Elephant {}

pub fn part_two(input: &str) -> usize {
    let (flows, dist) = parse(input);
    let good: Vec<_> = (0..flows.len()).filter(|i| flows[*i] > 0).collect();
    let mut stack = vec![Elephant {
        path: vec![],
        seen: HashSet::from([0]),
        one: 0,
        two: 0,
        flow: flows[0],
        time: 0,
        pres: 0,
    }];
    let mut best = (0, vec![]);
    while let Some(e) = stack.pop() {
        let options: Vec<_> = good
            .iter()
            .filter(|&next| !e.seen.contains(next) && dist[e.tail][*next] <= 30 - e.time)
            .collect();
        if options.len() == 0 {
            let mut path = e.path.clone();
            path.push((e.tail, e.time, e.pres));
            best = best.max((e.pres + (30 - e.time) * e.flow, path));
        }
        for &tail in options {
            let mut path = e.path.clone();
            path.push((e.tail, e.time, e.pres));
            let mut seen = e.seen.clone();
            seen.insert(tail);
            stack.push(Entry {
                path,
                seen,
                tail,
                flow: e.flow + flows[tail],
                time: e.time + dist[e.tail][tail],
                pres: e.pres + e.flow * dist[e.tail][tail],
            });
        }
    }
    println!("{:?}", best.1);
    simulate(&flows, &dist, &best.1.into_iter().map(|b| b.0).collect());
    best.0
}

fn simulate(flows: &Vec<usize>, dist: &Vec<Vec<usize>>, path: &Vec<usize>) {
    let mut flow = 0;
    let mut pres = 0;
    let mut i = 1;
    let mut tok = dist[path[i - 1]][path[i]];
    for time in 1..=30 {
        pres += flow;
        if time == tok {
            flow += flows[path[i]];
            if i + 1 < path.len() {
                i += 1;
                tok += dist[path[i - 1]][path[i]];
            } else {
                tok = 40;
            }
        }
        println!("{} {} {}", time, flow, pres);
    }
}
