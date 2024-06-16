use std::{
    collections::{HashMap, HashSet},
    vec,
};

pub const INPUT: &str = "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#";

type Graph = HashMap<(usize, usize), HashMap<(usize, usize), usize>>;

fn cold(c: char, coord: &(usize, usize), i: usize, j: usize) -> bool {
    match c {
        'v' => coord.0 != i + 1,
        '^' => coord.0 != i - 1,
        '>' => coord.1 != j + 1,
        '<' => coord.1 != j - 1,
        '.' => true,
        _ => false,
    }
}

fn hot(c: char) -> bool {
    c != '#'
}

pub fn parse(input: &str, warm: bool) -> (usize, usize, Graph) {
    let island: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect();

    let m = island.len();
    let goal = island[m - 1].iter().position(|t| *t == '.').unwrap();
    let mut graph: Graph = HashMap::from([((m - 1, goal), HashMap::new())]);
    let mut stack = vec![vec![(m - 1, goal), (m - 2, goal)]];
    while let Some(mut trail) = stack.pop() {
        let tail = trail.len() - 1;
        if let Some(verts) = graph.get_mut(&trail[tail]) {
            let length = verts.entry(trail[0]).or_default();
            *length = tail.max(*length);
        } else if trail[tail].0 > 0 {
            let (i, j) = trail[tail];
            let options: Vec<_> = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
                .into_iter()
                .filter(|coord| {
                    let c = island[coord.0][coord.1];
                    *coord != trail[tail - 1] && if warm { hot(c) } else { cold(c, coord, i, j) }
                })
                .collect();
            if options.len() == 1 {
                trail.push(options[0]);
                stack.push(trail);
            } else if options.len() > 1 {
                graph.insert(trail[tail], HashMap::from([(trail[0], tail)]));
                stack.extend(options.into_iter().map(|opt| vec![trail[tail], opt]))
            }
        } else {
            graph.insert(trail[tail], HashMap::from([(trail[0], tail)]));
        }
    }
    let start = island[0].iter().position(|t| *t == '.').unwrap();
    let goal = m - 1;
    if !warm {
        return (start, goal, graph);
    }
    let mut bidirectional = graph.clone();
    for (key, verts) in graph.iter() {
        for (vert, len) in verts.iter() {
            bidirectional.entry(*vert).and_modify(|v| {
                let length = v.entry(*key).or_default();
                *length = (*len).max(*length)
            });
        }
    }
    (start, goal, bidirectional)
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Hike {
    tail: (usize, usize),
    set: HashSet<(usize, usize)>,
    len: usize,
}

fn search(graph: Graph, start: usize, goal: usize) -> usize {
    let mut stack = vec![Hike {
        tail: (0, start),
        set: HashSet::new(),
        len: 0,
    }];
    let mut best = 0;
    while let Some(mut hike) = stack.pop() {
        if hike.tail.0 < goal {
            hike.set.insert(hike.tail);
            let options = graph
                .get(&hike.tail)
                .unwrap()
                .iter()
                .filter(|(next, _)| !hike.set.contains(next))
                .map(|(next, len)| Hike {
                    tail: *next,
                    len: hike.len + len,
                    set: hike.set.clone(),
                });
            stack.extend(options);
        } else if hike.len > best {
            best = hike.len;
            println!("{}", best);
        }
    }
    best
}

pub fn part_one(input: &str) -> usize {
    let (start, goal, graph) = parse(input, false);
    search(graph, start, goal)
}

pub fn part_two(input: &str) -> usize {
    let (start, goal, graph) = parse(input, true);
    visualise(&graph);
    search(graph, start, goal)
}

fn visualise(graph: &Graph) {
    let keys: HashMap<_, _> = graph
        .keys()
        .enumerate()
        .map(|(i, k)| (*k, (i as u8 + b'A') as char))
        .collect();
    for (key, name) in keys.iter() {
        print!("{} -> ", name);
        for vert in graph.get(key).unwrap().keys() {
            print!(" {},", keys.get(vert).unwrap())
        }
        println!();
    }
}
