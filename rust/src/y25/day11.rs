use std::collections::{BinaryHeap, HashMap};
pub const INPUT: &str = "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out";

fn parse(input: &str) -> HashMap<String, Vec<String>> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (key, values) = line.split_once(':').unwrap();
            (
                key.to_string(),
                values.split_whitespace().map(|s| s.to_string()).collect(),
            )
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let graph = parse(input);
    solve(&graph, "you")
}

fn solve(graph: &HashMap<String, Vec<String>>, curr: &str) -> usize {
    if curr == "out" {
        return 1;
    }
    graph
        .get(curr)
        .unwrap()
        .iter()
        .map(|next| solve(graph, next))
        .sum()
}

pub const INPUT_TWO: &str = "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out";

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
struct Search {
    len: usize,
    fft: bool,
    dac: bool,
    tip: String,
}

impl Ord for Search {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.len.cmp(&self.len)
    }
}

impl PartialOrd for Search {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.len.partial_cmp(&self.len)
    }
}

pub fn part_two(input: &str) -> usize {
    let graph = parse(input);
    let start = Search {
        len: 0,
        fft: false,
        dac: false,
        tip: "svr".to_string(),
    };
    let mut dp: HashMap<Search, usize> = HashMap::new();
    dp.insert(start.clone(), 1);
    let mut frontier = BinaryHeap::from([start]);
    while let Some(curr) = frontier.pop() {
        if curr.tip == "out" {
            continue;
        }
        let curr_count = *dp.get(&curr).unwrap();
        let fft = curr.tip == "fft" || curr.fft;
        let dac = curr.tip == "dac" || curr.dac;
        for next in graph.get(&curr.tip).unwrap() {
            let next = Search {
                len: curr.len + 1,
                fft,
                dac,
                tip: next.clone(),
            };
            if let Some(next_count) = dp.get_mut(&next) {
                *next_count += curr_count;
            } else {
                dp.insert(next.clone(), curr_count);
                frontier.push(next);
            }
        }
    }
    dp.into_iter()
        .filter(|(search, _)| search.tip == "out" && search.fft && search.dac)
        .map(|(_, count)| count)
        .sum()
}
