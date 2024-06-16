use std::collections::HashMap;

pub const INPUT: &str = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}";

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
struct Rule {
    rating: usize,
    greater: bool,
    threshold: usize,
    target: String,
}

impl Rule {
    fn applies(&self, part: &Vec<usize>) -> bool {
        if self.greater {
            part[self.rating] > self.threshold
        } else {
            part[self.rating] < self.threshold
        }
    }

    fn test(&self, (min, max): (usize, usize)) -> (Option<(usize, usize)>, Option<(usize, usize)>) {
        if self.greater {
            if min > self.threshold {
                return (Some((min, max)), None);
            } else if max <= self.threshold {
                return (None, Some((min, max)));
            } else {
                return (Some((self.threshold + 1, max)), Some((min, self.threshold)));
            }
        } else {
            if max < self.threshold {
                return (Some((min, max)), None);
            } else if min >= self.threshold {
                return (None, Some((min, max)));
            } else {
                return (Some((min, self.threshold - 1)), Some((self.threshold, max)));
            }
        }
    }
}

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
pub struct Workflow {
    rules: Vec<Rule>,
    default: String,
}

impl Workflow {
    fn new() -> Self {
        Workflow {
            rules: vec![],
            default: String::from("A"),
        }
    }
}

fn xmas(c: char) -> usize {
    match c {
        'x' => 0,
        'm' => 1,
        'a' => 2,
        's' => 3,
        _ => 4,
    }
}

pub fn parse(input: &str) -> (HashMap<String, Workflow>, Vec<Vec<usize>>) {
    let (flows, parts) = input.trim().split_once("\n\n").unwrap();
    let flows = flows
        .lines()
        .map(|line| {
            let mut flow = Workflow::new();
            let (label, rules) = line.split_once("{").unwrap();
            let rules = rules[..rules.len() - 1].split(',');
            for rule in rules {
                let Some((cond, target)) = rule.split_once(":") else {
                    flow.default = rule.to_owned();
                    continue;
                };
                flow.rules.push(Rule {
                    rating: xmas(cond.as_bytes()[0] as char),
                    greater: cond.as_bytes()[1] as char == '>',
                    threshold: cond[2..].parse().unwrap(),
                    target: target.to_owned(),
                });
            }
            (label.to_owned(), flow)
        })
        .collect();
    let parts = parts
        .trim()
        .lines()
        .map(|line| {
            let mut part = vec![0; 4];
            for rating in line[1..line.len() - 1].split(',') {
                let (key, value) = rating.split_once('=').unwrap();
                part[xmas(key.as_bytes()[0] as char)] = value.parse().unwrap();
            }
            return part;
        })
        .collect();
    (flows, parts)
}

fn target(part: &Vec<usize>, flow: &Workflow) -> String {
    for rule in flow.rules.iter() {
        if rule.applies(part) {
            return rule.target.clone();
        }
    }
    return flow.default.clone();
}

fn accepted(part: &Vec<usize>, flows: &HashMap<String, Workflow>) -> bool {
    let mut node = flows.get("in").unwrap();
    loop {
        let target = target(part, node);
        match target.as_str() {
            "A" => return true,
            "R" => return false,
            _ => node = flows.get(&target).unwrap(),
        }
    }
}

pub fn part_one(input: &str) -> usize {
    let (flows, parts) = parse(input);
    parts
        .iter()
        .map(|part| {
            if accepted(part, &flows) {
                part.iter().sum()
            } else {
                0
            }
        })
        .sum()
}

pub fn part_two(input: &str) -> usize {
    let (flows, _) = parse(input);
    let mut accepted = vec![];
    let mut rejected = vec![];
    let mut parts = vec![(vec![(1, 4000); 4], String::from("in"))];
    'outer: while let Some((part, node)) = parts.pop() {
        if node == "A" {
            accepted.push(part);
            continue;
        }
        if node == "R" {
            rejected.push(part);
            continue;
        }

        let node = flows.get(&node).unwrap();
        let mut part = part.clone();
        for rule in node.rules.iter() {
            let (pass, fail) = rule.test(part[rule.rating]);
            if pass.is_some() {
                let mut next = part.clone();
                next[rule.rating] = pass.unwrap();
                parts.push((next, rule.target.clone()))
            }
            if fail.is_none() {
                continue 'outer;
            }
            part[rule.rating] = fail.unwrap();
        }
        parts.push((part, node.default.clone()))
    }
    let rejected: usize = rejected
        .iter()
        .map(|part| {
            part.iter()
                .map(|(min, max)| max + 1 - min)
                .product::<usize>()
        })
        .sum();

    let accepted = accepted
        .iter()
        .map(|part| {
            part.iter()
                .map(|(min, max)| max + 1 - min)
                .product::<usize>()
        })
        .sum();
    dbg!(rejected, accepted, rejected + accepted);
    accepted
}
