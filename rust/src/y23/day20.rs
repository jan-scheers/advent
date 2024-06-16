use std::iter;

use crate::lcm;
pub const INPUT: &str = "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a";

pub const INPUT2: &str = "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output";

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
enum GateKind {
    Broadcast,
    FlipFlop,
    Inverter,
    Output,
}

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
struct Gate {
    label: String,
    index: usize,
    kind: GateKind,
    input: Vec<usize>,
    output: Vec<usize>,
}

fn parse(input: &str) -> Vec<Gate> {
    let gates: Vec<(String, GateKind, Vec<String>)> = input
        .trim()
        .lines()
        .map(|line| {
            let (label, output) = line.split_once(" -> ").unwrap();
            let (label, kind) = match label.as_bytes()[0] as char {
                '%' => (&label[1..], GateKind::FlipFlop),
                '&' => (&label[1..], GateKind::Inverter),
                _ => (label, GateKind::Broadcast),
            };
            let output = output.split(", ").map(|s| s.to_owned()).collect();
            (label.to_owned(), kind, output)
        })
        .collect();
    let all_gates: Vec<_> = gates.iter().flat_map(|g| g.2.iter()).collect();
    let receive = all_gates
        .iter()
        .find(|g| !gates.iter().any(|(label, _, _)| ***g == *label))
        .unwrap();
    let receive = (receive.to_owned().to_owned(), GateKind::Output, vec![]);

    let broadcast = gates.iter().filter(|g| g.1 == GateKind::Broadcast);
    let flipflops = gates.iter().filter(|g| g.1 == GateKind::FlipFlop);
    let inverter = gates.iter().filter(|g| g.1 == GateKind::Inverter);

    let mut gates: Vec<_> = broadcast
        .chain(flipflops)
        .chain(inverter)
        .enumerate()
        .collect();
    gates.push((gates.len(), &receive));

    let outputs: Vec<(String, GateKind, Vec<usize>)> = gates
        .iter()
        .map(|(_, (label, kind, output))| {
            (
                label.to_owned(),
                kind.to_owned(),
                output
                    .iter()
                    .map(|s| {
                        gates
                            .iter()
                            .find(|(_, (label, _, _))| label == s)
                            .unwrap()
                            .0
                    })
                    .collect(),
            )
        })
        .collect();
    let inputs: Vec<Vec<usize>> = (0..gates.len())
        .map(|g| {
            outputs
                .iter()
                .enumerate()
                .filter_map(|(j, (_, _, out))| {
                    if out.iter().any(|o| *o == g) {
                        Some(j)
                    } else {
                        None
                    }
                })
                .collect()
        })
        .collect();

    iter::zip(inputs.into_iter(), outputs.into_iter())
        .enumerate()
        .map(|(index, (input, (label, kind, output)))| Gate {
            label,
            index,
            kind,
            output,
            input,
        })
        .collect()
}

fn color(gates: &Vec<Gate>, conjunctions: &Vec<usize>, i: usize) -> usize {
    let simple = |i: usize| {
        conjunctions
            .iter()
            .position(|&c| gates[c].input.iter().any(|j| *j == i) || c == i)
    };

    if let Some(c) = simple(i) {
        return c;
    }

    if let Some(c) = gates[i].input.iter().filter_map(|g| simple(*g)).next() {
        return c;
    }

    gates[i]
        .output
        .iter()
        .filter_map(|g| simple(*g))
        .next()
        .unwrap_or(4)
}

fn submodules(gates: &Vec<Gate>) -> Vec<Vec<Gate>> {
    let conjunctions: Vec<usize> = (0..gates.len())
        .filter(|&c| {
            gates[c].kind == GateKind::Inverter
                && gates[c]
                    .input
                    .iter()
                    .all(|f| gates[*f].kind == GateKind::FlipFlop)
        })
        .collect();
    let color = |i: usize| color(gates, &conjunctions, i);

    (0..conjunctions.len())
        .map(|col| {
            let mut module = vec![gates[0].clone()];
            module.extend(
                gates
                    .iter()
                    .filter(|gate| color(gate.index) == col && color(gate.output[0]) != 4)
                    .map(|gate| gate.clone()),
            );
            module.push(Gate {
                kind: GateKind::Output,
                ..gates
                    .iter()
                    .find(|gate| color(gate.index) == col && color(gate.output[0]) == 4)
                    .unwrap()
                    .clone()
            });

            module
                .iter()
                .enumerate()
                .map(|(index, gate)| Gate {
                    input: gate
                        .input
                        .iter()
                        .filter_map(|i| (0..module.len()).find(|j| module[*j].index == *i))
                        .collect(),
                    index,
                    output: gate
                        .output
                        .iter()
                        .filter_map(|i| (0..module.len()).find(|j| module[*j].index == *i))
                        .collect(),
                    ..gate.clone()
                })
                .collect()
        })
        .collect()
}

pub fn part_one(input: &str) {
    let gates = parse(input);
    let mut count = vec![0; 2];
    let mut memory = vec![false; gates.len()];
    for _ in 0..1000 {
        let (mut next, mut last, mut pulses) = (0, 1, vec![(0, false); 1 << 12]);
        while next != last {
            let (dest, high) = pulses[next];
            next = (next + 1) % (1 << 12);
            count[high as usize] += 1;

            let gate = &gates[dest];
            match gate.kind {
                GateKind::FlipFlop => {
                    if high {
                        continue;
                    } else {
                        memory[gate.index] = !memory[gate.index];
                    }
                }
                GateKind::Inverter => memory[gate.index] = !gate.input.iter().all(|i| memory[*i]),
                _ => {}
            }

            for dest in gate.output.iter() {
                pulses[last] = (*dest, memory[gate.index]);
                last = (last + 1) % (1 << 12);
            }
        }
    }
    println!("{}", count[0] * count[1])
}

fn count(gates: Vec<Gate>) -> usize {
    let mut memory = vec![false; gates.len()];
    for count in 1.. {
        let (mut next, mut last, mut pulses) = (0, 1, vec![(0, false); 1 << 12]);
        while next != last {
            let (dest, high) = pulses[next];
            next = (next + 1) % (1 << 12);

            let gate = &gates[dest];
            match gate.kind {
                GateKind::FlipFlop => {
                    if high {
                        continue;
                    } else {
                        memory[gate.index] = !memory[gate.index];
                    }
                }
                GateKind::Inverter => memory[gate.index] = !gate.input.iter().all(|i| memory[*i]),
                GateKind::Output => {
                    if !high {
                        return count;
                    }
                }
                _ => {}
            }

            for dest in gate.output.iter() {
                pulses[last] = (*dest, memory[gate.index]);
                last = (last + 1) % (1 << 12);
            }
        }
    }
    0
}

pub fn part_two(input: &str) {
    let gates = parse(input);
    let counts: Vec<_> = submodules(&gates).into_iter().map(count).collect();
    println!("{}", lcm(&counts));
}
