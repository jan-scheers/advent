pub const INPUT: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
";
use std::{
    cmp::Ordering::Equal,
    fmt::{Display, Write},
};

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
enum Packet {
    List(Vec<Box<Packet>>),
    Value(usize),
}

impl Packet {
    fn new(line: &str) -> Self {
        Packet::List(Self::list(&line[1..line.len() - 1]))
    }

    fn list(s: &str) -> Vec<Box<Packet>> {
        let mut i = 0;
        let mut res = vec![];
        while i < s.len() {
            let packet = match s.as_bytes()[i] {
                b'[' => {
                    let ls = Self::close_bracket(&s[i..]);
                    i += ls.len() + 1;
                    Packet::List(Self::list(&ls[1..ls.len() - 1]))
                }
                _ => {
                    let ls = Self::close_value(&s[i..]);
                    i += ls.len() + 1;
                    Packet::Value(ls.parse().unwrap())
                }
            };
            res.push(Box::new(packet));
        }
        res
    }

    fn close_bracket(s: &str) -> &str {
        let mut h = 0;
        let j = s
            .as_bytes()
            .iter()
            .position(|c| {
                match *c {
                    b'[' => h += 1,
                    b']' => h -= 1,
                    _ => (),
                }
                h == 0
            })
            .unwrap_or(s.len() - 1);
        &s[..=j]
    }

    fn close_value(s: &str) -> &str {
        let j = s
            .as_bytes()
            .iter()
            .position(|c| *c == b',')
            .unwrap_or(s.len());
        &s[..j]
    }
}

impl Display for Packet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Packet::List(vec) => {
                f.write_char('[')?;
                let mut ps = vec.iter();
                if let Some(p) = ps.next() {
                    p.fmt(f)?;
                }
                for p in ps {
                    f.write_char(',')?;
                    p.fmt(f)?;
                }
                f.write_char(']')
            }
            Packet::Value(v) => v.fmt(f),
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Value(a), Self::Value(b)) => a.cmp(b),
            (a, Self::Value(b)) => {
                let b = Packet::List(vec![Box::new(Self::Value(*b))]);
                a.cmp(&b)
            }
            (Self::Value(a), b) => {
                let a = Packet::List(vec![Box::new(Self::Value(*a))]);
                a.cmp(&b)
            }
            (Self::List(a), Self::List(b)) => {
                let ord = std::iter::zip(a, b).fold(Equal, |ord, (a, b)| match ord {
                    Equal => a.cmp(b),
                    _ => ord,
                });
                match ord {
                    Equal => a.len().cmp(&b.len()),
                    _ => ord,
                }
            }
        }
    }
}

fn parse(input: &str) -> Vec<Packet> {
    input
        .trim()
        .split("\n\n")
        .flat_map(|chunk| chunk.lines().map(|line| Packet::new(line)))
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let packets = parse(input);
    packets
        .chunks(2)
        .enumerate()
        .map(|(i, c)| match c[0] <= c[1] {
            true => i + 1,
            false => 0,
        })
        .sum()
}

pub fn part_two(input: &str) -> usize {
    let mut packets = parse(input);
    packets.sort_unstable();
    for p in packets.iter() {
        println!("{}", p);
    }
    let i = packets.binary_search(&Packet::new("[[2]]")).unwrap_err();
    let j = packets.binary_search(&Packet::new("[[6]]")).unwrap_err();
    (i + 1) * (j + 2)
}
