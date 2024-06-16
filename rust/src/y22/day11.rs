pub const INPUT: &str = "Monkey 0:
Starting items: 79, 98
Operation: new = old * 19
Test: divisible by 23
  If true: throw to monkey 2
  If false: throw to monkey 3

Monkey 1:
Starting items: 54, 65, 75, 74
Operation: new = old + 6
Test: divisible by 19
  If true: throw to monkey 2
  If false: throw to monkey 0

Monkey 2:
Starting items: 79, 60, 97
Operation: new = old * old
Test: divisible by 13
  If true: throw to monkey 1
  If false: throw to monkey 3

Monkey 3:
Starting items: 74
Operation: new = old + 3
Test: divisible by 17
  If true: throw to monkey 0
  If false: throw to monkey 1";

struct Monkey {
    items: Vec<usize>,
    lhs: Option<usize>,
    op: String,
    rhs: Option<usize>,
    test: usize,
    good: usize,
    bad: usize,
    seen: usize,
}

impl Monkey {
    fn new(lines: &str) -> Monkey {
        let mut lines = lines.lines().skip(1);
        let start = lines.next().unwrap().trim()["Starting items:".len()..]
            .trim()
            .split(", ")
            .map(|x| x.parse().unwrap())
            .collect();

        let [lhs, o, rhs] = lines.next().unwrap().trim()["Operation: new =".len()..]
            .trim()
            .split_whitespace()
            .collect::<Vec<_>>()[..]
        else {
            panic!("ruhroh")
        };
        let lhs = lhs.parse::<usize>().ok();
        let rhs = rhs.parse::<usize>().ok();
        let op = o.to_owned();

        fn to_usize(line: &str, skip: usize) -> usize {
            line.trim()[skip..].trim().parse().unwrap()
        }

        Monkey {
            items: start,
            lhs,
            op,
            rhs,
            test: to_usize(lines.next().unwrap(), "Test: divisible by".len()),
            good: to_usize(lines.next().unwrap(), "If true: throw to monkey".len()),
            bad: to_usize(lines.next().unwrap(), "If false: throw to monkey".len()),
            seen: 0,
        }
    }

    fn op(&self, old: usize) -> usize {
        let lhs = self.lhs.unwrap_or(old);
        let rhs = self.rhs.unwrap_or(old);
        match self.op.as_str() {
            "+" => lhs + rhs,
            "*" => lhs * rhs,
            _ => 0,
        }
    }

    fn test(&self, val: usize) -> usize {
        if val % self.test == 0 {
            self.good
        } else {
            self.bad
        }
    }
}

fn parse(input: &str) -> Vec<Monkey> {
    input.trim().split("\n\n").map(Monkey::new).collect()
}

pub fn part_one(input: &str) -> usize {
    let mut monkeys = (0..20).fold(parse(input), |mut monkeys, _round| {
        for i in 0..monkeys.len() {
            let monkey = &mut monkeys[i];
            let items: Vec<_> = monkey
                .items
                .iter()
                .map(|item| {
                    let val = monkey.op(*item) / 3;
                    let next = monkey.test(val);
                    (next, val)
                })
                .collect();
            monkey.seen += items.len();
            monkey.items = vec![];
            for (next, val) in items {
                monkeys[next].items.push(val);
            }
        }
        monkeys
    });
    monkeys.sort_by_key(|m| usize::MAX - m.seen);
    monkeys[0].seen * monkeys[1].seen
}

pub fn part_two(input: &str) -> usize {
    let monkeys = parse(input);
    let divisor: usize = monkeys.iter().map(|m| m.test).product();
    let mut monkeys = (0..10000).fold(monkeys, |mut monkeys, _round| {
        for i in 0..monkeys.len() {
            let monkey = &mut monkeys[i];
            let items: Vec<_> = monkey
                .items
                .iter()
                .map(|item| {
                    let val = monkey.op(*item) % divisor;
                    let next = monkey.test(val);
                    (next, val)
                })
                .collect();
            monkey.seen += items.len();
            monkey.items = vec![];
            for (next, val) in items {
                monkeys[next].items.push(val);
            }
        }
        monkeys
    });
    monkeys.sort_by_key(|m| usize::MAX - m.seen);
    monkeys[0].seen * monkeys[1].seen
}
