use std::collections::HashMap;

pub const INPUT: &str = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

#[derive(Debug, PartialEq, Clone)]
enum Monkey {
    Grunt(f64),
    Operator { a: String, op: String, b: String },
}

type Monkeys = HashMap<String, Monkey>;

fn parse(input: &str) -> Monkeys {
    input
        .trim()
        .lines()
        .map(|line| {
            let (name, occupation) = line.split_once(": ").unwrap();
            let pieces: Vec<&str> = occupation.split_ascii_whitespace().collect();
            return (
                name.to_string(),
                if let [a, op, b] = pieces[..] {
                    Monkey::Operator {
                        a: a.to_string(),
                        op: op.to_string(),
                        b: b.to_string(),
                    }
                } else {
                    Monkey::Grunt(pieces[0].parse().unwrap())
                },
            );
        })
        .collect()
}

pub fn part_one(input: &str) -> i64 {
    let monkeys = parse(input);
    eval(&monkeys, "root") as i64
}

pub fn part_two(input: &str) -> i64 {
    let mut monkeys = parse(input);
    if let Some(Monkey::Operator { op, .. }) = monkeys.get_mut("root") {
        *op = "-".to_string();
    };
    let [mut a, mut b] = [i64::MIN as f64, i64::MAX as f64];
    let low_side = humn(&mut monkeys, a).signum();
    loop {
        let c = (a + b) / 2.0;
        let vc = humn(&mut monkeys, c);
        if vc.abs() < 0.5 {
            return c.round() as i64;
        }
        if vc.signum() == low_side {
            a = c;
        } else {
            b = c;
        }
    }
}

fn eval(monkeys: &Monkeys, name: &str) -> f64 {
    match monkeys.get(name) {
        Some(Monkey::Grunt(v)) => *v,
        Some(Monkey::Operator { a, op, b }) => {
            let (a, b) = (eval(monkeys, a), eval(monkeys, b));
            match op.as_str() {
                "+" => a + b,
                "-" => a - b,
                "*" => a * b,
                _ => a / b,
            }
        }
        None => 0.0,
    }
}

fn humn(monkeys: &mut Monkeys, val: f64) -> f64 {
    if let Some(s) = monkeys.get_mut("humn") {
        *s = Monkey::Grunt(val)
    }
    eval(monkeys, "root")
}
