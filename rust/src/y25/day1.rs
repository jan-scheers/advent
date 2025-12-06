#[derive(Debug, Clone, Copy)]
struct State {
    dial: i32,
    counter: usize,
}

pub fn part_one(input: &str) -> usize {
    input
        .trim()
        .lines()
        .fold(
            State {
                dial: 50,
                counter: 0,
            },
            |state, line| {
                let mut x = line.chars();
                let dir = x.next().unwrap();
                let num: i32 = x.collect::<String>().parse().unwrap();
                let dial = match dir {
                    'L' => (state.dial - num) % 100,
                    'R' => (state.dial + num) % 100,
                    _ => state.dial,
                };
                State {
                    dial,
                    counter: state.counter + if dial == 0 { 1 } else { 0 },
                }
            },
        )
        .counter
}

pub fn part_two(input: &str) -> usize {
    input
        .trim()
        .lines()
        .fold(
            State {
                dial: 50,
                counter: 0,
            },
            |state, line| {
                let mut x = line.chars();
                let dir = x.next().unwrap();
                let num: i32 = x.collect::<String>().parse().unwrap();
                let (div, rem) = (num / 100, num % 100);
                if rem == 0 {
                    return State {
                        dial: state.dial,
                        counter: state.counter + div as usize,
                    };
                }
                match dir {
                    'L' => {
                        let next = state.dial - rem;
                        State {
                            dial: if next < 0 { next + 100 } else { next },
                            counter: state.counter
                                + div as usize
                                + if next <= 0 && state.dial != 0 { 1 } else { 0 },
                        }
                    }
                    'R' => {
                        let next = state.dial + rem;
                        State {
                            dial: if next >= 100 { next - 100 } else { next },
                            counter: state.counter + div as usize + if next >= 100 { 1 } else { 0 },
                        }
                    }
                    _ => state,
                }
            },
        )
        .counter
}
