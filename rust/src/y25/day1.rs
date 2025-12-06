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
            |mut state, line| {
                let mut x = line.chars();
                let dir = x.next().unwrap();
                let num: i32 = x.collect::<String>().parse().unwrap();
                match dir {
                    'L' => {
                        for _ in 0..num {
                            state.dial = state.dial - 1;
                            if state.dial == 0 {
                                state.counter += 1;
                            } else if state.dial < 0 {
                                state.dial = state.dial + 100;
                            }
                        }
                    }
                    'R' => {
                        for _ in 0..num {
                            state.dial = state.dial + 1;
                            if state.dial == 100 {
                                state.counter += 1;
                                state.dial = 0;
                            }
                        }
                    }
                    _ => {}
                };
                state
            },
        )
        .counter
}
