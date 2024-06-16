pub const INPUT: &str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
";

pub fn parse(input: &str) -> (Vec<Vec<char>>, Vec<(usize, usize, usize)>) {
    let (crates, moves) = input.split_once("\n\n").unwrap();
    let mut crates = crates.lines().rev();
    let n = crates.next().unwrap().trim().split_whitespace().count();
    let mut stacks = vec![vec![]; n];
    for line in crates {
        let line = line.as_bytes();
        for i in 0..n {
            let c = line[1 + 4 * i];
            if c != b' ' {
                stacks[i].push(c as char);
            }
        }
    }
    let moves: Vec<(usize, usize, usize)> = moves
        .trim()
        .lines()
        .map(|line| {
            let mut digits = line
                .split_whitespace()
                .skip(1)
                .step_by(2)
                .map(|d| d.parse().unwrap());
            (
                digits.next().unwrap(),
                digits.next().unwrap() - 1,
                digits.next().unwrap() - 1,
            )
        })
        .collect();
    return (stacks, moves);
}

pub fn part_one(input: &str) -> String {
    let (mut stacks, moves) = parse(input);
    for (num, from, to) in moves {
        for _ in 0..num {
            let c = stacks[from].pop().unwrap();
            stacks[to].push(c);
        }
    }
    stacks.iter().map(|s| *s.last().unwrap()).collect()
}

pub fn part_two(input: &str) -> String {
    let (mut stacks, moves) = parse(input);
    for (num, from, to) in moves {
        let length = stacks[from].len() - num;
        let slice = stacks[from][length..].to_vec();
        stacks[from].truncate(length);
        stacks[to].extend(slice);
    }
    stacks.iter().map(|s| *s.last().unwrap()).collect()
}
