pub const INPUT: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

pub fn hash(cmd: &[char]) -> usize {
    cmd.iter().fold(0u8, |hash, b| {
        (hash.wrapping_add(*b as u8)).wrapping_mul(17)
    }) as usize
}

pub fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .trim()
        .lines()
        .collect::<String>()
        .split(',')
        .map(|step| step.chars().collect())
        .collect()
}

pub fn part_one(input: &str) -> usize {
    parse(input).iter().map(|cmd| hash(&cmd[..])).sum()
}

pub fn part_two(input: &str) -> usize {
    let mut hashmap: Vec<Vec<(&[char], u8)>> = vec![vec![]; 256];
    let commands = parse(input);
    commands.iter().for_each(|command| {
        let sep = command.iter().position(|&c| c == '=' || c == '-').unwrap();
        let label = &command[..sep];
        let box_ = &mut hashmap[hash(label)];
        let index = box_.iter().position(|(l, _)| *l == label);

        if command[sep] == '-' {
            if index.is_some() {
                box_.remove(index.unwrap());
            }
        } else {
            let focus: u8 = command[sep + 1].to_string().parse().unwrap();
            if let Some(index) = index {
                box_[index].1 = focus
            } else {
                box_.push((label, focus))
            }
        }
    });
    hashmap.iter().enumerate().fold(0, |sum, (i, box_)| {
        sum + box_
            .iter()
            .enumerate()
            .map(|(j, (_, f))| (i + 1) * (j + 1) * *f as usize)
            .sum::<usize>()
    })
}
