pub const INPUT: &str = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1";

pub fn parse(input: &str) -> Vec<(Vec<char>, Vec<usize>)> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (row, seq) = line.split_once(' ').unwrap();
            return (
                row.chars().collect(),
                seq.split(',').map(|n| n.parse().unwrap()).collect(),
            );
        })
        .collect()
}

fn can_fit(seq: &[char], n: usize) -> bool {
    seq.len() >= n
        && seq[..n].iter().all(|&c| c == '?' || c == '#')
        && (seq.len() == n || seq[n] != '#')
}

fn run((onsen, nums): (Vec<char>, Vec<usize>)) -> usize {
    let mut slices = vec![0; onsen.len() + 2];
    slices[0] = 1;
    for &n in nums.iter() {
        let mut next = vec![0; onsen.len() + 2];
        for (i, count) in slices[..onsen.len()].iter().enumerate() {
            if *count == 0 {
                continue;
            }

            let slice = &onsen[i..];
            for (j, &c) in slice.iter().enumerate() {
                if c == '.' {
                    continue;
                }

                if (c == '?' || c == '#') && can_fit(&slice[j..], n) {
                    next[i + j + n + 1] += count;
                }

                if c == '#' {
                    break;
                }
            }
        }
        slices = next;
    }
    slices
        .into_iter()
        .enumerate()
        .filter(|c| c.0 >= onsen.len() || !onsen[c.0..].iter().any(|&c| c == '#'))
        .map(|c| c.1)
        .sum()
}

pub fn part_one(input: &str) -> usize {
    parse(input).into_iter().map(run).sum()
}

pub fn part_two(input: &str) -> usize {
    parse(input)
        .into_iter()
        .map(|(onsen, nums)| {
            (
                vec![onsen.iter().collect::<String>(); 5]
                    .join("?")
                    .chars()
                    .collect::<Vec<char>>(),
                vec![nums; 5].concat(),
            )
        })
        .map(run)
        .sum()
}
