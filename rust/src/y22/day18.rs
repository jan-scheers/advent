use std::collections::HashSet;

pub const INPUT: &str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

type Cube = [i64; 3];
type Cubes = HashSet<Cube>;

pub fn parse(input: &str) -> Cubes {
    input
        .trim()
        .lines()
        .map(|l| {
            let [a, b, c] = l.split(',').map(|n| n.parse().unwrap()).collect::<Vec<_>>()[..] else {
                panic!();
            };
            [a, b, c]
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let cubes = parse(input);
    cubes
        .iter()
        .map(|&c| {
            (0..=2)
                .flat_map(|dim| {
                    let mut dirs = [c; 2];
                    dirs[0][dim] -= 1;
                    dirs[1][dim] += 1;
                    dirs
                })
                .filter(|c| !cubes.contains(c))
                .count()
        })
        .sum()
}

pub fn part_two(input: &str) -> usize {
    let cubes = parse(input);

    let min_bound = [0, 1, 2]
        .map(|i| cubes.iter().map(|c| c[i]).min().unwrap())
        .map(|d| d - 1);
    let max_bound = [0, 1, 2]
        .map(|i| cubes.iter().map(|c| c[i]).max().unwrap())
        .map(|d| d + 1);
    let mut water = HashSet::new();
    let mut frontier = vec![min_bound];
    while let Some(drop) = frontier.pop() {
        water.insert(drop);
        (0..=2)
            .flat_map(|dim| {
                let mut dirs = [drop; 2];
                dirs[0][dim] -= 1;
                dirs[1][dim] += 1;
                dirs.into_iter()
                    .filter(move |o| min_bound[dim] <= o[dim] && o[dim] <= max_bound[dim])
            })
            .filter(|d| !(cubes.contains(d) || water.contains(d)))
            .for_each(|d| frontier.push(d));
    }
    cubes
        .iter()
        .map(|&c| {
            (0..=2)
                .flat_map(|dim| {
                    let mut opts = [c.clone(), c.clone()];
                    opts[0][dim] -= 1;
                    opts[1][dim] += 1;
                    opts
                })
                .filter(|c| !cubes.contains(c) && water.contains(c))
                .count()
        })
        .sum()
}
