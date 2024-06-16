pub const INPUT: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

extern crate nalgebra as na;

type Map = na::DMatrix<char>;

pub fn parse(input: &str) -> Map {
    let cmds: Vec<Vec<(usize, usize)>> = input
        .trim()
        .lines()
        .map(|line| {
            line.split(" -> ")
                .map(|ix| {
                    let (a, b) = ix.split_once(",").unwrap();
                    (a.parse().unwrap(), b.parse().unwrap())
                })
                .collect()
        })
        .collect();
    let n = cmds
        .iter()
        .flat_map(|r| r.iter().map(|c| c.1))
        .max()
        .unwrap()
        + 2;
    let mut map = na::DMatrix::from_fn(n + 1, 2 * n + 1, |_, _| '.');
    for cmd in cmds {
        for w in cmd.windows(2) {
            let mut row: Vec<_> = w.iter().map(|c| c.1).collect();
            row.sort();
            let mut col: Vec<_> = w.iter().map(|c| c.0 + n - 500).collect();
            col.sort();
            for row in row[0]..=row[1] {
                for col in col[0]..=col[1] {
                    map[(row, col)] = '#'
                }
            }
        }
    }
    map
}

pub fn part_one(input: &str) -> usize {
    let mut map = parse(input);
    let count = (0..).position(|_| drop(&mut map)).unwrap();
    println!("{}", map);
    count
}

pub fn part_two(input: &str) -> usize {
    let mut map = parse(input);
    let (m, n) = map.shape();
    for col in 0..n {
        map[(m - 1, col)] = '#'
    }
    let count = (0..).position(|_| drop(&mut map)).unwrap();
    println!("{}", map);
    count
}

fn drop(map: &mut Map) -> bool {
    let (m, n) = map.shape();
    let mut col = n / 2;
    if map[(0, col)] != '.' {
        return true;
    }
    for row in 0..m - 1 {
        if let Some(next) = [col, col - 1, col + 1]
            .iter()
            .find(|c| map[(row + 1, **c)] == '.')
        {
            col = *next
        } else {
            map[(row, col)] = 'o';
            return false;
        }
    }
    return true;
}
