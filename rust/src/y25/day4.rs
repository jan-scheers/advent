pub const INPUT: &str = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.";

fn is_removable(map: &Vec<Vec<char>>, i: i64, j: i64) -> bool {
    if map[i as usize][j as usize] == '.' {
        return false;
    }
    let (m, n) = (map.len() as i64, map[0].len() as i64);
    let mut count = 0;
    for i_ in i - 1..=i + 1 {
        for j_ in j - 1..=j + 1 {
            if i == i_ && j == j_ {
                continue;
            }
            if i_ < 0 || i_ >= m || j_ < 0 || j_ >= n {
                continue;
            }
            if map[i_ as usize][j_ as usize] == '@' {
                count += 1;
            }
        }
    }
    return count < 4;
}

fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .trim()
        .lines()
        .map(|line| line.chars().collect())
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let map = parse(input);
    let (m, n) = (map.len() as i64, map[0].len() as i64);
    let mut count = 0;
    for i in 0..m {
        for j in 0..n {
            if is_removable(&map, i, j) {
                count += 1;
            }
        }
    }
    return count;
}

pub fn part_two(input: &str) -> usize {
    let mut map = parse(input);
    let (m, n) = (map.len() as i64, map[0].len() as i64);
    let mut removable = 0;
    loop {
        let mut copy = map.clone();
        let mut count = 0;
        for i in 0..m {
            for j in 0..n {
                if is_removable(&map, i, j) {
                    count += 1;
                    copy[i as usize][j as usize] = '.';
                }
            }
        }
        if count == 0 {
            break;
        }
        removable += count;
        map = copy;
    }
    crate::pretty(&map);
    removable
}
