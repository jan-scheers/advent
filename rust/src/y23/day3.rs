use std::cmp::{max, min};
use std::collections::HashMap;

pub const INPUT: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

pub fn part_one(input: &str) -> u32 {
    let mat: Vec<Vec<char>> = input
        .trim()
        .split("\n")
        .map(|line| line.chars().collect())
        .collect();
    let (m, n) = (mat.len(), mat[0].len());
    let test = |i, j, k| {
        for i in max(i as i32 - 1, 0) as usize..min(i + 2, m) {
            for j in max(j as i32 - 1, 0) as usize..min(k + 1, n) {
                let c = mat[i][j];
                if !(c.is_digit(10) || c == '.') {
                    return true;
                }
            }
        }
        return false;
    };

    mat.iter().enumerate().fold(0, |acc, (i, line)| {
        let do_test = |j, k| {
            if test(i, j, k) {
                line[j..k]
                    .iter()
                    .collect::<String>()
                    .parse::<u32>()
                    .unwrap()
            } else {
                0
            }
        };
        let mut acc = acc;
        let mut j: Option<usize> = None;
        for (k, c) in line.iter().enumerate() {
            if j.is_some() && !c.is_digit(10) {
                acc += do_test(j.unwrap(), k);
                j = None;
            }
            if j.is_none() && c.is_digit(10) {
                j = Some(k);
            }
        }
        if j.is_some() {
            acc += do_test(j.unwrap(), line.len());
        }
        return acc;
    })
}

pub fn part_two(input: &str) -> usize {
    let mat: Vec<Vec<char>> = input
        .trim()
        .split("\n")
        .map(|line| line.chars().collect())
        .collect();
    let (m, n) = (mat.len(), mat[0].len());
    let mut map: HashMap<(usize, usize), Vec<usize>> = HashMap::new();

    for (i, line) in mat.iter().enumerate() {
        let mut do_test = |j, k| {
            let val = line[j..k]
                .iter()
                .collect::<String>()
                .parse::<usize>()
                .unwrap();

            for i in max(i as i32 - 1, 0) as usize..min(i + 2, m) {
                for j in max(j as i32 - 1, 0) as usize..min(k + 1, n) {
                    let c = mat[i][j];
                    if !(c.is_digit(10) || c == '.') {
                        if let Some(vec) = map.get_mut(&(i, j)) {
                            vec.push(val);
                        } else {
                            map.insert((i, j), vec![val]);
                        }
                    }
                }
            }
        };

        let mut j: Option<usize> = None;
        for (k, c) in line.iter().enumerate() {
            if j.is_some() && !c.is_digit(10) {
                do_test(j.unwrap(), k);
                j = None;
            }
            if j.is_none() && c.is_digit(10) {
                j = Some(k);
            }
        }
        if j.is_some() {
            do_test(j.unwrap(), line.len());
        }
    }

    map.iter().fold(0, |prod, (_, v)| {
        if v.len() == 2 {
            prod + v[0] * v[1]
        } else {
            prod
        }
    })
}
