use crate::{pretty, size};
use std::cmp;

pub const INPUT: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#";

pub fn parse(input: &str) -> Vec<Vec<Vec<char>>> {
    input
        .trim()
        .split("\n\n")
        .map(|mat| mat.lines().map(|line| line.chars().collect()).collect())
        .collect()
}

fn check_vert(mat: &Vec<Vec<char>>, r: usize) -> bool {
    let (m, n) = size(mat);
    let width = cmp::min(r, n - r);
    for i in 0..m {
        for j in 0..width {
            if mat[i][r - j - 1] != mat[i][r + j] {
                return false;
            }
        }
    }
    return true;
}

fn check_hori(mat: &Vec<Vec<char>>, r: usize) -> bool {
    let (m, n) = size(mat);
    let width = cmp::min(r, m - r);
    for i in 0..width {
        for j in 0..n {
            if mat[r - i - 1][j] != mat[r + i][j] {
                return false;
            }
        }
    }
    return true;
}

fn check_vert1(mat: &Vec<Vec<char>>, r: usize) -> bool {
    let (m, n) = size(mat);
    let width = cmp::min(r, n - r);
    let mut smudge = false;
    for i in 0..m {
        for j in 0..width {
            if mat[i][r - j - 1] != mat[i][r + j] {
                if smudge {
                    return false;
                } else {
                    smudge = true
                }
            }
        }
    }
    return smudge;
}

fn check_hori1(mat: &Vec<Vec<char>>, r: usize) -> bool {
    let (m, n) = size(mat);
    let width = cmp::min(r, m - r);
    let mut smudge = false;
    for i in 0..width {
        for j in 0..n {
            if mat[r - i - 1][j] != mat[r + i][j] {
                if smudge {
                    return false;
                } else {
                    smudge = true
                }
            }
        }
    }
    return smudge;
}

fn run(mat: Vec<Vec<char>>) -> usize {
    pretty(&mat);
    let (m, n) = size(&mat);
    let mut sum = 0;
    for i in 1..m {
        if check_hori(&mat, i) {
            sum += i * 100;
        }
    }
    for j in 1..n {
        if check_vert(&mat, j) {
            sum += j;
        }
    }
    sum
}

fn run2(mat: Vec<Vec<char>>) -> usize {
    pretty(&mat);
    let (m, n) = size(&mat);
    let mut sum = 0;
    for i in 1..m {
        if check_hori1(&mat, i) {
            sum += i * 100;
        }
    }
    for j in 1..n {
        if check_vert1(&mat, j) {
            sum += j;
        }
    }
    sum
}

pub fn part_one(input: &str) -> usize {
    parse(input).into_iter().map(run).sum()
}

pub fn part_two(input: &str) -> usize {
    parse(input).into_iter().map(run2).sum()
}
