use crate::{pretty, rotanti, rotclck, size};

pub const INPUT: &str = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....";

pub fn parse(input: &str) -> Vec<Vec<char>> {
    let mut map = input.trim().lines().fold(vec![], |mut map, line| {
        let mut row = vec!['#'];
        row.extend(line.chars());
        row.push('#');
        if map.len() == 0 {
            map.push(vec!['#'; row.len()])
        }
        map.push(row);
        map
    });
    map.push(map[0].clone());
    map
}

pub fn push(map: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let (m, n) = size(&map);
    (0..m).fold(vec![], |mut result, row| {
        let cubes = (0..n).fold(vec![], |mut cubes, col| {
            match map[row][col] {
                '#' => cubes.push((col, 0)),
                'O' => {
                    let last = cubes.len() - 1;
                    cubes[last].1 += 1
                }
                _ => {}
            }
            return cubes;
        });
        result.push(cubes.iter().fold(vec![], |mut row, (col, balls)| {
            let last = row.len();
            row.extend(vec!['.'; col - last]);
            row.push('#');
            row.extend(vec!['O'; *balls]);
            return row;
        }));
        return result;
    })
}

pub fn cycle(map: &Vec<Vec<char>>) -> (usize, Vec<Vec<Vec<char>>>) {
    let mut cycles: Vec<Vec<Vec<char>>> = vec![];
    let mut map = map.clone();
    loop {
        for _ in 0..4 {
            map = rotclck(&push(&map));
        }
        if let Some(start) = cycles.iter().position(|c| *c == map) {
            return (start, cycles);
        } else {
            cycles.push(map.clone());
        }
    }
}

pub fn count(map: &Vec<Vec<char>>) -> usize {
    let (m, n) = size(map);
    (0..n)
        .rev()
        .map(|col| (n - col - 1) * (0..m).filter(|&row| map[row][col] == 'O').count())
        .sum()
}

pub fn part_one(input: &str) -> usize {
    let map = push(&rotanti(&parse(input)));
    pretty(&rotclck(&map));
    count(&map)
}

pub fn part_two(input: &str) -> usize {
    let map = rotanti(&parse(input));
    let (start, cycles) = cycle(&map);
    let cycles = cycles[start..].to_vec();
    let cycle = &cycles[(1_000_000_000 - start - 1) % cycles.len()];
    pretty(&rotclck(cycle));
    count(cycle)
}
