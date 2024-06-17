use std::collections::HashMap;

extern crate nalgebra as na;
pub const INPUT: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

const ROCKS: [&str; 5] = [
    "####",
    ".#.
###
.#.",
    "..#
..#
###",
    "#
#
#
#",
    "##
##",
];

type Mat = Vec<Vec<char>>;
type State = (Mat, usize, usize);

pub fn part_one(input: &str) -> usize {
    let jet = input.trim().chars().collect();
    let rocks: Vec<Mat> = ROCKS
        .map(|r| r.lines().rev().map(|l| l.chars().collect()).collect())
        .to_vec();
    let state = (vec![vec!['#'; 9]], 0, 0);
    (0..2022)
        .fold(state, |state, _| drop(&rocks, &jet, state))
        .0
        .len()
        - 1
}

pub fn part_two(input: &str) -> usize {
    let jet = input.trim().chars().collect();
    let rocks: Vec<Mat> = ROCKS
        .map(|r| r.lines().rev().map(|l| l.chars().collect()).collect())
        .to_vec();
    let mut state = (vec![vec!['#'; 9]], 0, 0);
    let mut height = vec![];
    let mut search = HashMap::new();
    let mut found = (0, 0);
    for i in 0.. {
        let key = (fill(&state.0), state.1, state.2);
        height.push(state.0.len());
        if let Some(j) = search.get(&key) {
            found = (*j, i - j);
            break;
        }
        search.insert(key, i);
        state = drop(&rocks, &jet, state);
    }
    let (start, cycle) = found;
    let deltah = height[height.len() - 1] - height[start];
    let (div, rem) = (
        (1000000000000 - start) / cycle,
        (1000000000000 - start) % cycle,
    );
    deltah * div + height[rem + start] - 1
}

fn drop(rocks: &Vec<Mat>, jet: &Vec<char>, (mut tower, mut j, mut r): State) -> State {
    let rock = &rocks[r];
    r = (r + 1) % rocks.len();
    let mut row = vec!['.'; 9];
    (row[0], row[8]) = ('#', '#');
    tower.extend(vec![row; 3 + rock.len()]);
    let (mut x, mut y) = (tower.len() - rock.len(), 3);
    loop {
        let test_y = if jet[j] == '<' { y - 1 } else { y + 1 };
        j = (j + 1) % jet.len();
        if !touch(rock, &tower, (x, test_y)) {
            y = test_y;
        }
        if touch(rock, &tower, (x - 1, y)) {
            (0..rock.len()).for_each(|i| {
                (0..rock[0].len()).for_each(|j| {
                    if rock[i][j] == '#' {
                        tower[i + x][j + y] = '#'
                    }
                })
            });
            break;
        } else {
            x -= 1;
        }
    }
    let length = tower
        .iter()
        .rposition(|r| r[1..r.len() - 1].iter().any(|c| *c == '#'))
        .unwrap()
        + 1;
    tower.truncate(length);
    (tower, j, r)
}

fn fill(tower: &Mat) -> String {
    let len = tower.len() - 1;
    let mut seen = vec![vec!['#'; 9]];
    let mut frontier: Vec<(usize, usize)> = (1..8)
        .map(|j| (0, j))
        .filter(|p| tower[len][p.1] == '.')
        .collect();
    while let Some((x, y)) = frontier.pop() {
        if x + 1 == seen.len() {
            seen.push(vec!['#'; 9])
        }
        seen[x][y] = '.';
        [(x, y - 1), (x, y + 1), (x + 1, y)]
            .into_iter()
            .filter(|p| seen[p.0][p.1] == '#' && tower[len - p.0][p.1] == '.')
            .for_each(|p| frontier.push(p));
    }
    seen.into_iter().flat_map(|v| v.into_iter()).collect()
}

fn touch(rock: &Mat, tower: &Mat, pos: (usize, usize)) -> bool {
    (0..rock.len()).any(|i| {
        (0..rock[0].len()).any(|j| tower[i + pos.0][j + pos.1] == '#' && rock[i][j] == '#')
    })
}

pub fn pretty(tower: &Mat) {
    let a = tower.iter().rev().flat_map(|r| r.iter()).map(|c| *c);
    let m = na::DMatrix::from_row_iterator(tower.len(), tower[0].len(), a);
    println!("{}", m);
}
