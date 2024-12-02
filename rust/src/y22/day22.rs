use crate::{Dir, Vec2, CLOCK, EAST, SOUTH, WEST};
extern crate nalgebra as na;

pub const INPUT: &str = "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5";

const DIRS: [na::Vector2<i64>; 4] = [
    na::Vector2::new(-1, 0),
    na::Vector2::new(0, 1),
    na::Vector2::new(1, 0),
    na::Vector2::new(0, -1),
];

struct Face {
    row: usize,
    col: usize,
    dirs: Vec<usize>,
}

type Cube = Vec<Vec<Face>>;

struct Coord {
    rows: usize,
    cols: usize,
}

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
enum Cmd {
    Move(usize),
    Turn(char),
}

const INVALID: na::Vector2<i64> = na::Vector2::new(-1, -1);

fn wrapped_add(
    rows: usize,
    cols: usize,
    a: na::Vector2<i64>,
    b: na::Vector2<i64>,
) -> na::Vector2<i64> {
    na::Vector2::new(
        (a.x + a.x).rem_euclid(rows as i64),
        (a.y + a.y).rem_euclid(cols as i64),
    )
}

fn checked_add(
    rows: usize,
    cols: usize,
    a: na::Vector2<i64>,
    b: na::Vector2<i64>,
) -> Option<na::Vector2<i64>> {
    let res = a + b;
    if 0 <= a.x && a.x < rows as i64 && 0 <= a.y && a.y < cols as i64 {
        Some(res)
    } else {
        None
    }
}

fn cube(map: &str) -> (Map, Cube) {
    let mut map: Vec<Vec<char>> = map.lines().map(|l| l.chars().collect()).collect();
    let len = map.iter().map(|v| v.len()).max().unwrap();
    for line in map.iter_mut() {
        line.extend(vec![' '; len - line.len()])
    }
    let rows = 3;
    let n = map.len() / rows;
    let cols = map[0].len() / n;
    let col = (0..cols).find(|col| map[0][col * n] != ' ').unwrap();
    let mut cube = vec![Face {
        row: 0,
        col,
        dirs: vec![0; 4],
    }];
    while let Some((name, row, col, dir)) = frontier.pop() {}

    (vec![], vec![])
}

fn cube(map: &str, part: i64) -> (Map, Cube) {
    let mut map: Vec<Vec<char>> = map.lines().map(|l| l.chars().collect()).collect();
    let len = map.iter().map(|v| v.len()).max().unwrap();
    for line in map.iter_mut() {
        line.extend(vec![' '; len - line.len()])
    }
    let rows = 3;
    let n = map.len() / rows;
    let cols = map[0].len() / n;
    let mut cube: Vec<Vec<Option<Vec<Option<na::Vector2<i64>>>>>> = (0..rows)
        .map(|row| {
            (0..cols)
                .map(|col| {
                    if map[row * n][col * n] == ' ' {
                        return None;
                    }
                    let curr = na::Vector2::new(row as i64, col as i64);
                    Some(
                        DIRS.into_iter()
                            .map(|delta| {
                                checked_add(rows, cols, curr, delta)
                                    .filter(|c| map[n * c.x as usize][n * c.y as usize] != ' ')
                            })
                            .collect(),
                    )
                })
                .collect()
        })
        .collect();

    for row in 0..rows {
        for col in 0..cols {
            let Some(face) = cube[row][col] else {
                continue;
            };

            for (dir, delta) in DIRS.into_iter().enumerate() {
                if face[dir].is_some() {
                    continue;
                }
                let mut curr = na::Vector2::new(row as i64, col as i64);
                if part == 1 {
                    loop {
                        let mut next = wrapped_add(rows, cols, curr, delta);
                        if cube[next.x as usize][next.y as usize].is_some() {
                            face[dir] == Some(next);
                            break;
                        }
                        curr = next;
                    }
                } else {
                    if let Some(right) = checked_add(rows, cols, curr, DIRS[(dir + 1) % 4]) {
                        if let Some(right) = cube[right.x as usize][right.y as usize] {
                            if let Some(up) = right[dir] {
                                face[dir] = Some(up)
                            }
                        }
                    }
                    if let Some(left) = checked_add(rows, cols, curr, DIRS[(dir - 1) % 4]) {
                        if let Some(right) = cube[left.x as usize][left.y as usize] {
                            if let Some(up) = right[dir] {
                                face[dir] = Some(up)
                            }
                        }
                    }
                }
            }
        }
    }
    (map, cube)
}

fn parse(input: &str) -> (Map, Vec<Cmd>) {
    let (map, cmd) = input.trim().split_once("\n\n").unwrap();

    let mut log = vec![];
    let mut cmd = cmd.chars();
    let mut out = vec![];
    while let Some(c) = cmd.next() {
        if c.is_numeric() {
            log.push(c);
        } else {
            let num = log.into_iter().collect::<String>().parse().unwrap();
            out.push(Cmd::Move(num));
            out.push(Cmd::Turn(c));
            log = vec![];
        }
    }
    if log.len() > 0 {
        let num = log.into_iter().collect::<String>().parse().unwrap();
        out.push(Cmd::Move(num));
    }
    (map, out)
}
type Map = Vec<Vec<char>>;

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
struct State {
    row: usize,
    col: usize,
    dir: usize,
}

pub fn part_one(input: &str) -> usize {
    let (mut map, cmd) = parse(input);
    dbg!(&cmd);
    let st = State {
        row: 0,
        col: map[0].iter().position(|c| *c != ' ').unwrap(),
        dir: EAST,
    };
    map.iter()
        .for_each(|s| println!("{}", s.iter().map(|c| *c).collect::<String>()));
    let st = cmd.into_iter().fold(st, |st, cmd| step(&mut map, st, cmd));
    map.iter()
        .for_each(|s| println!("{}", s.iter().map(|c| *c).collect::<String>()));
    (st.row + 1) * 1000 + (st.col + 1) * 4 + st.dir.checked_sub(1).unwrap_or(3)
}

fn step(map: &mut Map, mut st: State, cmd: Cmd) -> State {
    match cmd {
        Cmd::Turn(c) => {
            st.dir = if c == 'L' {
                st.dir.checked_sub(1).unwrap_or(3)
            } else {
                (st.dir + 1) % 4
            }
        }
        Cmd::Move(n) => {
            for _ in 0..n {
                let (row, col) = match st.dir {
                    EAST => (
                        st.row,
                        if st.col + 1 >= map[0].len() || map[st.row][st.col + 1] == ' ' {
                            map[st.row].iter().position(|c| *c != ' ').unwrap()
                        } else {
                            st.col + 1
                        },
                    ),
                    SOUTH => (
                        if st.row + 1 >= map.len() || map[st.row + 1][st.col] == ' ' {
                            (0..map.len()).position(|r| map[r][st.col] != ' ').unwrap()
                        } else {
                            st.row + 1
                        },
                        st.col,
                    ),
                    WEST => (
                        st.row,
                        if st.col == 0 || map[st.row][st.col - 1] == ' ' {
                            map[st.row].iter().rposition(|c| *c != ' ').unwrap()
                        } else {
                            st.col - 1
                        },
                    ),
                    _ => (
                        if st.row == 0 || map[st.row - 1][st.col] == ' ' {
                            (0..map.len()).rposition(|r| map[r][st.col] != ' ').unwrap()
                        } else {
                            st.row - 1
                        },
                        st.col,
                    ),
                };
                if map[row][col] == '#' {
                    return st;
                }
                st.col = col;
                st.row = row;
                map[st.row][st.col] = match st.dir {
                    EAST => '>',
                    SOUTH => 'v',
                    WEST => '<',
                    _ => '^',
                };
            }
        }
    };
    st
}
