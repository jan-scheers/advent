pub const INPUT: &str = "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2";

type Shape = Vec<Vec<char>>;
type Grid = ((usize, usize), Vec<usize>);

fn parse(input: &str) -> (Vec<Shape>, Vec<Grid>) {
    let mut x = input.trim().split("\n\n").collect::<Vec<_>>();
    let grids = x.pop().unwrap();
    let shapes = x
        .into_iter()
        .map(|shape| {
            shape
                .lines()
                .skip(1)
                .map(|line| line.chars().collect::<Vec<_>>())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let grids: Vec<((usize, usize), Vec<usize>)> = grids
        .lines()
        .map(|line| {
            let (shape, values) = line.split_once(": ").unwrap();
            let (rows, cols) = shape.split_once("x").unwrap();
            (
                (rows.parse().unwrap(), cols.parse().unwrap()),
                values
                    .split_whitespace()
                    .map(|n| n.parse().unwrap())
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();

    (shapes, grids)
}

pub fn part_one(input: &str) -> usize {
    let (_, grids) = parse(input);
    grids
        .iter()
        .filter(|grid| {
            let (rows, cols) = grid.0;
            let feasible = rows * cols >= grid.1.iter().sum::<usize>() * 9;
            feasible
        })
        .count()
}
