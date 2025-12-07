pub const INPUT: &str = ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............";

fn parse(input: &str) -> Vec<Vec<char>> {
    input.lines().map(|line| line.chars().collect()).collect()
}

pub fn solve(input: &str) -> (usize, usize) {
    let map = parse(input);
    let (head, tail) = map.split_at(1);
    let mut beams = head[0]
        .iter()
        .map(|c| if *c == 'S' { 1 } else { 0 })
        .collect::<Vec<_>>();

    let mut trail = map[1..].to_vec();
    let mut count = 0;
    for (row_idx, row) in tail.iter().enumerate() {
        let mut new_beams = vec![0; row.len()];
        for (i, beam) in beams.iter().enumerate() {
            if *beam == 0 {
                continue;
            }
            match row[i] {
                '^' => {
                    count += 1;
                    new_beams[i - 1] += beam;
                    new_beams[i + 1] += beam;
                }
                _ => {
                    new_beams[i] += beam;
                }
            }
        }
        beams = new_beams;
        for (i, b) in beams.iter().enumerate() {
            if *b > 0 {
                trail[row_idx][i] = '|';
            }
        }
    }
    crate::pretty(&trail);
    (count, beams.iter().sum::<usize>())
}
