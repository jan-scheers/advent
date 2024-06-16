use std::cmp::max;
pub const INPUT: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

fn parse(input: &str) -> Vec<Vec<(usize, usize, usize)>> {
    input
        .trim()
        .split("\n")
        .map(|line| {
            let Some((_, line)) = line.split_once(": ") else {
                return vec![];
            };
            line.split("; ")
                .map(|set| {
                    set.split(", ").fold((0, 0, 0), |(r, g, b), item| {
                        let Some((num, col)) = item.split_once(" ") else {
                            return (r, g, b);
                        };
                        let num = num.parse::<usize>().unwrap();
                        match col {
                            "red" => (r + num, g, b),
                            "green" => (r, g + num, b),
                            _ => (r, g, b + num),
                        }
                    })
                })
                .collect()
        })
        .collect()
}

pub fn part_one(input: &str) -> usize {
    let parsed = parse(input);
    parsed.iter().enumerate().fold(0, |acc, (i, line)| {
        let fits = !line
            .iter()
            .any(|(r, g, b)| (*r > 12) || (*g > 13) || (*b > 14));
        return if fits { acc + i + 1 } else { acc };
    })
}

pub fn part_two(input: &str) -> usize {
    let parsed = parse(input);
    parsed.iter().fold(0, |acc, line| {
        let (r, g, b) = line.iter().fold((0, 0, 0), |(ar, ag, ab), (r, g, b)| {
            (max(ar, *r), max(ag, *g), max(ab, *b))
        });
        return acc + r * g * b;
    })
}
