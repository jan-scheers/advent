pub fn part_one(input: &String) -> usize {
    input.trim().split("\n").fold(0, |acc, line| {
        let big = c2i(line.chars().find(|c| c.is_digit(10)).unwrap());
        let small = c2i(line.chars().rev().find(|c| c.is_digit(10)).unwrap());
        acc + 10 * big + small
    })
}

fn c2i(c: char) -> usize {
    let mut s = String::new();
    s.push(c);
    s.parse().unwrap()
}

pub fn part_two(input: &String) -> usize {
    let digits = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    input.trim().split("\n").fold(0, |acc, line| {
        let bigstr = digits
            .iter()
            .enumerate()
            .filter_map(|(d, &digit)| line.find(digit).and_then(|i| Some((i, d + 1))))
            .min()
            .unwrap_or((line.len(), 0));
        let bigchr = line
            .chars()
            .enumerate()
            .find(|(_, c)| c.is_digit(10))
            .and_then(|(i, c)| Some((i, c2i(c))))
            .unwrap_or((line.len(), 0));
        let (_, big) = std::cmp::min(bigstr, bigchr);

        let smlstr = digits
            .iter()
            .enumerate()
            .filter_map(|(d, &digit)| line.rfind(digit).and_then(|i| Some((i, d + 1))))
            .max()
            .unwrap_or((0, 0));
        let smlchr = line
            .chars()
            .rev()
            .enumerate()
            .find(|(_, c)| c.is_digit(10))
            .and_then(|(i, c)| Some((line.len() - 1 - i, c2i(c))))
            .unwrap_or((0, 0));
        let (_, small) = std::cmp::max(smlstr, smlchr);
        acc + big * 10 + small
    })
}
