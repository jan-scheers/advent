pub const INPUT: &str = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ";

fn parse(input: &str) -> (Vec<Vec<i64>>, Vec<char>) {
    let (values, ops) = input.trim().rsplit_once('\n').unwrap();
    let values: Vec<Vec<i64>> = values
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|n| n.parse().unwrap())
                .collect()
        })
        .collect();
    let ops: Vec<char> = ops
        .trim()
        .split_whitespace()
        .map(|op| op.chars().next().unwrap())
        .collect();
    (values, ops)
}

pub fn part_one(input: &str) -> i64 {
    let (values, ops) = parse(input);
    let (m, n) = (values.len(), ops.len());
    (0..n)
        .map(|col| match ops[col] {
            '+' => (0..m).map(|row| values[row][col]).sum::<i64>(),
            '*' => (0..m).map(|row| values[row][col]).product::<i64>(),
            _ => panic!("Invalid operation: {}", ops[col]),
        })
        .sum::<i64>()
}

pub fn part_two(input: &str) -> i64 {
    let map: Vec<Vec<char>> = input.lines().map(|line| line.chars().collect()).collect();
    let (m, n) = (map.len(), map[0].len());
    let mut total = 0;
    let mut nums = vec![];
    for col in (0..n).rev() {
        let mut num = vec![];
        for row in 0..m - 1 {
            let c = map[row][col];
            if c != ' ' {
                num.push(c);
            }
        }
        if num.is_empty() {
            continue;
        }
        let num = num.into_iter().collect::<String>().parse::<i64>().unwrap();
        nums.push(num);
        let op = map[m - 1][col];
        if op == ' ' {
            continue;
        }
        match op {
            '+' => total += nums.iter().sum::<i64>(),
            '*' => total += nums.iter().product::<i64>(),
            _ => panic!("Invalid operation: {}", op),
        }
        nums.clear();
    }
    total
}
