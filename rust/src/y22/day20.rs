pub const INPUT: &str = "1
2
-3
3
-2
0
4";

#[derive(Hash, Debug, PartialEq, Eq, Clone)]
struct Item {
    index: i64,
    shift: i64,
}

type State = (Vec<Item>, Vec<usize>);

fn parse(input: &str, key: i64) -> State {
    let items: Vec<Item> = input
        .lines()
        .map(|s| s.parse::<i64>().unwrap())
        .enumerate()
        .map(|(index, shift)| Item {
            index: index as i64,
            shift: shift * key,
        })
        .collect();
    let n = items.len();
    (items, (0..n).collect())
}

pub fn part_one(input: &str) -> i64 {
    let state = parse(input, 1);
    let (items, table) = mix(state, 1);
    let numbers: Vec<_> = table.into_iter().map(|i| items[i].shift).collect();
    let index0 = numbers.iter().position(|&n| n == 0).unwrap();
    vec![1000, 2000, 3000]
        .into_iter()
        .map(|i| numbers[(index0 + i) % numbers.len()])
        .sum()
}

pub fn part_two(input: &str) -> i64 {
    let state = parse(input, 811589153);
    let (items, table) = mix(state, 10);
    let numbers: Vec<_> = table.into_iter().map(|i| items[i].shift).collect();
    let index0 = numbers.iter().position(|&n| n == 0).unwrap();
    vec![1000, 2000, 3000]
        .into_iter()
        .map(|i| numbers[(index0 + i) % numbers.len()])
        .sum()
}

fn mix(state: State, iter: usize) -> State {
    (0..state.0.len() * iter).fold(state, shift)
}

fn shift((mut items, mut table): State, i: usize) -> State {
    let i = i % items.len();
    let item = &items[i];
    let len = items.len() as i64;
    let mut dest = (item.index + item.shift).rem_euclid(len - 1);
    if dest == 0 && item.shift != 0 {
        dest = len - 1;
    }

    if item.index < dest {
        (item.index..dest).for_each(|i| {
            let (curr, next) = (i as usize + 1, i as usize);
            let item = table[curr];
            table[next] = item;
            items[item].index = next as i64;
        })
    } else {
        (dest..item.index).rev().for_each(|i| {
            let (curr, next) = (i as usize, i as usize + 1);
            let item = table[curr];
            table[next] = item;
            items[item].index = next as i64;
        })
    }
    items[i].index = dest;
    table[dest as usize] = i;
    (items, table)
}
