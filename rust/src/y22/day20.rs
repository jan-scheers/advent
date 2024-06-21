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

pub fn part_one() {
    let mut items: Vec<Item> = vec![1, 2, -3, 3, -2, 0, 4]
        .into_iter()
        .enumerate()
        .map(|(index, shift)| Item {
            index: index.try_into().unwrap(),
            shift,
        })
        .collect();
    let mut table: Vec<usize> = (0..items.len()).collect();

    println!(
        "{:?}",
        table.iter().map(|i| items[*i].shift).collect::<Vec<_>>()
    );
    for i in 0..7 {
        shift(&mut items, &mut table, i);
        println!(
            "{:?}",
            table.iter().map(|i| items[*i].shift).collect::<Vec<_>>()
        );
    }
}

fn shift(items: &mut Vec<Item>, table: &mut Vec<usize>, i: usize) {
    let item = items[i].clone();
    let len = items.len() as i64;
    let mut dest = (item.index + item.shift).rem_euclid(len - 1);
    if dest == 0 && item.shift != 0 {
        dest = len - 1;
    }

    let mut ix = [item.index as usize, dest as usize];
    ix.sort();
    (ix[0]..ix[1]).for_each(|i| {
        let (a, b) = if item.index < dest {
            (i + 1, i)
        } else {
            (i, i + 1)
        };
        table[a] = table[b];
        items[table[a]].index = a as i64;
    });
    items[i].index = dest;
    table[ix[1]] = i;
}
