pub const INPUT: &str = "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9";

#[derive(Hash, Debug, PartialEq, Eq, Clone, Copy)]
pub struct Block {
    x: (u32, u32),
    y: (u32, u32),
    z: (u32, u32),
}

pub fn parse(input: &str) -> Vec<Block> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('~').unwrap();
            let mut dims = a.split(',').zip(b.split(',')).map(|(a, b)| {
                let (a, b) = (a.parse().unwrap(), b.parse().unwrap());
                if a <= b {
                    (a, b)
                } else {
                    (b, a)
                }
            });
            Block {
                x: dims.next().unwrap(),
                y: dims.next().unwrap(),
                z: dims.next().unwrap(),
            }
        })
        .collect()
}

pub fn part_one(input: &str) -> u32 {
    let mut blocks = parse(input);
    settle(&mut blocks);

    (0..blocks.len())
        .map(|i| {
            let z = blocks[i].z.1;
            let mut without = blocks[..i].to_vec();
            without.extend_from_slice(&blocks[i + 1..]);
            let moved = (0..without.len())
                .filter(|i| without[*i].z.0 == z + 1)
                .any(|i| drop(&without, &without[i]) != z + 1);
            (!moved) as u32
        })
        .sum()
}

pub fn part_two(input: &str) -> u32 {
    let mut blocks = parse(input);
    settle(&mut blocks);

    (0..blocks.len())
        .map(|i| {
            let mut without = blocks[..i].to_vec();
            without.extend_from_slice(&blocks[i + 1..]);
            settle(&mut without)
        })
        .sum()
}

fn settle(blocks: &mut Vec<Block>) -> u32 {
    let mut moved = vec![false; blocks.len()];
    let mut start = 0;
    loop {
        let mut stable = true;
        blocks.sort_by_key(|b| b.z.0);
        for i in start..blocks.len() {
            let z = drop(&blocks, &blocks[i]);
            let Block { z: (z0, z1), .. } = blocks[i];
            if z != z0 {
                stable = false;
                moved[i] = true;
                blocks[i].z.0 = z;
                blocks[i].z.1 = z1 - z0 + z;
            } else if stable {
                start = i;
            }
        }
        if stable {
            return moved.into_iter().map(|b| b as u32).sum();
        }
    }
}

fn drop(blocks: &[Block], block: &Block) -> u32 {
    blocks
        .iter()
        .filter(|b| b.z.1 < block.z.0)
        .map(|b: &Block| {
            let x_out = b.x.1 < block.x.0 || block.x.1 < b.x.0;
            let y_out = b.y.1 < block.y.0 || block.y.1 < b.y.0;
            let dont_overlap = x_out || y_out;
            return if dont_overlap { 1 } else { b.z.1 + 1 };
        })
        .max()
        .unwrap_or(1)
}
