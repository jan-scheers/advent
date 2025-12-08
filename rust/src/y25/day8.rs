pub const INPUT: &str = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689";

use std::collections::HashMap;

type Graph = HashMap<usize, HashMap<usize, i64>>;

struct Vec3 {
    x: i64,
    y: i64,
    z: i64,
}

fn dist(a: &Vec3, b: &Vec3) -> i64 {
    (a.x - b.x).pow(2) + (a.y - b.y).pow(2) + (a.z - b.z).pow(2)
}

fn parse(input: &str) -> (Vec<Vec3>, Vec<(i64, (usize, usize))>) {
    let points: Vec<Vec3> = input
        .lines()
        .map(|line| {
            let [x, y, z] = line
                .split(',')
                .map(|n| n.parse().unwrap())
                .collect::<Vec<_>>()[..]
            else {
                panic!();
            };
            Vec3 { x, y, z }
        })
        .collect();
    let mut pairs = (0..points.len())
        .flat_map(|i| {
            (0..i)
                .map(|j| (dist(&points[i], &points[j]), (i, j)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    pairs.sort_unstable();
    (points, pairs)
}

fn insert(graphs: &mut Vec<Graph>, i: usize, j: usize, dist: i64) {
    let ix = (0..graphs.len())
        .filter(|&k| graphs[k].contains_key(&i) || graphs[k].contains_key(&j))
        .collect::<Vec<_>>();
    match ix.len() {
        0 => {
            graphs.push(HashMap::from([
                (i, HashMap::from([(j, dist)])),
                (j, HashMap::from([(i, dist)])),
            ]));
        }
        1 => {
            let g: &mut Graph = &mut graphs[ix[0]];
            g.entry(i).or_default().insert(j, dist);
            g.entry(j).or_default().insert(i, dist);
        }
        2 => {
            let g2: Graph = graphs.remove(ix[1]);
            let g: &mut Graph = &mut graphs[ix[0]];
            g.extend(g2);
            g.entry(i).or_default().insert(j, dist);
            g.entry(j).or_default().insert(i, dist);
        }
        _ => {
            panic!();
        }
    }
}

pub fn part_one(input: &str, n: usize) -> usize {
    let (_, pairs) = parse(input);

    let mut graphs: Vec<Graph> = vec![];
    for (dist, (i, j)) in pairs.into_iter().take(n) {
        insert(&mut graphs, i, j, dist);
    }
    graphs.sort_by_key(|g: &Graph| -(g.len() as i64));
    graphs[..3].iter().map(|g: &Graph| g.len()).product()
}

pub fn part_two(input: &str) -> i64 {
    let (points, pairs) = parse(input);

    let mut graphs: Vec<Graph> = vec![];
    for (dist, (i, j)) in pairs {
        insert(&mut graphs, i, j, dist);
        if graphs[0].len() == points.len() {
            return points[i].x * points[j].x;
        }
    }
    panic!();
}
