extern crate nalgebra as na;
use na::{DMatrix, DVector};

pub const INPUT: &str = "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3";

#[derive(Debug, PartialEq, Clone)]
pub struct Hail {
    p: (f64, f64, f64),
    v: (f64, f64, f64),
}

pub fn parse(input: &str) -> Vec<Hail> {
    input
        .trim()
        .lines()
        .map(|line| {
            let (p, v) = line.split_once(" @ ").unwrap();
            let [p, v] = [p, v]
                .map(|s| {
                    s.split(',')
                        .map(|s| s.trim().parse().unwrap())
                        .collect::<Vec<f64>>()
                })
                .to_owned();
            Hail {
                p: (p[0], p[1], p[2]),
                v: (v[0], v[1], v[2]),
            }
        })
        .collect()
}

fn solve_x(a: &Hail, b: &Hail) -> f64 {
    let (va, vb) = (a.v.1 / a.v.0, b.v.1 / b.v.0);
    (a.p.0 * va - b.p.0 * vb - a.p.1 + b.p.1) / (va - vb)
}

fn solve_y(a: &Hail, x: f64) -> f64 {
    (x - a.p.0) * a.v.1 / a.v.0 + a.p.1
}

fn solve_t(a: &Hail, x: f64) -> f64 {
    (x - a.p.0) / a.v.0
}

pub fn part_one(input: &str, low: f64, high: f64) -> i32 {
    let hail = parse(input);
    let mut count = 0;
    for i in 0..hail.len() - 1 {
        for j in i + 1..hail.len() {
            let (a, b) = (&hail[i], &hail[j]);
            let x = solve_x(a, b);
            let y = solve_y(a, x);
            if low <= x
                && x <= high
                && low <= y
                && y <= high
                && solve_t(a, x) >= 0.0
                && solve_t(b, x) >= 0.0
            {
                count += 1;
            }
        }
    }
    count
}

pub fn part_two(input: &str) -> i64 {
    let hail = &parse(input)[..3];
    let mut sol = na::DVector::from_vec(vec![1.0; 6 + hail.len()]);
    for i in 0..1000 {
        let delta = jac(&hail, &sol)
            .svd_unordered(true, true)
            .solve(&neg_fun(&hail, &sol), 0.1)
            .unwrap();
        if delta.norm_squared() < 1.0 {
            println!("{}", i);
            break;
        }
        sol += delta;
    }
    let sol: Vec<i64> = sol.iter().map(|f| f.round() as i64).collect();
    println!("{}", na::DVector::from_vec(sol.clone()));
    let [_a, b, _c, d, _e, f] = sol[..6] else {
        panic!("");
    };
    b + d + f
}

fn neg_fun(hail: &[Hail], sol: &DVector<f64>) -> DVector<f64> {
    let sol = sol.as_slice();
    let [a, b, c, d, e, f] = sol[..6] else {
        panic!("boo");
    };
    let t = &sol[6..];
    DVector::from_iterator(
        3 * hail.len(),
        [].into_iter()
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .map(|(t, hail)| (hail.v.0 - a) * t + (hail.p.0 - b)),
            )
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .map(|(t, hail)| (hail.v.1 - c) * t + (hail.p.1 - d)),
            )
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .map(|(t, hail)| (hail.v.2 - e) * t + (hail.p.2 - f)),
            ),
    )
}

fn jac(hail: &[Hail], sol: &DVector<f64>) -> DMatrix<f64> {
    let sol = sol.as_slice();
    let [a, _b, c, _d, e, _f] = sol[..6] else {
        panic!("Boo");
    };
    let t = &sol[6..];
    DMatrix::from_iterator(
        sol.len(),
        3 * hail.len(),
        [].into_iter()
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .enumerate()
                    .flat_map(|(i, (t, hail))| {
                        let mut v = vec![0.0; sol.len()];
                        v[0] = *t;
                        v[1] = 1.0;
                        v[6 + i] = a - hail.v.0;
                        v
                    }),
            )
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .enumerate()
                    .flat_map(|(i, (t, hail))| {
                        let mut v = vec![0.0; sol.len()];
                        v[2] = *t;
                        v[3] = 1.0;
                        v[6 + i] = c - hail.v.1;
                        v
                    }),
            )
            .chain(
                t.iter()
                    .zip(hail.iter())
                    .enumerate()
                    .flat_map(|(i, (t, hail))| {
                        let mut v = vec![0.0; sol.len()];
                        v[4] = *t;
                        v[5] = 1.0;
                        v[6 + i] = e - hail.v.2;
                        v
                    }),
            ),
    )
    .transpose()
}
