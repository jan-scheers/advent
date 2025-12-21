use faer::linalg::solvers::Solve;
use faer::perm::{permute_cols, permute_rows, Perm};
use faer::prelude::SolveLstsq;
use faer::{concat, Col, Mat};
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
enum SimplexState {
    Optimal,
    Pivot,
}

#[derive(Debug)]
struct SimplexStep {
    x: Mat<f64>,
    perm: Perm<usize>,
    state: SimplexState,
}

impl SimplexStep {
    pub fn slice(self, m: usize, n: usize) -> SimplexStep {
        let (a, _) = self.perm.into_arrays();
        let a = a
            .iter()
            .filter_map(|i| i.checked_sub(m))
            .collect::<Vec<usize>>()
            .into_boxed_slice();
        let mut b = a.clone();
        for (i, &a) in a.iter().enumerate() {
            b[a] = i;
        }

        SimplexStep {
            x: self.x.subrows(m, n).to_owned(),
            perm: Perm::new_checked(a, b, n),
            state: SimplexState::Pivot,
        }
    }
}

#[derive(Debug)]
pub enum Quality {
    Optimal,
    BFS(String),
    Lstsq(String),
}
impl fmt::Display for Quality {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn simplex(a: &Mat<f64>, b: &Mat<f64>, c: &Mat<f64>) -> (Quality, Col<f64>) {
    let (m, n) = a.shape();
    assert_eq!(b.shape(), (m, 1));
    assert_eq!(c.shape(), (n, 1));
    let eye: Mat<f64> = Mat::identity(m, m);
    let a_ = concat![[eye, a]];
    let (c_1, c_0): (Mat<f64>, Mat<f64>) = (Mat::ones(m, 1), Mat::zeros(n, 1));
    let c_: Mat<f64> = concat![[c_1], [c_0]];
    let sol = SimplexStep {
        perm: identity(m + n),
        x: concat![[b], [c_0]],
        state: SimplexState::Pivot,
    };
    let sol = match solve(&a_, &c_, sol) {
        Ok(sol) => sol.slice(m, n),
        Err(e) => {
            let x = a.col_piv_qr().solve_lstsq(b.col(0).as_ref());
            return (Quality::Lstsq(e), x);
        }
    };
    let bfs = sol.x.col(0).to_owned();
    match solve(&a, &c, sol) {
        Ok(sol) => (Quality::Optimal, sol.x.col(0).to_owned()),
        Err(e) => (Quality::BFS(e), bfs),
    }
}

fn solve(a: &Mat<f64>, c: &Mat<f64>, mut x_0: SimplexStep) -> Result<SimplexStep, String> {
    for _ in 0..1000 {
        x_0 = step(&a, &c, x_0)?;
        if x_0.state == SimplexState::Optimal {
            return Ok(x_0);
        }
    }
    Err("No convergence".to_string())
}

fn step(a: &Mat<f64>, c: &Mat<f64>, x_0: SimplexStep) -> Result<SimplexStep, String> {
    let SimplexStep { x, perm, .. } = x_0;
    let (m, n) = a.shape();
    let (mut a_, mut c_, mut x_) = (a.clone(), c.clone(), x.clone());
    permute_cols(a_.as_mut(), a.as_ref(), perm.as_ref());
    permute_rows(c_.as_mut(), c.as_ref(), perm.as_ref());
    permute_rows(x_.as_mut(), x.as_ref(), perm.as_ref());

    let (a_b, a_n) = a_.split_at_col(m);
    let (c_b, c_n) = c_.split_at_row(m);

    let lambda = a_b.transpose().partial_piv_lu().solve(c_b.as_ref());
    let s_n = c_n - a_n.transpose() * lambda.as_ref();
    let s_n = s_n.col(0);
    //println!("a_b: {:?}", &a_b);
    //println!("lambda: {:?}", &lambda);
    //println!("s_n: {:?}", &s_n);
    if s_n.iter().all(|s| s >= &0.0) {
        return Ok(SimplexStep {
            x,
            perm,
            state: SimplexState::Optimal,
        });
    };
    let i_en = (0..s_n.nrows())
        .fold((f64::INFINITY, 0), |acc, i| {
            if s_n[i] < acc.0 {
                (s_n[i], i)
            } else {
                acc
            }
        })
        .1;
    let d_en = a_b.partial_piv_lu().solve(a_n.col(i_en).as_ref());

    let mut x = x_.col_mut(0);
    let i_ex = (0..m).fold((f64::INFINITY, 0), |acc, i| {
        let d = d_en[i];
        let v = x[i];
        if d > 0.0 && v / d < acc.0 {
            (v / d, i)
        } else {
            acc
        }
    });
    if i_ex.0 == f64::INFINITY {
        return Err("Unbounded".to_string());
    }
    for i in 0..m {
        x[i] -= d_en[i] * i_ex.0;
    }
    x[i_en + m] = i_ex.0;
    let mut x = Mat::zeros(n, 1);
    let perm_inverse = perm.clone().into_inverse();
    permute_rows(x.as_mut(), x_.as_ref(), perm_inverse.as_ref());
    let perm = perm * swap(i_ex.1, i_en + m, n);
    Ok(SimplexStep {
        perm,
        x,
        state: SimplexState::Pivot,
    })
}

fn identity(n: usize) -> Perm<usize> {
    let id = (0..n).map(|k| k).collect::<Vec<usize>>().into_boxed_slice();
    Perm::new_checked(id.clone(), id, n)
}

fn swap(i: usize, j: usize, n: usize) -> Perm<usize> {
    let swap = (0..n)
        .map(|k| {
            if k == j {
                i
            } else if k == i {
                j
            } else {
                k
            }
        })
        .collect::<Vec<usize>>()
        .into_boxed_slice();
    Perm::new_checked(swap.clone(), swap, n)
}
