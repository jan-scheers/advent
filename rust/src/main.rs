impl Solution {
    pub fn kth_smallest_prime_fraction(arr: Vec<i32>, k: i32) -> Vec<i32> {
        let mut idx = vec![0; arr.len()];
        let key = |(i, j)| arr[i] as f64 / arr[j] as f64;
        for cnt in 0.. {
            let i = idx[arr.len() - 1];
            let (i, j) = (0..arr.len() - 1)
                .rev()
                .fold((i, (i, arr.len() - 1)), |(h, best), j| {
                    if idx[j] < h {
                        let h = idx[j];
                        if key((h, j)) < key(best) {
                            (h, (h, j))
                        } else {
                            (h, best)
                        }
                    } else {
                        (h, best)
                    }
                })
                .1;
            println!("{} / {}", arr[i], arr[j]);
            if cnt == k - 1 {
                return vec![arr[i], arr[j]];
            }
            idx[j] = i + 1;
        }
        return vec![];
    }
}

fn main() {
    //let input = aoc::y22::day16::INPUT;

    let input = &aoc::y22::get_day(16);
    Solution::kth_smallest_prime_fraction(vec![1, 2, 3, 5], 3);
    //println!("{:?}", aoc::y22::day15::parse(input));
    //println!("part one: {:?}", aoc::y22::day16::part_one(input));
    //println!("part two: {:?}", aoc::y22::day15::part_two(input, 4000000));
}
struct Solution;

use aoc::TreeNode;
use num::Integer;
