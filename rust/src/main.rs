use std::time::Instant;

fn main() {
    let input = aoc::y25::get_day(12);
    let start = Instant::now();
    let part_one = aoc::y25::day12::part_one(&input);
    println!("Part 1: {} in {:?}", part_one, start.elapsed());
}
