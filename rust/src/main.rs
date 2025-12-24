use std::time::Instant;

fn main() {
    let input = aoc::y25::get_day(10);
    let start = Instant::now();
    let part_one = aoc::y25::day10::part_one(&input);
    println!("Part 1: {} in {:?}", part_one, start.elapsed());
    let start = Instant::now();
    let part_two = aoc::y25::day10::part_two(&input);
    println!("Part 2: {} in {:?}", part_two, start.elapsed());
}
