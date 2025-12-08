fn main() {
    let input = aoc::y25::get_day(8);
    let count = aoc::y25::day8::part_one(&input, 1000);
    let count2 = aoc::y25::day8::part_two(&input);
    println!("{} {}", count, count2);
}
