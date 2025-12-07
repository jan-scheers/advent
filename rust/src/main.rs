fn main() {
    let input = aoc::y25::get_day(7);
    let (count, sum) = aoc::y25::day7::solve(&input);
    println!("{} {}", count, sum);
}
