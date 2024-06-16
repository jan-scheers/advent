pub mod day1;
pub mod day10;
pub mod day11;
pub mod day12;
pub mod day13;
pub mod day14;
pub mod day15;
pub mod day16;
pub mod day17;
pub mod day18;
pub mod day19;
pub mod day2;
pub mod day20;
pub mod day21;
pub mod day22;
pub mod day23;
pub mod day24;
pub mod day25;
pub mod day3;
pub mod day4;
pub mod day5;
pub mod day6;
pub mod day7;
pub mod day8;
pub mod day9;

const SESSION: &str = "session=53616c7465645f5f1b5e774f9590b3208d567b2e2dcd60ce52d10c6aed45725f2709551eaa39f78c528b06bb5639c7e83dd90f7080464df4314ff7c7afaa1629";
pub fn get_day(day: u32) -> String {
    let client = reqwest::blocking::Client::new();
    let url = format!("https://adventofcode.com/2023/day/{}/input", day);
    client
        .get(url)
        .header("Cookie", SESSION)
        .send()
        .map(|r| r.text().unwrap_or_default())
        .unwrap_or_default()
}
