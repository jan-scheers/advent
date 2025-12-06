pub mod day1;

const SESSION: &str = "_ga=GA1.2.849613819.1765045556; _gid=GA1.2.85157165.1765045556; _ga_MHSNPJKWC7=GS2.2.s1765045556$o1$g1$t1765045954$j60$l0$h0; session=53616c7465645f5f27a4a6ea630cef70bb822da39dbd9057bdcc38e89f16ffc15d52f50a1291dbd6685f6bf7f52df198df2f0ee474595ec2c57e9f06501427f8";

pub fn get_day(day: u32) -> String {
    let client = reqwest::blocking::Client::new();
    let url = format!("https://adventofcode.com/2025/day/{}/input", day);
    client
        .get(url)
        .header("Cookie", SESSION)
        .send()
        .map(|r| r.text().unwrap_or_default())
        .unwrap_or_default()
}
