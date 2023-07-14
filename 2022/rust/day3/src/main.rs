use std::{fs, collections::HashSet};


fn part1(input: &str) -> i32 {
    let content = fs::read_to_string(input).expect("failed");
    let rucks: Vec<(&str, &str)> = content.split("\n").map(|l| l.split_at(l.len() / 2)).collect();
    let mut sum = 0;
    for ruck in rucks {
        let ruck1: HashSet<u8> = ruck.0.bytes().into_iter().collect();
        let ruck2: HashSet<u8> = ruck.1.bytes().into_iter().collect();
        ruck1.intersection(&ruck2).for_each(|x| if x.is_ascii_uppercase() {
            sum = sum + *x as i32 - b'A' as i32 + 1i32 + 26i32;
        }else {
            sum = sum + *x as i32 - b'a' as i32 + 1i32;
        });
    }
    return sum;
}

fn part2(input: &str) -> i32 {
    fs::read_to_string(input).expect("failed")
        .lines()
        .map(|s| s.to_string())
        .collect::<Vec<String>>()
        .chunks_exact(3)
        .filter_map(|s| {
            s[0].chars().find(|&c| s[1].contains(c) && s[2].contains(c))
        })
        .map(|badge| ((badge as i32 - 39)% 58 + 1))
        .sum()
}

fn main() {
    let result1 = part1("input3.txt");
    let result2 = part2("input3.txt");

    println!("Result 1: {result1}");
    println!("Result 2: {result2}")
}
