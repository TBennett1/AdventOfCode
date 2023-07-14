use std::fs;
use itertools::Itertools;

fn part1(input: &str) -> usize {
    let content = fs::read_to_string(input).expect("failed").chars().collect_vec();
    content.windows(4).enumerate().filter(|(_, window)| window.into_iter().all_unique()).map(|(i,_)| i+4).next().unwrap()
}

fn part2(input: &str) -> usize {
    let content = fs::read_to_string(input).expect("failed").chars().collect_vec();
    content.windows(14).enumerate().filter(|(_, window)| window.into_iter().all_unique()).map(|(i,_)| i+14).next().unwrap()
}


fn main() {
    let result1 = part1("input.txt");
    let result2 = part2("input.txt");

    println!("Result 1: {result1:?}");
    println!("Result 2: {result2}")
}