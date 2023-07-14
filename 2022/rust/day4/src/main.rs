use std::fs;

use regex::Regex;


fn part1(input: &str) -> i32 {
    let mut sections = Vec::new();
    let r = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)").unwrap();
    let s = fs::read_to_string(input).expect("failed");
    
    for l in s.lines() {
        let pair = r.captures(l).unwrap();
        sections.push(((pair[1].parse::<i32>().unwrap(), pair[2].parse::<i32>().unwrap()), (pair[3].parse::<i32>().unwrap(), pair[4].parse::<i32>().unwrap())))
    }

    sections
        .iter()
        .filter_map(|((a1,a2), (b1,b2))| if a1 <= b1 && a2 >= b2 || b1 <= a1 && b2 >= a2 {
            Some(1)
        }else{
            None
        })
        .sum()
}

fn part2(input: &str) -> i32{
    let mut sections = Vec::new();
    let r = Regex::new(r"^(\d+)-(\d+),(\d+)-(\d+)").unwrap();
    let s = fs::read_to_string(input).expect("failed");
    
    for l in s.lines() {
        let pair = r.captures(l).unwrap();
        sections.push(((pair[1].parse::<i32>().unwrap(), pair[2].parse::<i32>().unwrap()), (pair[3].parse::<i32>().unwrap(), pair[4].parse::<i32>().unwrap())))
    }

    sections
        .iter()
        .filter_map(|((a1, a2), (b1, b2))| if a2 >= b1 && a1 <= b2 {
            Some(1)
        }else{
            None
        })
        .sum()
}


fn main() {
    let result1 = part1("input4.txt");
    let result2 = part2("input4.txt");

    println!("Result 1: {result1:?}");
    println!("Result 2: {result2}")
}