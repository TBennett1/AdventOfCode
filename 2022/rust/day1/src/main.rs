use std::fs;

fn part1(input: &str) -> i32 {
    let content = fs::read_to_string(input).expect("failed to read file");
    let elves: Vec<&str> = content.split("\n\n").collect();
    let max: i32 = elves.into_iter().map(|l| l.split("\n").map(|c| c.parse::<i32>().unwrap()).sum()).max().unwrap();
    return max;
}

fn part2(input: &str) -> i32 {
    let content: String = fs::read_to_string(input).expect("failed");
    let elves: Vec<&str> = content.split("\n\n").collect();
    let mut cals: Vec<i32> = elves.into_iter().map(|l| l.split("\n").map(|c| c.parse::<i32>().unwrap()).sum()).collect::<Vec<i32>>();
    cals.sort_by(|a, b| b.cmp(a));
    cals.truncate(3);
    return cals.into_iter().sum();
}


fn main() {
    let result1 = part1("input1.txt");
    let result2 = part2("input1.txt");

    println!("Result 1: {result1}");
    println!("Result 2: {result2}")
}
