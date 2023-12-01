use itertools::Itertools;

fn main() {
    let input = include_str!("input.txt");
    let result = part1(input);
    println!("{}", result)
}

fn part1(input: &str) -> u32 {
    input
        .lines()
        .map(|l| l.chars().filter(|c| c.is_ascii_digit()).collect_vec())
        .fold(0, |acc, v| {
            let num = format!("{}{}", v.first().unwrap(), v.last().unwrap());
            acc + num.parse::<u32>().unwrap()
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let input = "1abc2
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet";

        assert_eq!(142, part1(input))
    }
}
