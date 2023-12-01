use itertools::Itertools;

fn main() {
    let input = include_str!("input.txt");
    println!("{}", part2(input))
}

fn part2(input: &str) -> u32 {
    input.lines().map(process_line).sum()
}

fn process_line(line: &str) -> u32 {
    let it = (0..line.len())
        .filter_map(|index| {
            let reduced_line = &line[index..];

            let result = if reduced_line.starts_with("one") {
                '1'
            } else if reduced_line.starts_with("two") {
                '2'
            } else if reduced_line.starts_with("three") {
                '3'
            } else if reduced_line.starts_with("four") {
                '4'
            } else if reduced_line.starts_with("five") {
                '5'
            } else if reduced_line.starts_with("six") {
                '6'
            } else if reduced_line.starts_with("seven") {
                '7'
            } else if reduced_line.starts_with("eight") {
                '8'
            } else if reduced_line.starts_with("nine") {
                '9'
            } else {
                reduced_line.chars().next().unwrap()
            };

            result.to_digit(10)
        })
        .collect_vec();
    let first = it.first().unwrap();
    let last = it.last().unwrap();
    first * 10 + last
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part2_test() {
        let input = "two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen";

        assert_eq!(281, part2(input))
    }
}
