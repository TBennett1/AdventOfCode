use itertools::{Itertools, Position};
use nom::{
    character::complete::{self, line_ending, space1},
    multi::separated_list1,
    IResult,
};

fn history(input: &str) -> IResult<&str, Vec<i64>> {
    separated_list1(space1, complete::i64)(input)
}

fn find_next_value(mut first_num: Vec<i64>, mut history: Vec<i64>) -> i64 {
    history = history
        .iter()
        .tuple_windows()
        .with_position()
        .map(|(position, (left, right))| {
            dbg!(&position, &left, &right);
            match position {
                Position::Last | Position::Only => first_num.push(*right),
                _ => {}
            };
            right - left
        })
        .collect_vec();

    if history.iter().all(|&x| x == 0) {
        return first_num.iter().fold(0, |acc, &x| x + acc);
    } else {
        find_next_value(first_num, history)
    }
}

fn process(input: &str) -> i64 {
    let (input, histories) = separated_list1(line_ending, history)(input).expect("valid parse");

    debug_assert_eq!(input, "");

    histories
        .into_iter()
        .map(|history| find_next_value(vec![], history))
        .sum::<i64>()
}

fn main() {
    let input = include_str!("../../input.txt");
    let result = process(input);
    println!("{result}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process() {
        let input = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45";
        assert_eq!(114, process(input))
    }
}
