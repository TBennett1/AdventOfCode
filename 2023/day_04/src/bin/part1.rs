use std::collections::HashSet;

use nom::{
    bytes::complete::tag,
    character::complete::{self, digit1, line_ending, space0, space1},
    multi::{fold_many1, separated_list1},
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, PartialEq)]
struct Card {
    winning_numbers: HashSet<u32>,
    our_numbers: HashSet<u32>,
}

impl Card {
    fn score(&self) -> u32 {
        let power = self.winning_numbers.intersection(&self.our_numbers).count() as u32;

        match power.checked_sub(1) {
            Some(num) => 2u32.pow(num),
            None => 0,
        }
    }
}

// 41 48 83 86 17
fn numbers(input: &str) -> IResult<&str, HashSet<u32>> {
    fold_many1(
        terminated(complete::u32, space0),
        HashSet::new,
        |mut acc, item| {
            acc.insert(item);
            acc
        },
    )(input)
}

// Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
fn card(input: &str) -> IResult<&str, Card> {
    let (input, _) = delimited(
        tuple((tag("Card"), space1)),
        digit1,
        tuple((tag(":"), space1)),
    )(input)?;
    let (input, (winning_numbers, our_numbers)) =
        separated_pair(numbers, tuple((tag("|"), space1)), numbers)(input)?;
    Ok((
        input,
        Card {
            winning_numbers,
            our_numbers,
        },
    ))
}

fn parse_cards(input: &str) -> IResult<&str, Vec<Card>> {
    let (input, cards) = separated_list1(line_ending, card)(input)?;
    Ok((input, cards))
}

fn process(input: &str) -> u32 {
    let cards = parse_cards(input).expect("should parse").1;
    cards.iter().map(|c| c.score()).sum()
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
    fn parser_test_min() {
        let input = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";

        let values = parse_cards(input).expect("should parse").1;
        assert_eq!(
            values,
            vec![Card {
                winning_numbers: HashSet::from_iter(vec![1, 21, 53, 59, 44]),
                our_numbers: HashSet::from_iter(vec![69, 82, 63, 72, 16, 21, 14, 1])
            }]
        )
    }

    #[test]
    fn parser_test() {
        let input = include_str!("../../input.txt");
        let values = parse_cards(input).expect("should be parsable").1;
        assert_eq!(218, values.len())
    }

    #[test]
    fn test() {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

        assert_eq!(13, process(input))
    }
}
