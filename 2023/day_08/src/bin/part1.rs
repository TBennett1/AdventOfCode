use std::collections::BTreeMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{self, alpha1, line_ending, multispace1},
    combinator::eof,
    multi::{fold_many1, many1},
    sequence::{delimited, separated_pair, terminated},
    IResult, Parser,
};

type NodeMap<'a> = BTreeMap<&'a str, (&'a str, &'a str)>;

enum Direction {
    Left,
    Right,
}

fn parse(input: &str) -> IResult<&str, (Vec<Direction>, NodeMap)> {
    let (input, directions) = many1(alt((
        complete::char('R').map(|_| Direction::Right),
        complete::char('L').map(|_| Direction::Left),
    )))(input)?;
    let (input, _) = multispace1(input)?;
    let (input, map) = fold_many1(
        terminated(
            separated_pair(
                alpha1,
                tag(" = "),
                delimited(
                    tag("("),
                    separated_pair(alpha1, tag(", "), alpha1),
                    tag(")"),
                ),
            ),
            alt((line_ending, eof)),
        ),
        BTreeMap::new,
        |mut acc: BTreeMap<&str, (&str, &str)>, (key, value)| {
            acc.insert(key, value);
            acc
        },
    )(input)?;

    Ok((input, (directions, map)))
}

fn process(input: &str) -> usize {
    let (input, (directions, map)) = parse(input).expect("should parse");

    debug_assert_eq!(input, "");

    let mut curr_node = "AAA";
    let Some(steps) = directions
        .iter()
        .cycle()
        .enumerate()
        .find_map(|(idx, direction)| {
            let options = map.get(curr_node).unwrap();
            let next_node = match direction {
                Direction::Left => options.0,
                Direction::Right => options.1,
            };
            if next_node == "ZZZ" {
                Some(idx + 1)
            } else {
                curr_node = next_node;
                None
            }
        })else{
            panic!()
        };
    steps
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
    fn process_test() {
        let input = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)";

        assert_eq!(2, process(input))
    }

    #[test]
    fn process_test2() {
        let input = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)";

        assert_eq!(6, process(input))
    }
}
