use std::collections::BTreeMap;

use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{self, alphanumeric1, line_ending, multispace1},
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
                alphanumeric1,
                tag(" = "),
                delimited(
                    tag("("),
                    separated_pair(alphanumeric1, tag(", "), alphanumeric1),
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

fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = lcm(&nums[1..]);
    a * b / gcd(a, b)
}

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd(b, a % b)
}

fn process(input: &str) -> usize {
    let (input, (directions, map)) = parse(input).expect("should parse");

    debug_assert_eq!(input, "");

    let starting_nodes = map
        .keys()
        .filter(|key| key.ends_with('A'))
        .cloned()
        .collect_vec();

    let result = starting_nodes
        .iter()
        .map(|node| {
            let mut visited_nodes = vec![*node];
            let mut curr_node = *node;

            directions
                .iter()
                .cycle()
                .enumerate()
                .find_map(|(idx, direction)| {
                    let options = map.get(curr_node).unwrap();
                    let next_node = match direction {
                        Direction::Left => options.0,
                        Direction::Right => options.1,
                    };
                    if next_node.ends_with('Z') {
                        Some(idx + 1)
                    } else {
                        visited_nodes.push(next_node);
                        curr_node = next_node;
                        None
                    }
                })
                .expect("should find a cycle")
        })
        .collect_vec();

    lcm(&result)
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
        let input = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)";

        assert_eq!(6, process(input))
    }
}
