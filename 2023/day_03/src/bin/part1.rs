use glam::IVec2;
use nom::{
    branch::alt,
    bytes::complete::{is_not, take_till1},
    character::complete::digit1,
    combinator::iterator,
    IResult, Parser,
};
use nom_locate::LocatedSpan;
use std::collections::HashSet;

type Span<'a> = LocatedSpan<&'a str>;
type SpanIVec2<'a> = LocatedSpan<&'a str, IVec2>;

#[derive(Debug, PartialEq)]
enum Value<'a> {
    Empty,
    Symbol(SpanIVec2<'a>),
    Number(SpanIVec2<'a>),
}

fn with_xy(span: Span) -> SpanIVec2 {
    let x = span.get_column() as i32 - 1;
    let y = span.location_line() as i32 - 1;
    span.map_extra(|_| IVec2::new(x, y))
}

fn parse_grid(input: Span) -> IResult<Span, Vec<Value>> {
    let mut it = iterator(
        input,
        alt((
            digit1.map(with_xy).map(Value::Number),
            is_not(".\n0123456789").map(with_xy).map(Value::Symbol),
            take_till1(|c: char| c.is_ascii_digit() || c != '.' && c != '\n').map(|_| Value::Empty),
        )),
    );

    let parsed = it
        .filter(|value| value != &Value::Empty)
        .collect::<Vec<Value>>();
    let res = it.finish();
    res.map(|(input, _)| (input, parsed))
}

fn part1(input: &str) -> u32 {
    let objects = parse_grid(Span::new(input)).unwrap().1;

    let symbol_map = objects
        .iter()
        .filter_map(|value| match value {
            Value::Empty => None,
            Value::Symbol(sym) => Some(sym.extra),
            Value::Number(_) => None,
        })
        .collect::<HashSet<IVec2>>();

    let result = objects
        .iter()
        .filter_map(|value| {
            let Value::Number(num) = value else{
            return None;
        };
            let surrounding_positions = [
                // east
                IVec2::new(num.fragment().len() as i32, 0),
                //west
                IVec2::new(-1, 0),
            ]
            .into_iter()
            .chain(
                // north
                (-1..=num.fragment().len() as i32).map(|x_offset| IVec2::new(x_offset, 1)),
            )
            .chain(
                //south
                (-1..=num.fragment().len() as i32).map(|x_offset| IVec2::new(x_offset, -1)),
            )
            .map(|pos| pos + num.extra)
            .collect::<Vec<IVec2>>();

            surrounding_positions
                .iter()
                .any(|pos| symbol_map.contains(pos))
                .then_some(num.fragment().parse::<u32>().expect("should be a number"))
        })
        .sum::<u32>();
    result
}

fn main() {
    let input = include_str!("../../input.txt");
    let result = part1(input);
    println!("{result}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_min() {
        let input = "405*815";
        let values = parse_grid(Span::new(input)).expect("").1;
        assert_eq!(
            values,
            vec![
                Value::Number(unsafe {
                    LocatedSpan::new_from_raw_offset(0, 1, "405", IVec2::new(0, 0))
                }),
                Value::Symbol(unsafe {
                    LocatedSpan::new_from_raw_offset(3, 1, "*", IVec2::new(3, 0))
                }),
                Value::Number(unsafe {
                    LocatedSpan::new_from_raw_offset(4, 1, "815", IVec2::new(4, 0))
                })
            ]
        )
    }

    #[test]
    fn test_parser() {
        let input = include_str!("../../input.txt");
        let values = parse_grid(Span::new(input)).expect("").1;
        assert_eq!(
            1210,
            values
                .iter()
                .filter_map(|v| match v {
                    Value::Empty => None,
                    Value::Symbol(_) => None,
                    Value::Number(_) => Some(()),
                })
                .count()
        );
    }

    #[test]
    fn test() {
        let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";

        assert_eq!(4361, part1(input))
    }
}
