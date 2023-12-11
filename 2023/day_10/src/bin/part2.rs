use std::{
    collections::{HashMap, HashSet},
    iter::successors,
};

use glam::IVec2;
use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, combinator::all_consuming,
    multi::many1, sequence::terminated, IResult, Parser,
};
use nom_locate::LocatedSpan;

enum Status {
    In,
    Out,
}

#[derive(Eq, PartialEq, Debug)]
enum Pipe {
    Horizontal,
    Vertical,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    StartingPosition,
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug)]
struct PipeInfo<'a> {
    span: SpanIVec2<'a>,
    pipe_type: Pipe,
}

type SpanIVec2<'a> = LocatedSpan<&'a str, IVec2>;
type Span<'a> = LocatedSpan<&'a str>;

fn with_xy(span: Span) -> SpanIVec2 {
    let x = span.get_column() as i32 - 1;
    let y = span.location_line() as i32 - 1;
    span.map_extra(|_| IVec2::new(x, y))
}

fn parse_grid(input: Span) -> IResult<Span, HashMap<IVec2, Pipe>> {
    let (input, pipes) = all_consuming(many1(terminated(
        alt((
            tag("|").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::Vertical,
            }),
            tag("-").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::Horizontal,
            }),
            tag("L").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::NorthEast,
            }),
            tag("J").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::NorthWest,
            }),
            tag("7").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::SouthWest,
            }),
            tag("F").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::SouthEast,
            }),
            tag(".").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::Ground,
            }),
            tag("S").map(with_xy).map(|span| PipeInfo {
                span,
                pipe_type: Pipe::StartingPosition,
            }),
        )),
        multispace0,
    )))(input)?;

    Ok((
        input,
        pipes
            .into_iter()
            .map(|pipe_info| (pipe_info.span.extra, pipe_info.pipe_type))
            .collect(),
    ))
}

fn process(input: &str) -> usize {
    let (_, grid) = parse_grid(Span::new(input)).expect("should parse");

    let start_position = grid
        .iter()
        .find_map(|(key, value)| (value == &Pipe::StartingPosition).then_some(key))
        .expect("should have a starting position");

    let north = *start_position + IVec2::new(0, -1);
    let north_position = grid
        .get(&north)
        .is_some_and(|pipe_type| {
            matches!(
                pipe_type,
                Pipe::Vertical | Pipe::SouthWest | Pipe::SouthEast
            )
        })
        .then_some((Direction::South, north));

    let south = *start_position + IVec2::new(0, 1);
    let south_position = grid
        .get(&south)
        .is_some_and(|pipe_type| {
            matches!(
                pipe_type,
                Pipe::Vertical | Pipe::NorthWest | Pipe::NorthEast
            )
        })
        .then_some((Direction::North, south));

    let east = *start_position + IVec2::new(1, 0);
    let east_position = grid
        .get(&east)
        .is_some_and(|pipe_type| {
            matches!(
                pipe_type,
                Pipe::Horizontal | Pipe::NorthWest | Pipe::SouthWest
            )
        })
        .then_some((Direction::West, east));

    let west = *start_position + IVec2::new(-1, 0);
    let west_position = grid
        .get(&west)
        .is_some_and(|pipe_type| {
            matches!(
                pipe_type,
                Pipe::Horizontal | Pipe::NorthEast | Pipe::SouthEast
            )
        })
        .then_some((Direction::East, west));

    let mut iters = vec![north_position, south_position, east_position, west_position]
        .into_iter()
        .flatten()
        .map(|tuple| {
            successors(Some(tuple), |(from_direction, curr_loc)| {
                let pipe_type = grid.get(curr_loc).expect("should exist");

                let direction_to_go = match (from_direction, pipe_type) {
                    (Direction::North, Pipe::Vertical) => Direction::South,
                    (Direction::North, Pipe::NorthEast) => Direction::East,
                    (Direction::North, Pipe::NorthWest) => Direction::West,
                    (Direction::East, Pipe::Horizontal) => Direction::West,
                    (Direction::East, Pipe::NorthEast) => Direction::North,
                    (Direction::East, Pipe::SouthEast) => Direction::South,
                    (Direction::South, Pipe::Vertical) => Direction::North,
                    (Direction::South, Pipe::SouthWest) => Direction::West,
                    (Direction::South, Pipe::SouthEast) => Direction::East,
                    (Direction::West, Pipe::Horizontal) => Direction::East,
                    (Direction::West, Pipe::NorthWest) => Direction::North,
                    (Direction::West, Pipe::SouthWest) => Direction::South,
                    _ => unreachable!(),
                };

                Some(match direction_to_go {
                    Direction::North => (Direction::South, *curr_loc + IVec2::new(0, -1)),
                    Direction::East => (Direction::West, *curr_loc + IVec2::new(1, 0)),
                    Direction::South => (Direction::North, *curr_loc + IVec2::new(0, 1)),
                    Direction::West => (Direction::East, *curr_loc + IVec2::new(-1, 0)),
                })
            })
        });

    let path_a = iters.next().expect("path should exist");
    let path_b = iters.next().expect("path should exist");

    let zip = path_a.zip(path_b);
    let mut pipe_location: HashSet<IVec2> = HashSet::from([*start_position]);

    for (path_a_node, path_b_node) in zip {
        pipe_location.insert(path_a_node.1);
        pipe_location.insert(path_b_node.1);

        if path_a_node.1 == path_b_node.1 {
            break;
        }
    }

    input
        .lines()
        .enumerate()
        .map(|(y, line)| {
            let mut status = Status::Out;

            line.chars()
                .enumerate()
                .filter(|(x, _)| {
                    let position = IVec2::new(*x as i32, y as i32);
                    let pipe_type = grid.get(&position).expect("should be valid tile");
                    if pipe_location.contains(&position) {
                        if [
                            Pipe::StartingPosition,
                            Pipe::Vertical,
                            Pipe::SouthEast,
                            Pipe::SouthWest,
                        ]
                        .contains(pipe_type)
                        {
                            status = match status {
                                Status::In => Status::Out,
                                Status::Out => Status::In,
                            }
                        }
                        false
                    } else {
                        match status {
                            Status::In => true,
                            Status::Out => false,
                        }
                    }
                })
                .count()
        })
        .sum::<usize>()
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
    fn test_process_simple() {
        let input = "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........";
        assert_eq!(4, process(input))
    }

    #[test]
    fn test_process_large() {
        let input = ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...";
        assert_eq!(8, process(input))
    }

    #[test]
    fn test_process_complex() {
        let input = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L";

        assert_eq!(10, process(input))
    }
}
