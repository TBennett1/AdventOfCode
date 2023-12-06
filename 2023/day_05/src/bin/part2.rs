use std::ops::Range;

use nom::{
    bytes::complete::take_until,
    character::complete::{self, line_ending, space1},
    multi::{many1, separated_list1},
    sequence::{separated_pair, tuple},
    IResult, Parser,
};
use nom_supreme::{tag::complete::tag, ParserExt};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

struct SeedMap {
    mappings: Vec<(Range<u64>, Range<u64>)>,
}

impl SeedMap {
    fn translate(&self, source: u64) -> u64 {
        let valid_mapping = self
            .mappings
            .iter()
            .find(|(source_range, _)| source_range.contains(&source));

        let Some((source_range, dest_range)) = valid_mapping
        else{
            return source;
        };

        let offset = source - source_range.start;
        dest_range.start + offset
    }
}

// 50 98 2
fn parse_range(input: &str) -> IResult<&str, (Range<u64>, Range<u64>)> {
    let (input, (dest, source, num)) = tuple((
        complete::u64,
        complete::u64.preceded_by(tag(" ")),
        complete::u64.preceded_by(tag(" ")),
    ))(input)?;

    Ok((input, (source..(source + num), dest..(dest + num))))
}

// seed-to-soil map:
// 50 98 2
// 52 50 48
fn parse_map(input: &str) -> IResult<&str, SeedMap> {
    take_until("map:")
        .precedes(tag("map:"))
        .precedes(many1(line_ending.precedes(parse_range)).map(|mappings| SeedMap { mappings }))
        .parse(input)
}

// all the maps
fn parse_maps(input: &str) -> IResult<&str, (Vec<Range<u64>>, Vec<SeedMap>)> {
    let (input, seeds) = tag("seeds: ")
        .precedes(separated_list1(
            space1,
            separated_pair(complete::u64, tag(" "), complete::u64)
                .map(|(start, offset)| start..(start + offset)),
        ))
        .parse(input)?;
    let (input, maps) = many1(parse_map)(input)?;

    Ok((input, (seeds, maps)))
}

fn process(input: &str) -> u64 {
    let (_, (seeds, maps)) = parse_maps(input).expect("valid parse");

    let min_location = seeds
        .into_par_iter()
        .flat_map(|range| range)
        .map(|seed| maps.iter().fold(seed, |seed, map| map.translate(seed)))
        .min();

    min_location.expect("should have a min value")
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
        let input = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";
        assert_eq!(46, process(input));
    }
}
