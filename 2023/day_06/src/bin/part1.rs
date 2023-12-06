use nom::{
    bytes::complete::tag,
    character::complete::{self, space1},
    multi::separated_list1,
    sequence::{pair, preceded},
    IResult,
};

fn farthest(time: u64, distance: u64) -> u64 {
    (0..time + 1).fold(0, |acc, time_held| {
        let dist_covered = time_held * (time - time_held);
        if dist_covered > distance {
            acc + 1
        } else {
            acc
        }
    })
}

fn parse(input: &str) -> IResult<&str, (Vec<u64>, Vec<u64>)> {
    pair(
        preceded(
            pair(tag("Time:"), space1),
            separated_list1(space1, complete::u64),
        ),
        preceded(
            pair(tag("\nDistance:"), space1),
            separated_list1(space1, complete::u64),
        ),
    )(input)
}

fn process(input: &str) -> u64 {
    let (_, (times, distances)) = parse(input).expect("should be valid parse");
    let time_dist_vec: Vec<(u64, u64)> = times.into_iter().zip(distances.into_iter()).collect();
    time_dist_vec
        .into_iter()
        .map(|(time, dist)| farthest(time, dist))
        .product()
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
        let input = "Time:      7  15   30
Distance:  9  40  200";

        assert_eq!(288, process(input))
    }
}
