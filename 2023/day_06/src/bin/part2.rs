use nom::{
    bytes::complete::tag,
    character::complete::{digit1, space1},
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

fn parse(input: &str) -> IResult<&str, (Vec<&str>, Vec<&str>)> {
    pair(
        preceded(pair(tag("Time:"), space1), separated_list1(space1, digit1)),
        preceded(
            pair(tag("\nDistance:"), space1),
            separated_list1(space1, digit1),
        ),
    )(input)
}

fn process(input: &str) -> u64 {
    let (_, (times, distances)) = parse(input).expect("should be valid parse");
    let time = times.concat().parse::<u64>().expect("should be number");
    let dist = distances.concat().parse::<u64>().expect("shoud be number");
    farthest(time, dist)
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

        assert_eq!(71503, process(input))
    }
}
