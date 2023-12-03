use itertools::Itertools;

fn main() {
    let input = include_str!("../../input.txt");
    let result = part1(input);
    println!("{}", result)
}

fn part1(input: &str) -> u32 {
    let games = input
        .lines()
        .map(|l| l.split(':').collect_tuple::<(&str, &str)>().unwrap())
        .collect_vec();

    games.iter().fold(0, |acc, game| {
        let id = game
            .0
            .split(' ')
            .last()
            .map(|d| d.parse::<u32>().expect("should be number"))
            .unwrap();
        let results = game
            .1
            .split(';')
            .map(|g| g.split(',').collect_vec())
            .collect_vec();
        let bool_results = results
            .iter()
            .map(|r| {
                r.iter()
                    .map(|r| {
                        let pair = r
                            .trim_start()
                            .split(' ')
                            .collect_tuple::<(&str, &str)>()
                            .unwrap();
                        pair.1 == "blue" && pair.0.parse::<u32>().expect("should be number") <= 14
                            || pair.1 == "green"
                                && pair.0.parse::<u32>().expect("should be number") <= 13
                            || pair.1 == "red"
                                && pair.0.parse::<u32>().expect("should be number") <= 12
                    })
                    .all(|x| x)
            })
            .collect_vec();
        if bool_results.iter().all(|x| *x) {
            acc + id
        } else {
            acc
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
Game 72: 11 green, 4 red, 2 blue; 2 blue, 6 green, 1 red; 3 red, 1 blue, 9 green; 4 blue, 12 green, 3 red; 2 red, 3 green, 1 blue";

        assert_eq!(80, part1(input))
    }
}
