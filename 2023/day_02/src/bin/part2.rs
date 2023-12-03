use itertools::Itertools;

fn main() {
    let input = include_str!("../../input.txt");
    let result = part2(input);
    println!("{}", result)
}

fn part2(input: &str) -> u32 {
    let games = input
        .lines()
        .map(|l| l.split(':').collect_tuple::<(&str, &str)>().unwrap())
        .collect_vec();

    games.iter().fold(0, |acc, game| {
        let mut max_red = 0;
        let mut max_blue = 0;
        let mut max_green = 0;
        let results = game
            .1
            .split(';')
            .map(|g| g.split(',').collect_vec())
            .collect_vec();
        results.iter().for_each(|r| {
            r.iter().for_each(|r| {
                let pair = r
                    .trim_start()
                    .split(' ')
                    .collect_tuple::<(&str, &str)>()
                    .unwrap();
                let color = pair.1;
                let num = pair.0.parse::<u32>().expect("should be number");
                if color == "blue" && num > max_blue {
                    max_blue = num
                } else if color == "green" && num > max_green {
                    max_green = num
                } else if color == "red" && num > max_red {
                    max_red = num
                }
            })
        });
        acc + (max_blue * max_green * max_red)
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
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

        assert_eq!(2286, part2(input))
    }
}
