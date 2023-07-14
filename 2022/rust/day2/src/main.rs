use std::fs;


fn part1(path: &str) -> i32 {
    let content = fs::read_to_string(path).expect("failed");
    let moves: Vec<&str> = content.split("\n").collect();
    let mut result = 0;
    for mov in moves {
        match mov {
            "A Y" => result = result + 8 ,
            "A X" => result = result + 4, 
            "A Z" => result = result + 3, 
            "B Y" => result = result + 5, 
            "B X" => result = result + 1, 
            "B Z" => result = result + 9,
            "C Y" => result = result + 2, 
            "C X" => result = result + 7, 
            "C Z" => result = result + 6,
            &_ => panic!("shouldnt happen")
        }
    }
    return result;
}

fn part2(path: &str) -> i32 {
    let content = fs::read_to_string(path).expect("failed");
    let moves: Vec<&str> = content.split("\n").collect();
    let mut result = 0;
    for mov in moves {
        match mov {
            "A Y" => result = result + 4,
            "A X" => result = result + 3, 
            "A Z" => result = result + 8, 
            "B Y" => result = result + 5, 
            "B X" => result = result + 1, 
            "B Z" => result = result + 9,
            "C Y" => result = result + 6, 
            "C X" => result = result + 2, 
            "C Z" => result = result + 7,
            &_ => panic!("shouldnt happen")
        }
    }
    return result;
}


fn main() {
    let result1 = part1("input2.txt");
    let result2 = part2("input2.txt");

    println!("Result 1: {result1}");
    println!("Result 2: {result2}")
}
