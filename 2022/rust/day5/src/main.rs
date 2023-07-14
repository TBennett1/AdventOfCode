use std::{fs, collections::HashMap};
use regex::Regex;

fn state_hashmap(state: Vec<&str>) -> HashMap<char, String>{
    let mut state_map = HashMap::new();

    let nums = state.last().expect("no last");
    let nums_len = nums.len();

    for line_idx in (1..nums_len).step_by(4){
        let stack_num = nums.as_bytes()[line_idx] as char;

        for line_num in (0..(state.len()-1)).rev() {
            let char = state[line_num].as_bytes()[line_idx] as char;
            if char.is_alphabetic(){
                state_map
                    .entry(stack_num)
                    .and_modify(|x: &mut String| x.push(char))
                    .or_insert(char.to_string());
            }
        }
    }
    state_map
}

fn get_instruction(instr: &str) -> Vec<u32> {
    let reg = Regex::new(r"\d+").unwrap();

    return reg
        .find_iter(instr)
        .map(|n| {
            n.as_str()
                .parse::<u32>()
                .expect("not parsable")
        })
        .collect();
}

fn get_last_letters(map: HashMap<char, String>) -> String {
    let mut result = String::from("");
    let stack_size = map.keys().len();
    for idx in 1..stack_size + 1 {
        let stack_idx = &char::from_digit(idx as u32, 10).unwrap();
        let last_letter = map
            .get(stack_idx)
            .expect("should be stack idx")
            .chars()
            .last()
            .expect("should be char");
        result.push(last_letter);
    }
    result
}

fn part1(input: &str) -> String {
    let content = fs::read_to_string(input).expect("failed");
    let mut state_inst = content.split("\n\n");
    let state: Vec<&str> = state_inst.next().expect("no state").split("\n").collect();
    let inst: Vec<&str> = state_inst.next().expect("no instructs").split("\n").collect();

    let mut map = state_hashmap(state);

    for i in inst{
        let instruction = get_instruction(i);

        let stack_to_take_from_idx = &char::from_digit(instruction[1], 10).unwrap();
        let stack_to_take_from = map.get(stack_to_take_from_idx).expect("should be string");

        let stack_to_add_to_idx = &char::from_digit(instruction[2], 10).unwrap();
        let stack_to_add_to = map.get(stack_to_add_to_idx).expect("should be string");

        let split_idx = stack_to_take_from.len() - (instruction[0] as usize);
        let first_part = &stack_to_take_from[..split_idx];
        let second_part = &stack_to_take_from[split_idx..];
        let reversed = second_part.chars().rev().collect::<String>();
        let new_stack = format!("{}{}", stack_to_add_to, reversed);

        map.insert(*stack_to_take_from_idx, first_part.to_string());
        map.insert(*stack_to_add_to_idx, new_stack);
    }

    let result = get_last_letters(map);
    return result;
}

fn part2(input: &str) -> String {
    let content = fs::read_to_string(input).expect("failed");
    let mut state_inst = content.split("\n\n");
    let state: Vec<&str> = state_inst.next().expect("no state").split("\n").collect();
    let inst: Vec<&str> = state_inst.next().expect("no instructs").split("\n").collect();

    let mut map = state_hashmap(state);

    for i in inst{
        let instruction = get_instruction(i);

        let stack_to_take_from_idx = &char::from_digit(instruction[1], 10).unwrap();
        let stack_to_take_from = map.get(stack_to_take_from_idx).expect("should be string");

        let stack_to_add_to_idx = &char::from_digit(instruction[2], 10).unwrap();
        let stack_to_add_to = map.get(stack_to_add_to_idx).expect("should be string");

        let split_idx = stack_to_take_from.len() - (instruction[0] as usize);
        let first_part = &stack_to_take_from[..split_idx];
        let second_part = &stack_to_take_from[split_idx..];
        let new_stack = format!("{}{}", stack_to_add_to, second_part);

        map.insert(*stack_to_take_from_idx, first_part.to_string());
        map.insert(*stack_to_add_to_idx, new_stack);
    }

    let result = get_last_letters(map);
    return result;
}

fn main() {
    let result1 = part1("input5.txt");
    let result2 = part2("input5.txt");

    println!("Result 1: {result1:?}");
    println!("Result 2: {result2}")
}
