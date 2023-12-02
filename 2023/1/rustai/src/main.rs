use std::collections::HashMap;

fn text_to_number(text: &str) -> Option<&str> {
    let text_to_num = HashMap::from([
        ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"),
        ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9"),
    ]);
    text_to_num.get(text).copied()
}

fn find_calibration_value(line: &str, part_two: bool) -> u32 {
    let mut digits: Vec<String> = Vec::new();
    if part_two {
        let mut word = String::new();
        for char in line.chars() {
            if char.is_alphabetic() {
                word.push(char);
                if let Some(num) = text_to_number(&word) {
                    digits.push(num.to_owned());
                    word.clear();
                }
            } else if char.is_ascii_digit() {
                digits.push(char.to_string());
                word.clear();
            }
        }
    } else {
        digits = line.chars()
                     .filter(|c| c.is_ascii_digit())
                     .map(|c| c.to_string())
                     .collect();
    }

    if digits.len() >= 2 {
        let first = digits.first().unwrap();
        let last = digits.last().unwrap();
        first.parse::<u32>().unwrap() * 10 + last.parse::<u32>().unwrap()
    } else {
        0
    }
}

fn main() {
    // Example inputs
    let example_lines_part_one = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"];
    let example_lines_part_two = ["two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"];

    let sum_part_one: u32 = example_lines_part_one
        .iter()
        .map(|&line| find_calibration_value(line, false))
        .sum();
    
    let sum_part_two: u32 = example_lines_part_two
        .iter()
        .map(|&line| find_calibration_value(line, true))
        .sum();

    println!("Part One: {}", sum_part_one);
    println!("Part Two: {}", sum_part_two);
}
