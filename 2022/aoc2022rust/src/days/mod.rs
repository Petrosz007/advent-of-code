use std::{fmt::Display, fs::read_to_string};

pub mod day1;
pub mod day2;

#[derive(PartialEq, Debug)]
pub struct Solution {
    pub part1: Option<i64>,
    pub part2: Option<i64>,
}

impl Display for Solution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(part1) = self.part1 {
            write!(f, "Part1: {part1}")?;
        } else {
            write!(f, "Part1: TODO")?;
        };

        if let Some(part2) = self.part2 {
            write!(f, "\nPart2: {part2}")?;
        } else {
            write!(f, "\nPart2: TODO")?;
        };

        Ok(())
    }
}

fn read_lines(file_name: &str) -> Vec<String> {
    let path = format!("inputs/{file_name}");

    read_to_string(path)
        .expect("Error opening file {file_name}!")
        .lines()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
}

pub trait Day<const DAY: u8, ParsedInput> {
    fn input_file_name(&self) -> String {
        format!("day{}.txt", DAY)
    }

    fn parse_lines(&self, lines: &Vec<String>) -> ParsedInput;

    fn solve(&self, input: ParsedInput) -> Solution;

    fn solve_days_input(&self) -> Solution {
        let lines = read_lines(&self.input_file_name());
        let parsed_input = self.parse_lines(&lines);
        let solution = self.solve(parsed_input);

        solution
    }

    fn solve_text_input(&self, input: &str) -> Solution {
        let lines = input.lines().map(|x| x.to_string()).collect();
        let parsed_input = self.parse_lines(&lines);
        let solution = self.solve(parsed_input);

        solution
    }
}

pub struct Days {}

impl Days {
    pub fn new() -> Self {
        Days {}
    }
}
