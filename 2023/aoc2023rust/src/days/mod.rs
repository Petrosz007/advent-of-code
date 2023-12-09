use std::{fmt::Display, fs::read_to_string};

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day6;

#[derive(PartialEq, Eq, Debug)]
pub enum SolutionType {
    Todo,
    Number(i64),
    Readable(String),
}

impl Default for SolutionType {
    fn default() -> Self {
        Self::Todo
    }
}

pub use SolutionType::{Number, Readable, Todo};

impl Display for SolutionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Todo => write!(f, "TODO"),
            Number(x) => write!(f, "{x}"),
            Readable(x) => write!(f, "{x}"),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Solution {
    pub part1: SolutionType,
    pub part2: SolutionType,
}

impl Display for Solution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Part1: {}", self.part1)?;
        write!(f, "\nPart2: {}", self.part2)?;

        Ok(())
    }
}

fn read_lines(file_name: &str) -> Vec<String> {
    let path = format!("inputs/{file_name}");

    read_to_string(path)
        .unwrap_or_else(|_| panic!("Error opening file {file_name}!"))
        .lines()
        .map(std::string::ToString::to_string)
        .collect::<Vec<String>>()
}

pub trait Day {
    type ParsedInput;

    fn day_file() -> &'static str;

    fn parse_input(input: &str) -> Self::ParsedInput;

    fn solve(input: Self::ParsedInput) -> Solution;

    fn solve_file_input(file_name: &str) -> Solution {
        let path = format!("inputs/{file_name}");
        let file_contents = read_to_string(path).expect("File to exist");

        Self::solve_text_input(&file_contents)
    }

    fn solve_text_input(input: &str) -> Solution {
        let parsed_input = Self::parse_input(input);

        Self::solve(parsed_input)
    }

    fn solve_days_input() -> Solution {
        Self::solve_file_input(Self::day_file())
    }
}

pub fn solve(day: i32, input_path: &str) -> Solution {
    let input = read_to_string(input_path).expect("Error opening file {file_name}!");

    let solve = match day {
        1 => day1::Day1::solve_text_input,
        2 => day2::Day2::solve_text_input,
        3 => day3::Day3::solve_text_input,
        6 => day6::Day6::solve_text_input,

        unimplemented_day => panic!("Day {} is not implemented yet!", unimplemented_day),
    };

    solve(&input)
}
