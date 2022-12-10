use std::{fmt::Display, fs::read_to_string};

pub mod day1;
pub mod day2;
pub mod day3;
pub mod day4;
pub mod day5;

#[derive(PartialEq, Eq, Debug)]
pub enum SolutionType {
    TODO,
    Number(i64),
    Readable(String),
}

pub use SolutionType::{Number, Readable, TODO};

impl Display for SolutionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TODO => write!(f, "TODO"),
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
        .expect("Error opening file {file_name}!")
        .lines()
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
}

pub trait Day<const DAY: u8, ParsedInput> {
    fn input_file_name(&self) -> String {
        format!("day{}.txt", DAY)
    }

    fn parse_lines(&self, lines: &[&str]) -> ParsedInput;

    fn solve(&self, input: ParsedInput) -> Solution;

    fn solve_days_input(&self) -> Solution {
        let lines = read_lines(&self.input_file_name());
        let lines = lines.iter().map(|line| &line[..]).collect::<Vec<&str>>();
        let parsed_input = self.parse_lines(&lines);

        self.solve(parsed_input)
    }

    fn solve_text_input(&self, input: &str) -> Solution {
        let lines = input.lines().collect::<Vec<&str>>();
        let parsed_input = self.parse_lines(&lines);

        self.solve(parsed_input)
    }
}

pub struct Days {}

impl Days {
    pub const fn new() -> Self {
        Self {}
    }
}
