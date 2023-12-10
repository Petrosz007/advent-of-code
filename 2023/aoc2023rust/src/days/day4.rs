use std::collections::HashSet;

use recap::Recap;
use serde::Deserialize;

use crate::utils::StrExtensions;

use super::{
    Day, Solution,
    SolutionType::{Number, Todo},
};

#[derive(Deserialize, Recap)]
#[recap(regex = r#"Card\s+(?P<id>\d+): (?P<winning_numbers>[^|]*) \| (?P<own_numbers>.*)"#)]
struct Line {
    id: i64,
    winning_numbers: String,
    own_numbers: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Scratchcard {
    id: i64,
    winning_numbers: HashSet<i64>,
    own_numbers: HashSet<i64>,
}
impl Scratchcard {
    fn count_wins(&self) -> usize {
        self.winning_numbers
            .iter()
            .filter(|n| self.own_numbers.contains(n))
            .count()
    }
}

fn part1(scratchcards: &[Scratchcard]) -> i64 {
    scratchcards
        .iter()
        .map(|scratchcard| match scratchcard.count_wins() as u32 {
            0 => 0,
            n => 2_i64.pow(n - 1),
        })
        .sum()
}

fn part2(scratchcards: &[Scratchcard]) -> i64 {
    let mut copies = vec![1; scratchcards.len()];

    for (i, scratchcard) in scratchcards.iter().enumerate() {
        for j in i + 1..=i + scratchcard.count_wins() {
            copies[j] += copies[i];
        }
    }

    copies.iter().sum()
}

pub struct Day4;
impl Day for Day4 {
    type ParsedInput = Vec<Scratchcard>;

    fn day_file() -> &'static str {
        "day4.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        input
            .lines()
            .map(|line| {
                let parsed_line = line.parse::<Line>().unwrap();
                Scratchcard {
                    id: parsed_line.id,
                    winning_numbers: parsed_line
                        .winning_numbers
                        .whitespace_split_to::<i64>()
                        .collect(),
                    own_numbers: parsed_line
                        .own_numbers
                        .whitespace_split_to::<i64>()
                        .collect(),
                }
            })
            .collect()
    }

    fn solve(input: Self::ParsedInput) -> super::Solution {
        Solution {
            part1: Number(part1(&input)),
            part2: Number(part2(&input)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_part1() {
        let input = r#"
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
        "#
        .trim();

        let parsed_input = Day4::parse_input(input);
        let result = part1(&parsed_input);

        assert_eq!(result, 13);
    }

    #[test]
    fn test_part2() {
        let input = r#"
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
        "#
        .trim();

        let parsed_input = Day4::parse_input(input);
        let result = part2(&parsed_input);

        assert_eq!(result, 30);
    }

    #[test]
    fn test_solution() {
        let result = Day4::solve_days_input();

        assert_eq!(
            result,
            Solution {
                part1: Number(20_407),
                part2: Number(23_806_951)
            }
        )
    }
}
