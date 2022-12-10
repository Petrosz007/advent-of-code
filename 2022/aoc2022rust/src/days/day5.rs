use std::collections::HashMap;

use recap::Recap;
use regex::Regex;
use serde::Deserialize;

use crate::utils::transpose_lines;

use super::SolutionType::Readable;
use super::{Day, Days, Solution};

#[derive(Deserialize, Recap, PartialEq, Eq, Debug)]
#[recap(regex = r#"move (?P<amount>\d+) from (?P<from>\d+) to (?P<to>\d+)"#)]
struct Instruction {
    amount: u8,
    from: u8,
    to: u8,
}

type Crates = HashMap<u8, Vec<char>>;

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedInput {
    crates: Crates,
    instructions: Vec<Instruction>,
}

fn get_from_and_to<'a>(
    crates: &'a mut Crates,
    instruction: &'a Instruction,
) -> (&'a mut Vec<char>, &'a mut Vec<char>) {
    // We are sure that we won't borrow the same value twice
    // This unsafe block looks very hacky, but at least I've learnt something about unsafes today :D
    unsafe {
        let a = crates.get_mut(&instruction.from).unwrap() as *mut _;
        let b = crates.get_mut(&instruction.to).unwrap() as *mut _;
        assert_ne!(a, b, "The two keys must not resolve to the same value");

        let from: &mut Vec<char> = &mut *a;
        let to: &mut Vec<char> = &mut *b;

        (from, to)
    }
}

fn do_instruction_part1(crates: &mut Crates, instruction: &Instruction) {
    let (from, to) = get_from_and_to(crates, instruction);

    for _ in 1..=instruction.amount {
        to.push(from.pop().unwrap());
    }
}

fn do_instruction_part2(crates: &mut Crates, instruction: &Instruction) {
    let (from, to) = get_from_and_to(crates, instruction);

    let amount = instruction.amount as usize;
    to.append(&mut from.as_slice()[from.len() - amount..].to_vec());
    from.truncate(from.len() - amount);
}

fn solve(input: &ParsedInput, do_instruction: impl Fn(&mut Crates, &Instruction)) -> String {
    let mut crates = input.crates.clone();

    for instruction in &input.instructions {
        do_instruction(&mut crates, instruction);
    }

    let mut tops = crates
        .iter()
        .map(|(i, xs)| (i, xs.last().unwrap()))
        .collect::<Vec<_>>();

    tops.sort_by_key(|(i, _)| *i);

    tops.iter().map(|(_, x)| *x).collect()
}

fn part1(input: &ParsedInput) -> String {
    solve(input, do_instruction_part1)
}

fn part2(input: &ParsedInput) -> String {
    solve(input, do_instruction_part2)
}

impl Day<5, ParsedInput> for Days {
    fn parse_lines(&self, lines: &[&str]) -> ParsedInput {
        let raw_input = lines.join("\n");
        let sections = raw_input.split("\n\n").collect::<Vec<&str>>();

        let crate_lines = sections[0].lines().collect();
        let instruction_lines = sections[1].lines();

        let instructions = instruction_lines
            .map(|line| line.parse().unwrap())
            .collect();

        let wrong_line_re = Regex::new(r#"^(\[|\]| )+$"#).unwrap();
        let transposed_crate_lines = transpose_lines(&crate_lines)
            .into_iter()
            .filter(|s| !wrong_line_re.is_match(s));

        let crates = transposed_crate_lines
            .map(|xs| {
                let ys: Vec<char> = xs.trim_start().chars().rev().collect();
                (ys[0].to_digit(10).unwrap() as u8, ys[1..].to_vec())
            })
            .collect();

        ParsedInput {
            crates,
            instructions,
        }
    }

    fn solve(&self, input: ParsedInput) -> super::Solution {
        let part1 = Readable(part1(&input));
        let part2 = Readable(part2(&input));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod tests {
    use crate::days::Solution;
    use crate::days::SolutionType::{Readable, TODO};
    use crate::days::{day5::Instruction, Day, Days};

    use super::ParsedInput;

    const fn input1() -> &'static str {
        r#"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"#
    }

    #[test]
    fn test_parse() {
        let lines = input1().lines().collect::<Vec<&str>>();
        let parsed = Day::<5, ParsedInput>::parse_lines(&Days::new(), &lines);

        assert_eq!(
            parsed,
            ParsedInput {
                crates: vec![
                    (1u8, vec!['Z', 'N']),
                    (2u8, vec!['M', 'C', 'D']),
                    (3u8, vec!['P'])
                ]
                .into_iter()
                .collect(),
                instructions: vec![
                    Instruction {
                        amount: 1,
                        from: 2,
                        to: 1
                    },
                    Instruction {
                        amount: 3,
                        from: 1,
                        to: 3
                    },
                    Instruction {
                        amount: 2,
                        from: 2,
                        to: 1
                    },
                    Instruction {
                        amount: 1,
                        from: 1,
                        to: 2
                    },
                ]
            }
        );
    }

    #[test]
    fn test_solution() {
        let solution = Day::<5, ParsedInput>::solve_days_input(&Days::new());
        assert_eq!(
            solution,
            Solution {
                part1: Readable("SVFDLGLWV".to_string()),
                part2: Readable("DCVTCVPCL".to_string())
            }
        );
    }

    #[test]
    fn test_part1() {
        let solution = Day::<5, ParsedInput>::solve_text_input(&Days::new(), input1());
        assert_eq!(solution.part1, Readable("CMZ".to_string()));
    }

    #[test]
    fn test_part2() {
        let solution = Day::<5, ParsedInput>::solve_text_input(&Days::new(), input1());
        assert_eq!(solution.part2, Readable("MCD".to_string()));
    }
}
