use crate::utils::intersect;

use super::{Day, Days, Solution};

pub type Compartment = Vec<char>;
pub type Rugsack = (Compartment, Compartment);

const fn calc_priority(c: char) -> i64 {
    if c.is_ascii_lowercase() {
        (c as i64) - ('a' as i64) + 1
    } else {
        (c as i64) - ('A' as i64) + 27
    }
}

fn parse_line(line: &str) -> Rugsack {
    let chars: Vec<char> = line.chars().collect();
    let count = chars.len();
    let (left, right) = (chars[..(count / 2)].to_vec(), chars[(count / 2)..].to_vec());

    (left, right)
}

fn part1(input: &[String]) -> i64 {
    input
        .iter()
        .map(|line| parse_line(line))
        .map(|(left, right)| intersect(&[left, right])[0])
        .map(calc_priority)
        .sum()
}

fn part2(input: &[String]) -> i64 {
    input
        .chunks(3)
        .map(|three_rugsacks| {
            let rugsacks: Vec<Vec<char>> =
                three_rugsacks.iter().map(|s| s.chars().collect()).collect();
            intersect(&rugsacks)[0]
        })
        .map(calc_priority)
        .sum()
}

pub type ParsedInput = Vec<String>;
impl Day<3, ParsedInput> for Days {
    fn parse_lines(&self, lines: &[&str]) -> ParsedInput {
        lines.iter().map(std::string::ToString::to_string).collect()
    }

    fn solve(&self, input: ParsedInput) -> Solution {
        let part1 = Some(part1(&input));
        let part2 = Some(part2(&input));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let solution = Day::<3, ParsedInput>::solve_days_input(&Days::new());

        assert_eq!(
            solution,
            Solution {
                part1: Some(7831),
                part2: Some(2683)
            }
        );
    }

    #[test]
    fn test_part1() {
        let input = r#"
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
        "#
        .trim();

        let solution = Day::<3, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part1, Some(157));
    }

    #[test]
    fn test_part2() {
        let input = r#"
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
        "#
        .trim();

        let solution = Day::<3, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part2, Some(70));
    }
}
