use recap::Recap;
use serde::Deserialize;

use super::{Day, Days, Solution};

#[derive(Deserialize, Recap)]
#[recap(regex = r#"(?P<left_lo>\d+)-(?P<left_hi>\d+),(?P<right_lo>\d+)-(?P<right_hi>\d+)"#)]
pub struct Line {
    left_lo: u32,
    left_hi: u32,
    right_lo: u32,
    right_hi: u32,
}

fn is_totally_overlapping(line: &Line) -> bool {
    let Line {
        left_lo: llo,
        left_hi: lhi,
        right_lo: rlo,
        right_hi: rhi,
    } = line;

    (rlo <= llo && lhi <= rhi) || (llo <= rlo && rhi <= lhi)
}

fn is_overlapping(line: &Line) -> bool {
    let Line {
        left_lo: llo,
        left_hi: lhi,
        right_lo: rlo,
        right_hi: rhi,
    } = line;

    (rlo <= llo && llo <= rhi)
        || (rlo <= lhi && lhi <= rhi)
        || (llo <= rlo && rlo <= lhi)
        || (llo <= rhi && rhi <= lhi)
}

fn part1(lines: &[Line]) -> i64 {
    lines
        .iter()
        .filter(|line| is_totally_overlapping(line))
        .count() as i64
}

fn part2(lines: &[Line]) -> i64 {
    lines.iter().filter(|line| is_overlapping(line)).count() as i64
}

pub type ParsedInput = Vec<Line>;
impl Day<4, ParsedInput> for Days {
    fn parse_lines(&self, lines: &[&str]) -> ParsedInput {
        lines
            .iter()
            .map(|x| {
                x.parse()
                    .unwrap_or_else(|_| panic!("Line was in an incorrect format: '{x}'"))
            })
            .collect()
    }

    fn solve(&self, input: ParsedInput) -> Solution {
        let part1 = Some(part1(&input));
        let part2 = Some(part2(&input));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use crate::days::Solution;

    use super::*;

    #[test]
    fn test_solution() {
        let solution = Day::<4, ParsedInput>::solve_days_input(&Days::new());

        assert_eq!(
            solution,
            Solution {
                part1: Some(657),
                part2: Some(938)
            }
        );
    }

    #[test]
    fn test_part1() {
        let input = r#"
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
        "#
        .trim();

        let solution = Day::<4, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part1, Some(2));
    }

    #[test]
    fn test_part2() {
        let input = r#"
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
        "#
        .trim();

        let solution = Day::<4, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part2, Some(4));
    }
}
