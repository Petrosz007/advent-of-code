use crate::days::{Day, Solution, SolutionType::Number};

fn part1(input: &[i32]) -> i64 {
    input
        .iter()
        .fold((50, 0), |(acc, zero_count), x| {
            let after_rotation = (acc + x) % 100;
            (
                after_rotation,
                if after_rotation == 0 {
                    zero_count + 1
                } else {
                    zero_count
                },
            )
        })
        .1
}

fn part2(input: &[i32]) -> i64 {
    // TODO: Could be done with some modulo magic, but it's too early in the morning for that
    let mut position = 50;
    let mut zero_count = 0;
    for amount in input.iter() {
        for _ in 0..amount.abs() {
            position = (position + amount.signum()) % 100;
            if position == 0 {
                zero_count += 1;
            }
        }
    }

    zero_count
}

pub struct Day1;
impl Day for Day1 {
    type ParsedInput = Vec<i32>;

    fn day_file() -> &'static str {
        "day1.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        input
            .lines()
            .map(|line| {
                line.replace("L", "")
                    .replace("R", "-")
                    .parse()
                    .expect("input to be a number")
            })
            .collect()
    }

    fn solve(input: Self::ParsedInput) -> Solution {
        Solution {
            part1: Number(part1(&input)),
            part2: Number(part2(&input)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::days::Solution;

    use super::*;

    #[test]
    fn test_part1() {
        let input = r#"
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
        "#
        .trim();

        let parsed_input = Day1::parse_input(input);
        let result = part1(&parsed_input);

        assert_eq!(result, 3)
    }

    #[test]
    fn test_part2() {
        let input = r#"
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
        "#
        .trim();

        let parsed_input = Day1::parse_input(input);
        let result = part2(&parsed_input);

        assert_eq!(result, 6)
    }

    #[test]
    fn test_solution() {
        let result = Day1::solve_days_input();

        assert_eq!(
            result,
            Solution {
                part1: Number(1081),
                part2: Number(6689),
            }
        )
    }
}
