use super::Day;
use super::SolutionType::Number;

fn line_value_part1(line: &str) -> i64 {
    let first_digit = line.chars().flat_map(|c| c.to_digit(10)).next().unwrap();
    let last_digit = line
        .chars()
        .rev()
        .flat_map(|c| c.to_digit(10))
        .next()
        .unwrap();

    (first_digit * 10 + last_digit) as i64
}

fn part1(lines: &[String]) -> i64 {
    lines.iter().map(|line| line_value_part1(line)).sum()
}

fn line_value_part2(line: &str) -> i64 {
    // let mapping = vec![
    //     ("twone", "21"),
    //     ("oneight", "18"),
    //     ("threeight", "38"),
    //     ("fiveight", "58"),
    //     ("sevenine", "79"),
    //     ("eightwo", "82"),
    //     ("eightree", "83"),
    //     ("nineight", "98"),
    //     ("one", "1"),
    //     ("two", "2"),
    //     ("three", "3"),
    //     ("four", "4"),
    //     ("five", "5"),
    //     ("six", "6"),
    //     ("seven", "7"),
    //     ("eight", "8"),
    //     ("nine", "9"),
    // ];

    // let replaced_line = mapping
    //     .into_iter()
    //     .fold(line.to_string(), |acc, (from, to)| acc.replace(from, to));

    // line_value_part1(&replaced_line)

    let mut i = 0;
    let chars = line.chars().collect::<Vec<char>>();
    let mut first_digit = None;
    let mut last_digit = None;

    let mut update_digits = |digit: u32| {
        if first_digit.is_none() {
            first_digit = Some(digit);
        }

        last_digit = Some(digit);
    };

    loop {
        if i >= chars.len() {
            break;
        }

        match chars[i] {
            'o' if line[i..].starts_with("one") => update_digits(1),
            't' if line[i..].starts_with("two") => update_digits(2),
            't' if line[i..].starts_with("three") => update_digits(3),
            'f' if line[i..].starts_with("four") => update_digits(4),
            'f' if line[i..].starts_with("five") => update_digits(5),
            's' if line[i..].starts_with("six") => update_digits(6),
            's' if line[i..].starts_with("seven") => update_digits(7),
            'e' if line[i..].starts_with("eight") => update_digits(8),
            'n' if line[i..].starts_with("nine") => update_digits(9),
            c if c.is_ascii_digit() => update_digits(
                c.to_digit(10)
                    .expect("We've just checked that this is a digit"),
            ),
            _ => {}
        };
        i += 1;
    }

    if let (Some(first), Some(last)) = (first_digit, last_digit) {
        (first * 10 + last) as i64
    } else {
        panic!("We didn't find two digits in the line '{line}'");
    }
}

fn part2(lines: &[String]) -> i64 {
    lines.iter().map(|line| line_value_part2(line)).sum()
}

pub struct Day1;
impl Day for Day1 {
    type ParsedInput = Vec<String>;

    fn day_file() -> &'static str {
        "day1.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        input
            .lines()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
    }

    fn solve(input: Self::ParsedInput) -> super::Solution {
        let part1 = Number(part1(&input));
        let part2 = Number(part2(&input));

        super::Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use crate::days::Solution;

    use super::*;

    #[test]
    fn test_part1() {
        let input = r#"
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
        "#
        .trim();

        let parsed_input = Day1::parse_input(input);
        let result = part1(&parsed_input);

        assert_eq!(result, 142)
    }

    #[test]
    fn test_part2() {
        let input = r#"
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
        "#
        .trim();

        let parsed_input = Day1::parse_input(input);
        let result = part2(&parsed_input);

        assert_eq!(result, 281)
    }

    #[test]
    fn test_solution() {
        let result = Day1::solve_days_input();

        assert_eq!(
            result,
            Solution {
                part1: Number(55_029),
                part2: Number(55_686)
            }
        )
    }
}
