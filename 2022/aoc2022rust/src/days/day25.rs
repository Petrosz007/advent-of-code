use super::{
    Day, Days, Solution,
    SolutionType::{Number, Todo},
};

fn parse_snafu(s: &str) -> i64 {
    let mut place = 1;
    let mut sum = 0;
    for c in s.chars().rev() {
        let val = match c {
            '2' => 2,
            '1' => 1,
            '0' => 0,
            '-' => -1,
            '=' => -2,
            _ => panic!("Enexpected SNAFU digit: '{c}'"),
        };
        sum += val * place;
        place *= 5;
    }

    sum
}

fn part1(input: &[i64]) -> i64 {
    input.iter().sum()
}

pub type ParsedInput = Vec<i64>;
impl Day<25, ParsedInput> for Days {
    fn parse_lines(&self, lines: &[&str]) -> ParsedInput {
        lines.iter().map(|s| parse_snafu(s)).collect()
    }

    fn solve(&self, input: ParsedInput) -> Solution {
        let part1 = Number(part1(&input));
        let part2 = Todo;

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod tests {
    use crate::days::{Days, SolutionType::Number, Day};
    use super::{ParsedInput, parse_snafu};

    fn input1() -> &'static str {
        r#"    
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"#
        .trim()
    }

    #[test]
    fn test_parse() {
        let tests = vec![("1=-0-2", 1747)];

        for (input, expected) in tests {
            assert_eq!(expected, parse_snafu(input));
        }
    }

    #[test]
    fn test_part1() {
        let solution = Day::<25, ParsedInput>::solve_text_input(&Days::new(), input1());
        assert_eq!(solution.part1, Number(4890));
    }
}
