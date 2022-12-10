use super::SolutionType::Number;
use super::{Day, Days, Solution};

type Calories = Vec<i64>;

fn part1(calories: &Calories) -> i64 {
    calories[0]
}

fn part2(calories: &Calories) -> i64 {
    calories[0] + calories[1] + calories[2]
}

pub type ParsedInput = Calories;
impl Day<1, Calories> for Days {
    fn parse_lines(&self, lines: &[&str]) -> Calories {
        let mut calories = Vec::new();
        let mut acc = 0;

        for line in lines {
            if let Ok(num) = line.parse::<i64>() {
                acc += num;
            } else {
                calories.push(acc);
                acc = 0;
            }
        }

        calories
    }

    fn solve(&self, calories: Calories) -> Solution {
        let mut calories = calories;

        calories.sort_unstable();
        calories.reverse();

        let part1 = Number(part1(&calories));
        let part2 = Number(part2(&calories));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let solution = Day::<1, ParsedInput>::solve_days_input(&Days::new());

        assert_eq!(
            solution,
            Solution {
                part1: Number(69_912),
                part2: Number(208_180)
            }
        );
    }
}
