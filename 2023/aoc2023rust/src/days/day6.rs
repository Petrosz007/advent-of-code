use super::SolutionType::Number;
use super::{Day, Solution};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RaceRecord {
    time: i64,
    distance: i64,
}

const fn calc_result(time_held: i64, total_time: i64) -> i64 {
    time_held * (total_time - time_held)
}

fn num_ways_to_win(race_record: &RaceRecord) -> i64 {
    (0..=race_record.time)
        .map(|time_held| calc_result(time_held, race_record.time))
        .filter(|result| *result > race_record.distance)
        .count() as i64
}

fn part1(race_records: &[RaceRecord]) -> i64 {
    race_records.iter().map(num_ways_to_win).product()
}

fn part2(race_record: &RaceRecord) -> i64 {
    num_ways_to_win(race_record)
}

pub struct Day6;
impl Day for Day6 {
    type ParsedInput = (Vec<RaceRecord>, RaceRecord);

    fn day_file() -> &'static str {
        "day6.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        let lines = input.lines().collect::<Vec<&str>>();

        fn parse(line: &str) -> Vec<i64> {
            line.split_ascii_whitespace()
                .skip(1)
                .map(|x| x.parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        }
        let times = parse(lines[0]);
        let distance = parse(lines[1]);

        let part1 = times
            .into_iter()
            .zip(distance)
            .map(|(time, distance)| RaceRecord { time, distance })
            .collect::<Vec<_>>();

        let part2 = RaceRecord {
            time: lines[0]
                .replace("Time:", "")
                .replace(' ', "")
                .parse::<i64>()
                .unwrap(),
            distance: lines[1]
                .replace("Distance:", "")
                .replace(' ', "")
                .parse::<i64>()
                .unwrap(),
        };

        (part1, part2)
    }

    fn solve(parsed_input: Self::ParsedInput) -> Solution {
        let part1 = Number(part1(&parsed_input.0));
        let part2 = Number(part2(&parsed_input.1));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let input = r#"
Time:      7  15   30
Distance:  9  40  200
        "#
        .trim();
        let expected = (
            vec![(7, 9), (15, 40), (30, 200)]
                .into_iter()
                .map(|(time, distance)| RaceRecord { time, distance })
                .collect::<Vec<_>>(),
            RaceRecord {
                time: 71530,
                distance: 940200,
            },
        );

        let result = Day6::parse_input(input);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_test_solution() {
        let input = r#"
Time:      7  15   30
Distance:  9  40  200
        "#
        .trim();

        let result = Day6::solve_text_input(input);

        assert_eq!(
            result,
            Solution {
                part1: Number(288),
                part2: Number(71503)
            }
        );
    }

    #[test]
    fn test_solution() {
        let result = Day6::solve_file_input("day6.txt");

        assert_eq!(
            result,
            Solution {
                part1: Number(293_046),
                part2: Number(35_150_181)
            }
        );
    }
}
