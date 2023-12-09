use recap::Recap;
use serde::Deserialize;

use crate::days::{Solution, SolutionType::Number};

use super::Day;

#[derive(Debug, Deserialize, PartialEq, Eq, Recap)]
#[recap(regex = r#"(?P<count>\d+) (?P<color>\w+)"#)]
pub struct Pull {
    count: i64,
    color: String,
}

#[derive(Deserialize, Recap, Debug, PartialEq, Eq)]
#[recap(regex = r#"Game (?P<id>\d+): (?P<pulls>.*)"#)]
pub struct RawGame {
    id: i64,
    pulls: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Reveal {
    red: i64,
    green: i64,
    blue: i64,
}

impl Reveal {
    fn from_pulls(pulls: &[Pull]) -> Self {
        let mut reveal = Self {
            red: 0,
            green: 0,
            blue: 0,
        };
        for pull in pulls {
            match pull.color.as_str() {
                "red" => reveal.red = pull.count,
                "green" => reveal.green = pull.count,
                "blue" => reveal.blue = pull.count,
                x => panic!("Unimplemented color '{x}'"),
            }
        }

        reveal
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Game {
    id: i64,
    reveals: Vec<Reveal>,
}

fn parse_line(line: &str) -> Game {
    let raw_game = line.parse::<RawGame>().unwrap();
    let reveals = raw_game
        .pulls
        .split(';')
        .map(|round| {
            let pulls = round
                .split(',')
                .map(|s| s.parse::<Pull>().unwrap())
                .collect::<Vec<Pull>>();

            Reveal::from_pulls(&pulls)
        })
        .collect::<Vec<Reveal>>();

    Game {
        id: raw_game.id,
        reveals,
    }
}

fn max_reveals(reveals: &[Reveal]) -> Reveal {
    reveals
        .iter()
        .cloned()
        .reduce(|acc, x| Reveal {
            red: acc.red.max(x.red),
            green: acc.green.max(x.green),
            blue: acc.blue.max(x.blue),
        })
        .unwrap()
}

fn part1(games: &[Game]) -> i64 {
    games
        .iter()
        .filter(|game| {
            let max_reveals = max_reveals(&game.reveals);

            max_reveals.red <= 12 && max_reveals.green <= 13 && max_reveals.blue <= 14
        })
        .map(|game| game.id)
        .sum()
}

fn part2(games: &[Game]) -> i64 {
    games
        .iter()
        .map(|game| {
            let max_reveals = max_reveals(&game.reveals);

            max_reveals.red * max_reveals.green * max_reveals.blue
        })
        .sum()
}

pub struct Day2;
impl Day for Day2 {
    type ParsedInput = Vec<Game>;

    fn day_file() -> &'static str {
        "day2.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        input.lines().map(parse_line).collect()
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
        Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
                    "#
        .trim();

        let games = Day2::parse_input(input);
        let result = part1(&games);

        assert_eq!(result, 8);
    }

    #[test]
    fn test_part2() {
        let input = r#"
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
            "#
        .trim();

        let games = Day2::parse_input(input);
        let result = part2(&games);

        assert_eq!(result, 2286);
    }

    #[test]
    fn test_solution() {
        let result = Day2::solve_days_input();

        assert_eq!(
            result,
            Solution {
                part1: Number(2449),
                part2: Number(63_981)
            }
        )
    }
}
