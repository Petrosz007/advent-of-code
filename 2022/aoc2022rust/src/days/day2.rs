use super::{Day, Days, Solution};
use recap::Recap;
use serde::Deserialize;

#[derive(Deserialize, Recap)]
#[recap(regex = r#"(?P<first>[A-C])\s(?P<second>[X-Z])"#)]
pub struct RoundInput {
    first: char,
    second: char,
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum RPC {
    Rock,
    Paper,
    Scissors,
}
use RPC::*;

#[derive(Debug)]
enum RoundResult {
    Win,
    Lose,
    Tie,
}
use RoundResult::*;

#[derive(Debug)]
pub struct Round {
    opponents_play: RPC,
    my_play: RPC,
    result: RoundResult,
}

type StrategyGuide = Vec<RoundInput>;

impl RPC {
    fn from_abc(c: char) -> RPC {
        match c {
            'A' => Rock,
            'B' => Paper,
            'C' => Scissors,
            _ => panic!("Unknown input: '{}'", c),
        }
    }

    fn from_xzy(c: char) -> RPC {
        match c {
            'X' => Rock,
            'Y' => Paper,
            'Z' => Scissors,
            _ => panic!("Unknown input: '{}'", c),
        }
    }
}

impl RoundResult {
    fn from_xzy(c: char) -> RoundResult {
        match c {
            'X' => Lose,
            'Y' => Tie,
            'Z' => Win,
            _ => panic!("Unknown input: '{}'", c),
        }
    }
}

fn play_round(my_play: &RPC, opponents_play: &RPC) -> RoundResult {
    match (my_play, opponents_play) {
        (Rock, Scissors) => Win,
        (Scissors, Paper) => Win,
        (Paper, Rock) => Win,
        (x, y) if x == y => Tie,
        _ => Lose,
    }
}

fn reverse_engineer_round(opponents_play: &RPC, result: &RoundResult) -> RPC {
    match (opponents_play, result) {
        (x, Tie) => *x,
        (Rock, Lose) => Scissors,
        (Scissors, Lose) => Paper,
        (Paper, Lose) => Rock,
        (Rock, Win) => Paper,
        (Scissors, Win) => Rock,
        (Paper, Win) => Scissors,
    }
}

fn parse_part1(input: &RoundInput) -> Round {
    let opponents_play = RPC::from_abc(input.first);
    let my_play = RPC::from_xzy(input.second);
    let result = play_round(&my_play, &opponents_play);

    Round {
        opponents_play,
        my_play,
        result,
    }
}

fn parse_part2(input: &RoundInput) -> Round {
    let opponents_play = RPC::from_abc(input.first);
    let result = RoundResult::from_xzy(input.second);
    let my_play = reverse_engineer_round(&opponents_play, &result);

    Round {
        opponents_play,
        my_play,
        result,
    }
}

fn calc_score(round: &Round) -> i64 {
    let shape_score = match round.my_play {
        Rock => 1,
        Paper => 2,
        Scissors => 3,
    };

    let result_score = match round.result {
        Win => 6,
        Tie => 3,
        Lose => 0,
    };

    shape_score + result_score
}

fn calc_total_score(input: &StrategyGuide, parse_fn: impl Fn(&RoundInput) -> Round) -> i64 {
    input
        .iter()
        .map(parse_fn)
        .map(|round| calc_score(&round))
        .sum()
}

pub type ParsedInput = StrategyGuide;
impl Day<2, StrategyGuide> for Days {
    fn parse_lines(&self, lines: &Vec<String>) -> StrategyGuide {
        lines
            .iter()
            .map(|line| {
                line.parse::<RoundInput>()
                    .expect("Input for day 2 didn't match '\\d \\d'")
            })
            .collect()
    }

    fn solve(&self, input: StrategyGuide) -> Solution {
        let part1 = Some(calc_total_score(&input, parse_part1));
        let part2 = Some(calc_total_score(&input, parse_part2));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let solution = Day::<2, ParsedInput>::solve_days_input(&Days::new());

        assert_eq!(
            solution,
            Solution {
                part1: Some(8933),
                part2: Some(11998)
            }
        );
    }

    #[test]
    fn test_part1() {
        let input = r#"
        A Y
        B X
        C Z
        "#
        .trim();

        let solution = Day::<2, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part1, Some(15));
    }

    #[test]
    fn test_part2() {
        let input = r#"
        A Y
        B X
        C Z
        "#
        .trim();

        let solution = Day::<2, ParsedInput>::solve_text_input(&Days::new(), input);
        assert_eq!(solution.part2, Some(12));
    }
}
