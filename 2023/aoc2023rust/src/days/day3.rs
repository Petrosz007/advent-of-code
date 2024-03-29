use regex::Regex;

use super::SolutionType::Number;
use super::{Day, Solution};

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct NumberCell {
    num: i64,
    i: usize,
    j: usize,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Cell {
    Empty,
    Symbol(char),
    Number(NumberCell),
}

type Schematic = Vec<Vec<Cell>>;

fn get_value_of_adjecent(schematic: &Schematic, i: usize, j: usize) -> Option<NumberCell> {
    schematic
        .get(i)
        .and_then(|row| {
            row.get(j).map(|cell| match cell {
                Cell::Number(number_cell) => Some(*number_cell),
                _ => None,
            })
        })
        .flatten()
}

fn get_adjecent_nums(schematic: &Schematic, i: usize, j: usize) -> Vec<NumberCell> {
    let mut adjecent_nums = vec![
        (i - 1, j - 1),
        (i - 1, j),
        (i - 1, j + 1),
        (i, j - 1),
        (i, j + 1),
        (i + 1, j - 1),
        (i + 1, j),
        (i + 1, j + 1),
    ]
    .into_iter()
    .filter_map(|(i, j)| get_value_of_adjecent(schematic, i, j))
    .collect::<Vec<_>>();

    adjecent_nums.sort();
    adjecent_nums.dedup();

    adjecent_nums
}

fn sum_of_adjecents(schematic: &Schematic, i: usize, j: usize) -> i64 {
    get_adjecent_nums(schematic, i, j)
        .iter()
        .map(|NumberCell { num, .. }| num)
        .sum()
}

fn product_of_two_adjecents(schematic: &Schematic, i: usize, j: usize) -> i64 {
    let adjecent_nums = get_adjecent_nums(schematic, i, j);

    if adjecent_nums.len() == 2 {
        adjecent_nums[0].num * adjecent_nums[1].num
    } else {
        0
    }
}

fn part1(schematic: &Schematic) -> i64 {
    let mut sum = 0;
    for (row, i) in schematic.iter().zip(0..) {
        for (cell, j) in row.iter().zip(0..) {
            if let Cell::Symbol(_) = cell {
                sum += sum_of_adjecents(schematic, i, j)
            }
        }
    }

    sum
}

fn part2(schematic: &Schematic) -> i64 {
    let mut sum = 0;
    for (row, i) in schematic.iter().zip(0..) {
        for (cell, j) in row.iter().zip(0..) {
            if let Cell::Symbol(_) = cell {
                sum += product_of_two_adjecents(schematic, i, j)
            }
        }
    }

    sum
}

pub struct Day3;
impl Day for Day3 {
    type ParsedInput = Schematic;

    fn day_file() -> &'static str {
        "day3.txt"
    }

    fn parse_input(input: &str) -> Schematic {
        let lines = input.lines().collect::<Vec<&str>>();

        let mut tmp_line = Vec::new();
        tmp_line.resize(lines[0].len(), Cell::Empty);

        let mut schematic = Vec::new();
        schematic.resize(lines.len(), tmp_line);

        let num_regex = Regex::new(r"\d+").expect("The regex should compile");

        for (line, i) in lines.iter().zip(0..) {
            let mut skip = 0;
            for (j, c) in line.char_indices() {
                if skip > 0 {
                    skip -= 1;
                    continue;
                }

                if c == '.' {
                } else if c.is_ascii_digit() {
                    num_regex.find_at(line, j).map_or_else(
                        || panic!("Couldn't match number regex where we saw a number"),
                        |m| {
                            let num = m
                                .as_str()
                                .parse::<i64>()
                                .expect("A number should be parsed");
                            for new_j in m.start()..m.end() {
                                schematic[i][new_j] = Cell::Number(NumberCell { num, i, j });
                            }
                            skip = m.end() - m.start() - 1;
                        },
                    )
                } else {
                    schematic[i][j] = Cell::Symbol(c);
                }
            }
        }

        schematic
    }

    fn solve(schematic: Schematic) -> Solution {
        let part1 = Number(part1(&schematic));
        let part2 = Number(part2(&schematic));

        Solution { part1, part2 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let input = r#"
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"#
        .trim();
        let solution = Day3::solve_text_input(input);

        assert_eq!(
            solution,
            Solution {
                part1: Number(4361),
                part2: Number(467_835)
            }
        );
    }

    #[test]
    fn test_actual_solution() {
        let solution = Day3::solve_days_input();

        assert_eq!(
            solution,
            Solution {
                part1: Number(532_428),
                part2: Number(84_051_670)
            }
        );
    }
}
