use std::fmt::Display;

use crate::days::SolutionType::{Number, Todo};

use super::{Day, Solution};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum Direction {
    North,
    South,
    East,
    West,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum Pipe {
    Ground,
    Start,
    NorthEast,
    NorthSouth,
    NorthWest,
    SouthWest,
    SouthEast,
    EastWest,
}

#[derive(Debug, PartialEq, Eq)]
enum MoveErr {
    GotToStart,
    TrueError(String),
}

use Direction::*;
use Pipe::*;

impl Pipe {
    fn from_char(c: char) -> Self {
        match c {
            'S' => Start,
            '.' => Ground,
            'L' => NorthEast,
            '|' => NorthSouth,
            'J' => NorthWest,
            '7' => SouthWest,
            'F' => SouthEast,
            '-' => EastWest,
            x => panic!("Unknown pipe '{x}'"),
        }
    }

    /// Next direction, when I have come from the `from` direction
    fn next(&self, from: &Direction) -> Result<Direction, MoveErr> {
        match (*from, *self) {
            (South, NorthEast) => Ok(East),
            (West, NorthEast) => Ok(North),
            (South, NorthSouth) => Ok(South),
            (North, NorthSouth) => Ok(North),
            (South, NorthWest) => Ok(West),
            (East, NorthWest) => Ok(North),
            (North, SouthWest) => Ok(West),
            (East, SouthWest) => Ok(South),
            (North, SouthEast) => Ok(East),
            (West, SouthEast) => Ok(South),
            (West, EastWest) => Ok(West),
            (East, EastWest) => Ok(East),
            (_, Start) => Err(MoveErr::GotToStart),
            _ => Err(MoveErr::TrueError(format!(
                "Can't get from {from:?} to {self:?}"
            ))),
        }
    }
}

impl Display for Pipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Ground => " ",
            Start => "S",
            NorthEast => "╚",
            NorthSouth => "║",
            NorthWest => "╝",
            SouthWest => "╗",
            SouthEast => "╔",
            EastWest => "═",
        };

        write!(f, "{}", c)
    }
}

pub type Map = Vec<Vec<((usize, usize), Pipe)>>;

fn print_map(map: &Map) {
    for line in map.iter() {
        for (_, pipe) in line.iter() {
            print!("{pipe}");
        }
        println!();
    }
}

// #[derive(Debug)]
pub struct ParsedMap {
    start: (usize, usize),
    map: Map,
    rows: usize,
    cols: usize,
}

const fn move_to(pos: (usize, usize), direction: &Direction) -> (usize, usize) {
    match *direction {
        North => (pos.0 - 1, pos.1),
        South => (pos.0 + 1, pos.1),
        East => (pos.0, pos.1 + 1),
        West => (pos.0, pos.1 - 1),
    }
}

fn part1(parsed_map: &ParsedMap) -> i64 {
    let start_directions = vec![North, South, East, West]
        .into_iter()
        .filter(|dir| {
            let (i, j) = move_to(parsed_map.start, dir);

            parsed_map.map[i][j].1.next(dir).is_ok()
        })
        .collect::<Vec<_>>();

    let start_dir = start_directions[0];

    let ParsedMap { map, start, .. } = parsed_map;

    let mut loop_length = 1;
    let mut pos = move_to(*start, &start_dir);
    let mut dir = start_dir;
    while map[pos.0][pos.1].1 != Start {
        let to_dir = map[pos.0][pos.1].1.next(&dir);

        let to_dir = to_dir.expect("The map to have a correct traversable loop");
        pos = move_to(pos, &to_dir);
        loop_length += 1;
        dir = to_dir;
    }

    loop_length / 2
}

pub struct Day10;
impl Day for Day10 {
    type ParsedInput = ParsedMap;

    fn day_file() -> &'static str {
        "day10.txt"
    }

    fn parse_input(input: &str) -> Self::ParsedInput {
        let lines = input.lines().collect::<Vec<_>>();

        // let mut raw_map = vec![vec![Ground; lines[0].len()]; lines.len()];
        let map = input
            .lines()
            .enumerate()
            .map(|(i, line)| {
                line.char_indices()
                    .map(|(j, c)| ((i, j), Pipe::from_char(c)))
                    .collect()
            })
            .collect::<Vec<Vec<((usize, usize), Pipe)>>>();

        let (start, _) = map
            .iter()
            .flatten()
            .find(|(_, pipe)| *pipe == Start)
            .unwrap();

        let rows = map.len();
        let cols = map[0].len();

        ParsedMap {
            start: *start,
            map,
            rows,
            cols,
        }
    }

    fn solve(input: Self::ParsedInput) -> super::Solution {
        // print_map(&input.map);

        Solution {
            part1: Number(part1(&input)),
            part2: Todo,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_solution() {
        let result = Day10::solve_days_input();

        assert_eq!(
            result,
            Solution {
                part1: Number(7093),
                part2: Todo
            }
        )
    }
}
