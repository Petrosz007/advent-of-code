mod icpu;
use icpu::{execute_at, IntCodeCPUState};
use std::collections::HashMap;

enum Color { Black, White }
pub enum Dir { Up, Down, Left, Right }

impl Color {
    pub fn to_instr(&self) -> i128 {
        match &self {
            Color::Black => 0,
            Color::White => 1,
        }
    } 
}

pub fn step(pos: &(i32, i32), dir: &Dir, next_dir: &i128) -> ((i32, i32), Dir) {
    if *next_dir == 0 {
        match dir {
            Dir::Up => ((pos.0 - 1, pos.1), Dir::Left),
            Dir::Left => ((pos.0, pos.1 - 1), Dir::Down),
            Dir::Down => ((pos.0 + 1, pos.1), Dir::Right),
            Dir::Right => ((pos.0, pos.1 + 1), Dir::Up),
        }
    } else {
        match dir {
            Dir::Up => ((pos.0 + 1, pos.1), Dir::Right),
            Dir::Left => ((pos.0, pos.1 + 1), Dir::Up),
            Dir::Down => ((pos.0 - 1, pos.1), Dir::Left),
            Dir::Right => ((pos.0, pos.1 - 1), Dir::Down),
        }
    }
}

pub fn execute_program(xs: &Vec<i128>) -> usize {
    let mut state = IntCodeCPUState::new(xs, &Vec::<i128>::new());
    let mut output_num = 0;

    let mut tiles: HashMap<(i32,i32), Color> = HashMap::new();
    let mut pos = (0,0);
    let mut dir = Dir::Up;
    tiles.insert(pos, Color::Black);
    state.input.push(tiles.entry(pos).or_insert(Color::Black).to_instr());

    loop {
        state = execute_at(&state);

        if state.output.len() >= output_num + 2 {
            output_num = state.output.len();
            let color = if state.output[state.output.len() - 2] == 0 {
                Color::Black
            } else {
                Color::White
            };

            tiles.insert(pos, color);

            let (pos2, dir2) = step(&pos, &dir, state.output.last().unwrap());
            pos = pos2;
            dir = dir2;

            state.input.push(tiles.entry(pos).or_insert(Color::Black).to_instr());
        }

        if state.halted {
            return tiles.len();
        }
    }
}


fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let xs: Vec<i128> = contents
        .lines().next().unwrap()
        .split(',')
        .map(|s| s.parse::<i128>().unwrap())
        .collect();

    println!("Part 1: {}", execute_program(&xs));
}
