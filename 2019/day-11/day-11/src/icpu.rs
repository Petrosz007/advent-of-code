use std::collections::HashMap;

#[derive(Clone)]
pub struct IntCodeCPUState {
    pub memory: HashMap<usize, i128>,
    pub ip: usize,
    pub rb: usize,
    pub input: Vec<i128>,
    pub output: Vec<i128>,
    pub halted: bool,
}

impl IntCodeCPUState {
    pub fn new(xs: &Vec<i128>, input: &Vec<i128>) -> IntCodeCPUState {
        let memory: HashMap<usize, i128> = (0usize..).zip(xs.iter()).map(|(x,y)| (x,*y)).collect();
        IntCodeCPUState {
            memory,
            ip: 0,
            rb: 0,
            input: input.clone(),
            output: Vec::new(),
            halted: false,
        }
    }
}

pub fn execute_at(state: &IntCodeCPUState) -> IntCodeCPUState {
    let mut new_state = state.clone();

    let op     = state.memory[&state.ip] % 100;
    let pmode  = |i: usize| ((state.memory[&state.ip] / (10i128.pow(i as u32 + 1))) % 10);
    let get_at = |i: usize| -> i128 { *state.memory.get(&i).unwrap_or(&0) };
    let fetch  = |i: usize| -> i128 {
        match pmode(i) {
            0 => get_at(get_at(state.ip+i) as usize),
            1 => get_at(state.ip+i),
            2 => get_at((get_at(state.ip+i) + state.rb as i128) as usize),
            x => {
                eprintln!("Error: Unknown mode: '{}'", x);
                0
            }
        }
    };

    let mut store = |i: usize, x: i128| {
        match pmode(i) {
            0 => new_state.memory.insert(get_at(state.ip+i) as usize, x),
            2 => new_state.memory.insert((get_at(state.ip+i) + state.rb as i128) as usize, x),
            x => {
                eprintln!("Error: Cannot write with mode '{}'", x);
                None
            },
        }
    };

    match op {
        // Exit
        99 => new_state.halted = true,

        // Add
        1 => {
            store(3, fetch(1) + fetch(2));
            new_state.ip += 4;
        },

        // Multiply
        2 => {
            store(3, fetch(1) * fetch(2));
            new_state.ip += 4;
        },

        // Read input
        3 => {
            // Waits until it gets a valid input
            if state.input.len() == 0 {
                ;
            } else {
                store(1, state.input[0]);
                new_state.ip += 2;
                new_state.input = state.input[1..].to_vec();
            }
        },

        // Output
        4 => {
            new_state.ip += 2;
            new_state.output.push(fetch(1));
        },

        // Non-zero
        5 => {
            if fetch(1) != 0 {
                new_state.ip = fetch(2) as usize;
            } else {
                new_state.ip += 3;
            }
        },

        // Is zero
        6 => {
            if fetch(1) == 0 {
                new_state.ip = fetch(2) as usize;
            } else {
                new_state.ip += 3;
            }
        },

        // Less than
        7 => {
            store(3, {
                if fetch(1) < fetch(2) { 1 }
                else { 0 }
            });
            new_state.ip += 4;
        },

        // Equals
        8 => {
            store(3, {
                if fetch(1) == fetch(2) { 1 }
                else { 0 }
            });
            new_state.ip += 4;
        },

        9 => {
            new_state.rb = (new_state.rb as i128 + fetch(1)) as usize;
            new_state.ip += 2;
        },

        // Error
        x => {
            println!("Invalid operation {} at {}!", x, state.ip);
        },
    }

    new_state
}
