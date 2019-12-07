use permutohedron::LexicalPermutation;

pub fn execute_at(xs: &Vec<i32>, ind: usize, input: Vec<i32>) -> Result<(Vec<i32>, usize, Vec<i32>, Option<i32>), Vec<i32>> {
    let mut ys = xs.clone();

    let op = xs[ind] % 100;
    let pmode = |i: usize| ((xs[ind] / (10i32.pow(i as u32 + 1))) % 10) == 0;
    let fetch = |i: usize| {
        if pmode(i) {
            xs[xs[ind+i] as usize]
        } else {
            xs[ind+i]
        }
    };
    let mut store = |i: usize, x: i32| ys[xs[ind+i] as usize] = x;

    match op {
        // Exit
        99 => Err(ys),

        // Add
        1 => {
            store(3, fetch(1) + fetch(2));
            Ok((ys, ind+4, input, None))
        },

        // Multiply
        2 => {
            store(3, fetch(1) * fetch(2));
            Ok((ys, ind+4, input, None)) 
        },

        // Read input
        3 => {
            // Waits until it gets a valid input
            if input.len() == 0 {
                Ok((ys, ind, input, None))
            } else {
                store(1, input[0]);
                Ok((ys, ind+2, input[1..].to_vec(), None))
            }
        },

        // Output
        4 => {
            Ok((ys, ind+2, input, Some(fetch(1))))
        },

        // Non-zero
        5 => {
            if fetch(1) != 0 {
                Ok((ys, fetch(2) as usize, input, None))
            } else {
                Ok((ys, ind+3, input, None))
            }
        },

        // Is zero
        6 => {
            if fetch(1) == 0 {
                Ok((ys, fetch(2) as usize, input, None))
            } else {
                Ok((ys, ind+3, input, None))
            }
        },

        // Less than
        7 => {
            store(3, {
                if fetch(1) < fetch(2) { 1 }
                else { 0 }
            });
            Ok((ys, ind+4, input, None)) 
        },

        // Equals
        8 => {
            store(3, {
                if fetch(1) == fetch(2) { 1 }
                else { 0 }
            });
            Ok((ys, ind+4, input, None)) 
        },

        // Error
        x => {
            println!("Invalid operation {} at {}!", x, ind);
            Err(ys)
        },
    }
}

pub fn execute_program(xs: &Vec<i32>, inp: &Vec<i32>) -> Vec<i32> {
    let mut ip = 0usize;
    let mut ys = xs.clone();
    let mut input = inp.clone();
    let mut output = Vec::new();
    loop {
        match execute_at(&ys, ip, input) {
            Ok((x, new_ip, new_input, plus_output)) => {
                ys = x;
                ip = new_ip;
                input = new_input;
                if let Some(out) = plus_output {
                    output.push(out);
                }
            },
            Err(_) => {
                return output; 
            },
        }
    }
}

pub fn execute_amplifiers(xs: &Vec<i32>, inp: &Vec<i32>) -> i32 {
    let mut ip: Vec<usize> = vec![0              , 0           , 0           , 0           , 0           ];
    let mut ys             = vec![xs.clone()     , xs.clone()  , xs.clone()  , xs.clone()  , xs.clone()  ];
    let mut input          = vec![vec![inp[0], 0], vec![inp[1]], vec![inp[2]], vec![inp[3]], vec![inp[4]]];
    let mut output         = vec![Vec::new()     , Vec::new()  , Vec::new()  , Vec::new()  , Vec::new()  ];

    loop {
        for i in 0..=4 {
            match execute_at(&ys[i], ip[i], input[i].clone()) {
                Ok((x, new_ip, new_input, plus_output)) => {
                    ys[i] = x;
                    ip[i] = new_ip;
                    input[i] = new_input;
                    if let Some(out) = plus_output {
                        output[i].push(out);
                        input[(i+1) % 5].push(out);
                    }
                },
                Err(_) if i == 4 => {
                    return *output[4].last().unwrap();
                },
                Err(_) => {
                    ;
                },
            }
        }
    }
}

pub fn optimise_thrusters(xs: &Vec<i32>, inputs: &Vec<i32>) -> i32 {
    let mut permutations = inputs.clone();
    let mut outputs = Vec::new();
    loop {
        outputs.push(execute_amplifiers(&xs, &permutations));
        if !permutations.next_permutation() {
            break;
        }
    }

    *outputs.iter().max().unwrap()
}


fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let xs: Vec<i32> = contents
        .lines().next().unwrap()
        .split(',')
        .map(|s| s.parse::<i32>().unwrap())
        .collect();

    println!("Part 1: {}", optimise_thrusters(&xs, &vec![0,1,2,3,4]));
    println!("Part 2: {}", optimise_thrusters(&xs, &vec![5,6,7,8,9]));
}
