pub fn execute_at(xs: &Vec<i32>, ind: usize, input: &i32) -> Result<(Vec<i32>, usize), Vec<i32>> {
    let mut ys = xs.clone();
    let op = ((xs[ind] / 10) % 10) * 10 + xs[ind] % 10;
    let p1mode = ((xs[ind] / 100) % 10) == 0;
    let p2mode = ((xs[ind] / 1000) % 10) == 0;
    let p3mode = ((xs[ind] / 10000) % 10) == 0;
    let modes = vec![false, p1mode, p2mode, p3mode];

    let fetch = |i: usize| {
        if !modes[i] {
            xs[ind+i]
        } else {
            xs[xs[ind+i] as usize]
        }
    };

    match op {
        99 => Err(ys),
        1 => {
            ys[xs[ind+3] as usize] = fetch(1) + fetch(2);
            Ok((ys, ind+4))
        },
        2 => {
            ys[xs[ind+3] as usize] = fetch(1) * fetch(2);
            Ok((ys, ind+4)) 
        },
        3 => {
            ys[xs[ind+1] as usize] = *input;
            Ok((ys, ind+2))
        },
        4 => {
            //println!("Output: {}", fetch(1));
            print_output(&fetch(1));
            Ok((ys, ind+2))
        },
        5 => {
            if fetch(1) != 0 {
                Ok((ys, fetch(2) as usize))
            } else {
                Ok((ys, ind+3))
            }
        },
        6 => {
            if fetch(1) == 0 {
                Ok((ys, fetch(2) as usize))
            } else {
                Ok((ys, ind+3))
            }
        },
        7 => {
            ys[xs[ind+3] as usize] = {
                if fetch(1) < fetch(2) { 1 }
                else { 0 }
            };
            Ok((ys, ind+4)) 
        },
        8 => {
            ys[xs[ind+3] as usize] = {
                if fetch(1) == fetch(2) { 1 }
                else { 0 }
            };
            Ok((ys, ind+4)) 
        },
        x => {
            println!("Invalid operation {} at {}!", x, ind);
            Err(ys)
        },
    }
}

pub fn execute_program(xs: &Vec<i32>, input: &i32) -> Vec<i32> {
    let mut ind = 0usize;
    let mut ys = xs.clone();
    loop {
        match execute_at(&ys, ind, input) {
            Ok((x, ip)) => {
                ys = x;
                ind = ip;
            },
            Err(x) => {
                return x; 
            },
        }
    }
}

fn print_output(x: &i32) {
    if *x != 0 {
        println!("{}", x);
    }
}

fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let xs: Vec<i32> = contents
        .lines().next().unwrap()
        .split(',')
        .map(|s| s.parse::<i32>().unwrap())
        .collect();

    print!("Part 1: ");
    execute_program(&xs, &1);

    print!("Part 2: ");
    execute_program(&xs, &5);
}
