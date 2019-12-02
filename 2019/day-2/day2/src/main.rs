pub fn execute_at(xs: &Vec<usize>, ind: usize) -> Result<Vec<usize>, Vec<usize>> {
    let mut ys = xs.clone();
    match xs[ind] {
        99 => Err(ys),
        1 => {
            ys[xs[ind+3]] = ys[xs[ind+1]] + xs[xs[ind+2]];
            Ok(ys)
        },
        2 => {
            ys[xs[ind+3]] = ys[xs[ind+1]] * ys[xs[ind+2]];
            Ok(ys) 
        },
        x => {
            println!("Invalid operation {} at {}!", x, ind);
            Err(ys)
        },
    }
}

pub fn execute_program(xs: &Vec<usize>) -> Vec<usize> {
    let mut ind = 0usize;
    let mut ys = xs.clone();
    loop {
        match execute_at(&ys, ind) {
            Ok(x) => {
                ys = x;
                ind += 4;
            },
            Err(x) => {
                return x; 
            },
        }
    }
}

fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let mut xs: Vec<usize> = contents
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect();

    xs[1] = 12;
    xs[2] = 2;
    
    let ys = execute_program(&xs);

    println!("Part 1: {}", ys[0]);

    'outer: for i in 0..100 {
        for j in 0..100 {
            xs[1] = i;
            xs[2] = j;
            if execute_program(&xs)[0] == 19690720 {
                println!("Part 2: Noun = {}, Verb = {}, result = {}", i, j, 100 * i + j);
                break 'outer;
            }
        }
    }
}
