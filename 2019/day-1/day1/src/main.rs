pub fn calc(x: &u32) -> u32 {
    let y = (*x as f64 / 3.0).floor() as u32;

    if y < 2 { 0 } 
    else     { y - 2 }
}

pub fn calc2(x: &u32) -> u32 {
    match calc(x) {
        0 => 0,
        y => y + calc2(&y),
    }
}
    
fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file.");

    let nums: Vec<u32> = contents
        .lines()
        .map(|s| s.parse::<u32>().unwrap())
        .collect();
    
    let solve = |f: Box<&dyn Fn(&u32) -> u32>| nums.iter().fold(0, |acc, x| acc + f(x));

    let part1 = solve(Box::new(&calc));
    let part2 = solve(Box::new(&calc2));

    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
