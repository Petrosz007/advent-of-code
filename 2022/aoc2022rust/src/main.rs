use days::Day;

mod days;

fn main() {
    let solution = days::Days::new().solve_days_input();

    println!("{solution}");
}
