#![warn(
    clippy::all,
    // clippy::restriction,
    // clippy::pedantic,
    clippy::nursery,
    // clippy::cargo
)]

use days::Day;

use crate::days::{day1, day2, day3, day4};

mod days;
mod utils;

fn main() {
    let day_to_run = 4;
    let solve = match day_to_run {
        1 => Day::<1, day1::ParsedInput>::solve_days_input,
        2 => Day::<2, day2::ParsedInput>::solve_days_input,
        3 => Day::<3, day3::ParsedInput>::solve_days_input,
        4 => Day::<4, day4::ParsedInput>::solve_days_input,

        unimplemented_day => panic!("Day {} is not implemented yet!", unimplemented_day),
    };

    println!("Day {}:", day_to_run);
    let solution = solve(&days::Days::new());

    println!("{solution}");
}
