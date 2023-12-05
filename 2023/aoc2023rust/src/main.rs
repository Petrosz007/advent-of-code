#![warn(
    clippy::all,
    // clippy::restriction,
    // clippy::pedantic,
    clippy::nursery,
    // clippy::cargo
)]
#![allow(clippy::missing_docs_in_private_items, clippy::implicit_return)]

use days::Day;

use crate::days::{day3};

mod days;
mod utils;

fn main() {
    let day_to_run = 3;
    let solve = match day_to_run {
        3 => Day::<3, day3::ParsedInput>::solve_days_input,

        unimplemented_day => panic!("Day {} is not implemented yet!", unimplemented_day),
    };

    println!("Day {}:", day_to_run);
    let solution = solve(&days::Days::new());

    println!("{solution}");
}
