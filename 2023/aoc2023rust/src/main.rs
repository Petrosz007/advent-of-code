#![warn(
    clippy::all,
    // clippy::restriction,
    // clippy::pedantic,
    clippy::nursery,
    // clippy::cargo
)]
#![allow(clippy::missing_docs_in_private_items, clippy::implicit_return)]

use clap::Parser;
use days::Days;

mod days;
mod utils;

/// Advent of Code code runner
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    day: i32,

    /// Number of times to greet
    #[arg(short, long)]
    file: String,
}

fn main() {
    let args = Args::parse();

    let solution = Days::solve(args.day, &args.file);

    println!("Day {}:", args.day);
    println!("{solution}");
}
