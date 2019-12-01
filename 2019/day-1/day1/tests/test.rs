#[path = "../src/main.rs"]
mod main;

use crate::main::calc;
use crate::main::calc2;

#[test]
fn test_calc() {
    assert_eq!(calc(&0), 0);
    assert_eq!(calc(&12), 2);
    assert_eq!(calc(&14), 2);
    assert_eq!(calc(&1969), 654);
    assert_eq!(calc(&100756), 33583);
}

#[test]
fn test_calc2() {
    assert_eq!(calc2(&14), 2);
    assert_eq!(calc2(&1969), 966);
    assert_eq!(calc2(&100756), 50346);
}