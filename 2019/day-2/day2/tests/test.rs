#[path = "../src/main.rs"]
mod main;

use crate::main::execute_at;
use crate::main::execute_program;

#[test]
fn test_execute_at() {
    assert_eq!(Ok(vec![2,0,0,0,99]), execute_at(&vec![1,0,0,0,99], 0));
    assert_eq!(Ok(vec![2,3,0,6,99]), execute_at(&vec![2,3,0,3,99], 0));
    assert_eq!(Err(vec![99,0,0,0,99]), execute_at(&vec![99,0,0,0,99], 0));
}

#[test]
fn test_execute_program() {
    assert_eq!(vec![2,0,0,0,99], execute_program(&vec![1,0,0,0,99]));
    assert_eq!(vec![2,3,0,6,99], execute_program(&vec![2,3,0,3,99]));
    assert_eq!(vec![30,1,1,4,2,5,6,0,99], execute_program(&vec![1,1,1,4,99,5,6,0,99]));
    assert_eq!(vec![3500,9,10,70,2,3,11,0,99,30,40,50], execute_program(&vec![1,9,10,3,2,3,11,0,99,30,40,50]));
}