#[path = "../src/main.rs"]
mod main;

use crate::main::has_same;
use crate::main::has_same2;
use crate::main::is_increasing;
#[test]
fn test_has_same() {
    assert!(has_same(&111111));
    assert!(has_same(&123411));
    assert!(has_same(&11234));
    assert!(!has_same(&12321));
    assert!(!has_same(&26171));
}

#[test]
fn test_has_same2() {
    assert!(!has_same2(&111111));
    assert!(has_same2(&123411));
    assert!(has_same2(&11234));
    assert!(!has_same2(&123241));
    assert!(has_same2(&1111144));
    assert!(!has_same2(&11111444));
    assert!(has_same2(&1133111444));
    assert!(has_same2(&1133311444));
}

#[test]
fn test_is_increasing() {
    assert!(is_increasing(&1234));
    assert!(is_increasing(&13789));
    assert!(!is_increasing(&91234));
    assert!(!is_increasing(&12342));
    assert!(!is_increasing(&16578));
    assert!(!is_increasing(&9876543));
}
