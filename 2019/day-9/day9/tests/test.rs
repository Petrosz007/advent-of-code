#[path="../src/main.rs"]
mod main;

use main::execute_program;

#[test]
fn test_whole() {
    assert_eq!(execute_program(&vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99], &vec![]), [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]);
    assert_eq!(execute_program(&vec![1102,34915192,34915192,7,4,7,99,0], &vec![]), [34915192*34915192]);
    assert_eq!(execute_program(&vec![104,1125899906842624,99], &vec![]), [1125899906842624]);
    assert_eq!(execute_program(&vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], &vec![7]), [999]);
    assert_eq!(execute_program(&vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], &vec![8]), [1000]);
    assert_eq!(execute_program(&vec![3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], &vec![9]), [1001]);
}