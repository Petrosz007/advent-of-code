use recap::Recap;
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Debug, Deserialize, Recap)]
#[recap(regex = r#"(?P<dir>[A-Z])(?P<dist>\d+)"#)]
struct InputInstruction {
    dir: char,
    dist: i32,
}

fn move_once(dir: &char, prev: &(i32, i32)) -> (i32, i32) {
    let (x, y) = *prev;
    match dir {
        'U' => (x, y + 1),
        'D' => (x, y - 1),
        'R' => (x + 1, y),
        'L' => (x - 1, y),
        _ => (200000, 200000),
    }
}

fn parse_line(line: &str) -> HashMap<(i32, i32), i32> {
    let instructions = line.split(',').map(|s| s.parse::<InputInstruction>().unwrap());
    let mut map  = HashMap::new();
    let mut steps = 1;
    let mut prev = (0,0);
    for instruction in instructions {
        for _ in 0..instruction.dist {
            prev = move_once(&instruction.dir, &prev);
            map.entry(prev).or_insert(steps);
            steps += 1;
        }
    }

    map
}

fn manhattan(x: &(i32, i32)) -> i32 {
    x.0.abs() + x.1.abs()
}

fn main() {
    let contents = std::fs::read_to_string("input.txt").expect("Error opening file!");
    let mut lines = contents.lines();
    let line1 = parse_line(lines.next().unwrap());
    let line2 = parse_line(lines.next().unwrap());

    let intersections: Vec<_> = line2.iter()
        .filter(|(pos, _)| line1.contains_key(pos))
        .collect();

    let closest1 = intersections.iter()
        .map(|(pos, _)| manhattan(pos))
        .min()
        .unwrap();

    let closest2 = intersections.iter()
        .map(|(pos, step)| *step + line1.get(pos).unwrap())
        .min()
        .unwrap();

    println!("Part 1: {}", closest1);
    println!("Part 2: {}", closest2);
}