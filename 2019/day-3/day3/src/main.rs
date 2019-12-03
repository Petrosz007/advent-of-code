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

fn manhattan(x: &(i32, i32)) -> i32 {
    x.0.abs() + x.1.abs()
}

fn main() {
    let contents = std::fs::read_to_string("input.txt").expect("Error opening file!");
    let mut lines = contents.lines();
    let line1_raw: Vec<InputInstruction> = lines.next().unwrap().split(',').map(|s| s.parse().unwrap()).collect();
    let line2_raw: Vec<InputInstruction> = lines.next().unwrap().split(',').map(|s| s.parse().unwrap()).collect();

    let mut line1 = HashMap::new();
    let mut steps = 1;
    let mut prev = (0,0);
    for line in line1_raw {
        for _ in 0..line.dist {
            prev = move_once(&line.dir, &prev);
            if !line1.contains_key(&prev) {
                line1.insert(prev, steps);
            }
            steps += 1;
        }
    }

    // Part 1
    let mut found = false;
    let mut closest = 0;
    let mut prev = (0,0);
    for line in &line2_raw {
        for _ in 0..line.dist {
            prev = move_once(&line.dir, &prev);
            if line1.contains_key(&prev) {
                let dist = manhattan(&prev);
                if !found {
                    closest = dist;
                    found = true;
                } else if found && closest > dist {
                    closest = dist;
                }
            }
        }
    }

    println!("Part 1: {}", closest);

    // Part 2
    let mut found = false;
    let mut closest = 0;
    let mut steps = 1;
    let mut prev = (0,0);
    for line in line2_raw {
        for _ in 0..line.dist {
            prev = move_once(&line.dir, &prev);
            if let Some(line1_dist) = line1.get(&prev) {
                let dist = *line1_dist + steps;
                if !found {
                    closest = dist;
                    found = true;
                } else if found && closest > dist {
                    closest = dist;
                }
            }
            steps += 1;
        }
    }

    println!("Part 2: {}", closest);
}
