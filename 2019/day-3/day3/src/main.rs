use recap::Recap;
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Debug, Deserialize, Recap, Clone)]
#[recap(regex = r#"(?P<dir>[A-Z])(?P<dist>\d+)"#)]
struct InputInstruction {
    dir: char,
    dist: usize,
}

fn move_once(dir: &char, prev: &(i32, i32)) -> (i32, i32) {
    let (x, y) = *prev;
    match dir {
        'U' => (x, y + 1),
        'D' => (x, y - 1),
        'R' => (x + 1, y),
        'L' => (x - 1, y),
        _ => (0, 0),
    }
}

fn parse_line(line: &str) -> HashMap<(i32, i32), i32> {
    let instructions: Vec<_> = line.split(',').map(|s| s.parse::<InputInstruction>().unwrap()).collect();

    instructions.iter()
        .flat_map(|instr| std::iter::repeat(instr).take((*instr).dist))
        .scan(((0,0), 0), |(prev, step), InputInstruction{dir, ..}| {
            *prev = move_once(&dir, &prev);
            *step += 1;
            Some((*prev, *step))
        })
        .fold(HashMap::new(), |mut map, (pos, step)| {
            map.entry(pos).or_insert(step);
            map
        })
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
        .map(|((x,y), _)| x.abs() + y.abs())
        .min()
        .unwrap();

    let closest2 = intersections.iter()
        .map(|(pos, step)| *step + line1.get(pos).unwrap())
        .min()
        .unwrap();

    println!("Part 1: {}", closest1);
    println!("Part 2: {}", closest2);
}