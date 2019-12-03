use recap::Recap;
use serde::Deserialize;
use std::collections::HashSet;

use std::cmp::{min, max};

#[derive(Debug, Deserialize, Recap)]
#[recap(regex = r#"(?P<dir>[A-Z])(?P<dist>\d+)"#)]
struct InputInstruction {
    dir: char,
    dist: i32,
}

#[derive(Debug)]
enum Line {
    Horizontal{y: i32, x1: i32, x2: i32},
    Vertical  {x: i32, y1: i32, y2: i32},
}

fn add_line(prev: &(i32, i32), x: InputInstruction) -> (i32, i32) {
    // println!("{:?}, {:?}", prev, x);
    match x.dir {
        'U' => (prev.0, prev.1 + x.dist),
        'D' => (prev.0, prev.1 - x.dist),
        'R' => (prev.0 + x.dist, prev.1),
        'L' => (prev.0 - x.dist, prev.1),
        _ => (20000, 20000),
    }
}

fn draw_line(line: &str) -> Vec<(i32, i32)> {
    line
        .split(',')
        .map(|s| s.parse::<InputInstruction>().unwrap())
        .fold(vec![(0,0)], |mut acc, x| {
            acc.push(add_line(acc.last().unwrap(), x));
            acc
        })
}

fn to_line(x: &(i32, i32), y: &(i32, i32)) -> Line {
    if x.0 == y.0 {
        Line::Vertical{
            x: x.0,
            y1: x.1,
            y2: y.1
        }
    } else {
        Line::Horizontal{
            y: x.1,
            x1: x.0,
            x2: y.0
        }
    }
}

fn manhattan(x: &i32, y: &i32) -> i32 {
    x.abs() + y.abs()
}

fn main() {
    let contents = std::fs::read_to_string("input.txt").expect("Error opening file!");
    let mut lines = contents.lines();
    let line1_raw = lines.next().unwrap();
    let line2_raw = lines.next().unwrap();

    let line1_points = draw_line(line1_raw);
    let line2_points = draw_line(line2_raw);

    let mut line1_line = Vec::new();
    for i in 0..(line1_points.len() - 1) {
        line1_line.push(to_line(&line1_points[i], &line1_points[i+1]));
    }

    let mut line2_line = Vec::new();
    for i in 0..(line2_points.len() - 1) {
        line2_line.push(to_line(&line2_points[i], &line2_points[i+1]));
    }

    let mut line1 = HashSet::new();
    for line in line1_line {
        match line {
            Line::Horizontal{y,x1,x2} => {
                for x in min(x1, x2)..(max(x1,x2) + 1) {
                    line1.insert((x,y));
                }
            },
            Line::Vertical{x,y1,y2} => {
                for y in min(y1,y2) .. (max(y1,y2) + 1) {
                    line1.insert((x,y));
                }
            }
        }
    }

    line1.remove(&(0,0));

    let mut found = false;
    let mut closest = 0;
    for line in line2_line {
        match line {
            Line::Horizontal{y,x1,x2} => {
                for x in min(x1, x2)..(max(x1,x2) + 1) {
                    if line1.contains(&(x,y)) {
                        let dist = manhattan(&x, &y);
                        if !found {
                            closest = dist;
                            found = true;
                        } else if found && closest > dist {
                            closest = dist;
                        }
                    }
                }
            },
            Line::Vertical{x,y1,y2} => {
                for y in min(y1,y2) .. (max(y1,y2) + 1) {
                    if line1.contains(&(x,y)) {
                        let dist = manhattan(&x, &y);
                        if !found {
                            closest = dist;
                            found = true;
                        } else if found && closest > dist {
                            closest = dist;
                        }
                    }
                }
            }
        }
    }

    println!("Part 1: {}", closest);
}
