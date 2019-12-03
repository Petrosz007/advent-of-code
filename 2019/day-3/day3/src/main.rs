use recap::Recap;
use serde::Deserialize;

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
    println!("{:?}, {:?}", prev, x);
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
    if y.0 == y.0 {
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

fn contains(x1: &i32, x2: &i32, y1: &i32, y2: &i32) -> bool {
    let minx = min(x1, x2);
    let maxx = max(x1, x2);
    let miny = min(y1, y2);
    let maxy = max(y1, y2);

    (minx <= maxy && miny <= maxx) || 
    (miny <= maxx && minx <= maxy)
}

fn contains2(x1: &i32, x2: &i32, y: &i32) -> bool {
    let minx = min(x1, x2);
    let maxx = max(x1, x2);

    minx <= y && y <= maxx
}

fn lines_cross(x: &Line, y: &Line) -> bool {
    match (x,y) {
        (Line::Horizontal{y: i, x1: j1, x2: j2}, Line::Horizontal{y: a, x1: b1, x2: b2}) => {
            i == a && contains(j1, j2, b1, b2)
        },
        (Line::Vertical{x: i, y1: j1, y2: j2}, Line::Vertical{x: a, y1: b1, y2: b2}) => {
            i == a && contains(j1, j2, b1, b2)
        },
        (Line::Horizontal{y: i, x1: j1, x2: j2}, Line::Vertical{x: a, y1: b1, y2: b2}) => {
            contains2(j1, j2, a) && contains2(b1, b2, i)
        },
        (Line::Vertical{x: i, y1: j1, y2: j2}, Line::Horizontal{y: a, x1: b1, x2: b2}) => {
            contains2(j1, j2, a) && contains2(b1, b2, i)
        },
    }
}

fn from_origo(x: &Line) -> bool {
    match x {
        Line::Horizontal{y: 0, x1, x2} => *x1 == 0 || *x2 == 0,
        Line::Vertical{x: 0, y1, y2} => *y1 == 0 || *y2 == 0,
        _ => false,
    }
}

fn closest_intersection(x: &Line, y: &Line) -> Option<i32> {
    /*if from_origo(x) || from_origo(y) {
        None
    } else */if !lines_cross(x, y) { 
        None 
    } else {
        match (x,y) {
            (Line::Horizontal{y: i, x1: j1, x2: j2}, Line::Horizontal{y: a, x1: b1, x2: b2}) => {
                Some(min(min(manhattan(&i, &b1), manhattan(&i, &j1)),
                    min(manhattan(&i, &b2), manhattan(&i, &j2))))
            },
            (Line::Vertical{x: i, y1: j1, y2: j2}, Line::Vertical{x: a, y1: b1, y2: b2}) => {
                Some(min(min(manhattan(&i, &b1), manhattan(&i, &j1)),
                    min(manhattan(&i, &b2), manhattan(&i, &j2))))
            },
            (Line::Horizontal{y: i, x1: j1, x2: j2}, Line::Vertical{x: a, y1: b1, y2: b2}) => {
                Some(manhattan(&i, &a))
            },
            (Line::Vertical{x: i, y1: j1, y2: j2}, Line::Horizontal{y: a, x1: b1, x2: b2}) => {
                Some(manhattan(&i, &a))
            },
        }
    }
}

fn main() {
    let contents = std::fs::read_to_string("test.txt").expect("Error opening file!");
    let mut lines = contents.lines();
    let line1_raw = lines.next().unwrap();
    let line2_raw = lines.next().unwrap();

    let line1_points = draw_line(line1_raw);
    // let line1: Vec<Line> = line1_points.iter().zip(line1_points.iter().next()).map(|x| to_line(x)).collect();

    let line2_points = draw_line(line2_raw);
    // let line2: Vec<Line> = line2_points.iter().zip(line2_points.iter().next()).map(|x| to_line(x)).collect();

    // let closest_dist = line2.iter().map(|y| line1.iter().map(|x| closest_intersection(&x, &y)).filter_map(Option::Some).max()).max();
    let mut line1: Vec<Line> = Vec::new();
    for i in 0..(line1_points.len() - 2) {
        line1.push(to_line(&line1_points[i], &line1_points[i+1]));
    }
    
    let mut line2: Vec<Line> = Vec::new();
    for i in 0..(line2_points.len() - 2) {
        line2.push(to_line(&line2_points[i], &line2_points[i+1]));
    }
    
    // println!("{:?}", line1);
    // println!("{:?}", line2);
    
    for l1 in &line1 {
        for l2 in &line2 {
            if let Some(i) = closest_intersection(&l1, &l2) {
                print!("{} ", i);
            }
        }
    }

    // println!("{:?}", closest_dist);
}
