use std::collections::HashSet;
use gcd::Gcd;

fn tan(a: &(i32, i32), b: &(i32, i32)) -> (i32, i32, i32) {
    let u = (b.1 - a.1).abs();
    let v = (b.0 - a.0).abs();
    let gcd = (u as u32).gcd(v as u32) as i32;
    let quarter = if b.0 > a.0 {
        if b.1 > a.1 {
            1
        } else {
            2
        }
    } else {
        if b.1 < a.1 {
            3
        } else {
            4
        }
    };

    (u / gcd, v / gcd, quarter)
}

fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let parsable: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect::<Vec<char>>()).collect();
    
    let mut asteroids: Vec<(i32, i32)> = Vec::new();

    for i in 0..parsable.len() {
        for j in 0..parsable[i].len() {
            if parsable[i][j] == '#' {
                asteroids.push((j as i32,i as i32));
            }
        }
    }

    let mut best = 0;
    for asteroid in asteroids.clone() {
        let mut tans = HashSet::new();
        for target in asteroids.clone() {
            if target == asteroid {continue;}

            tans.insert(tan(&asteroid, &target));
        }

        if tans.len() > best {
            best = tans.len();
        }
    }

    println!("Part 1: {}", best);
}
