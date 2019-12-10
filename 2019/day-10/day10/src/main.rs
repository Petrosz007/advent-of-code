use std::collections::HashSet;
use std::hash::{Hash, Hasher};

// fn manhattan(x: &(i32, i32), y: &(i32, i32)) -> i32 {
//     (x.0 - y.0).abs() + (x.1 - y.1).abs()
//     // ((x.0 - y.0)*(x.0 - y.0) + (x.1 - y.1)*(x.1 - y.1)).sqrt()
// }

// fn on_line(a: &(i32, i32), b: &(i32, i32), c: &(i32, i32)) -> bool {
//     (b.1 - a.1)*(c.0 - a.0) - (c.1 - a.1)*(b.0 - a.0) == 0.0
// }

struct dist {
    u: i32,
    v: i32,
}

impl PartialEq for dist {
    fn eq(&self, other: &dist) -> bool {
        self.u as f64 / self.v as f64 ==  other.u as f64 / other.v as f64
    }
}

impl Eq for dist {}

impl Hash for dist {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (((self.u as f64 / self.v as f64) * 100000.0) as i64).hash(state);
    }
}

fn tan(a: &(i32, i32), b: &(i32, i32)) -> dist {
    dist {
        u: (b.1 - a.1).abs(), 
        v: (b.0 - a.0).abs(),
    }
}

fn main() {
    let contents = std::fs::read_to_string("test.txt")
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

    // let mut best = 0;
    // let mut best_pos = (0.0,0.0); 
    // for asteroid in asteroids.clone() {
    //     let mut count = 0;

    //     for target in asteroids.clone() {
    //         if target == asteroid {continue;}

    //         let mut interferes = false;

    //         for int in asteroids.clone() {
    //             if int == asteroid || int == target {continue;}

    //             if on_line(&asteroid, &target, &int) && manhattan(&asteroid, &int) < manhattan(&asteroid, &target) {
    //                 interferes = true;
    //                 break;
    //             }
    //         }

    //         if !interferes {
    //             count += 1;
    //         }
    //     }
    //     if asteroid == (5.0,8.0) {
    //         println!("{}", count);
    //     }

    //     if count > best {
    //         best = count;
    //         best_pos = asteroid;
    //     }
    // }

    let mut best = 0;
    let mut best_pos = (0,0); 
    for asteroid in asteroids.clone() {
        let mut tans = HashSet::new();
        for target in asteroids.clone() {
            if target == asteroid {continue;}

            tans.insert(tan(&asteroid, &target));
        }

        if asteroid == (5,8) {
            println!("{}", tans.len());
        }

        if tans.len() > best {
            best = tans.len();
            best_pos = asteroid;
        }
    }

    println!("Part 1: {} {:?}", best, best_pos);
}
