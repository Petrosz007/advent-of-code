use std::collections::HashMap;
use std::collections::BinaryHeap;

fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let mut graph:   HashMap<&str, Vec<&str>> = HashMap::new();
    let mut dist:    HashMap<&str, i32>  = HashMap::new();
    let mut visited:    HashMap<&str, bool>  = HashMap::new();
    for edge in contents.lines() {
        let uv: Vec<&str> = edge.split(")").collect();
        graph.entry(uv[0]).or_insert(Vec::new()).push(uv[1]);
        visited.insert(uv[0], false);

        graph.entry(uv[1]).or_insert(Vec::new()).push(uv[0]);
        visited.insert(uv[1], false);
    }

    dist.insert("COM", 0);
    visited.insert("COM", true);
    let mut queue: BinaryHeap<&str> = BinaryHeap::new();
    queue.push("COM");

    let mut sum = 0;
    while !queue.is_empty() {
        let u = queue.pop().unwrap();
        let udist: i32 = *dist.get(u).unwrap();

        if let Some(vec) = graph.get(u) { 
            for v in vec {
                if !visited.get(v).unwrap() {
                    sum += udist + 1;
                    dist.insert(v, udist + 1);
                    visited.insert(v, true);
                    queue.push(v);
                }
            }
        }
    }

    println!("Part 1: {}", sum);

    for v in visited.values_mut() {
        *v = false;
    }

    dist.insert("YOU", -1);
    visited.insert("YOU", true);
    let mut queue: BinaryHeap<&str> = BinaryHeap::new();
    queue.push("YOU");

    let mut santa_dist = 0;
    'outer: while !queue.is_empty() {
        let u = queue.pop().unwrap();
        let udist: i32 = *dist.get(u).unwrap();

        if let Some(vec) = graph.get(u) { 
            for v in vec {
                if v == &"SAN" {
                    santa_dist = udist;
                    break 'outer;
                }
                if !visited.get(v).unwrap() {
                    dist.insert(v, udist + 1);
                    visited.insert(v, true);
                    queue.push(v);
                }
            }
        }
    }

    println!("Part 2: {}", santa_dist);
}
