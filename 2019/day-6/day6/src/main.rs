use std::collections::HashMap;

fn part1_bfs<'a>(u: &'a str, graph: &HashMap<&'a str, Vec<&'a str>>, visited: &mut HashMap<&'a str, bool>, dist: u32) -> u32 {
    if visited[u] {
        return 0;
    }

    visited.insert(u, true);

    let mut sum = dist;
    for v in graph.get(u).unwrap() {
        sum += part1_bfs(v, &graph, visited, dist + 1);
    }

    sum
}

fn part2_bfs<'a>(u: &'a str, graph: &HashMap<&'a str, Vec<&'a str>>, visited: &mut HashMap<&'a str, bool>, dist: u32) -> u32 {
    visited.insert(u, true);

    let mut san_dist = 0;
    for v in graph.get(u).unwrap() {
        if v == &"SAN" {
            return dist;
        }
        if !visited[v] {
            san_dist += part2_bfs(v, &graph, visited, dist + 1);
        }
    }

    san_dist
}

fn main() {
    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let mut graph:   HashMap<&str, Vec<&str>> = HashMap::new();
    let mut visited: HashMap<&str, bool     > = HashMap::new();
    for edge in contents.lines() {
        let uv: Vec<&str> = edge.split(")").collect();

        graph.entry(uv[0]).or_insert(Vec::new()).push(uv[1]);
        graph.entry(uv[1]).or_insert(Vec::new()).push(uv[0]);

        visited.insert(uv[0], false);
        visited.insert(uv[1], false);
    }

    println!("Part 1: {}", part1_bfs("COM", &graph, &mut visited, 0));

    for v in visited.values_mut() {
        *v = false;
    }

    println!("Part 2: {}", part2_bfs("YOU", &graph, &mut visited, 0) - 1);
}
