use std::collections::HashMap;

mod svg;

fn main() {
    let width = 25usize;
    let height = 6usize;

    let contents = std::fs::read_to_string("input.txt")
        .expect("Error opening file!");

    let chars: Vec<char> = contents.chars().collect();
    let mut result: Vec<(usize, usize)> = Vec::new();
    for i in 0..(contents.len() / (width * height)) {
        let mut count: Vec<usize> = vec![0,0,0];
        for j in 0..(width * height) {
            match chars[i*(width*height)+j] {
                '0' => count[0] += 1,
                '1' => count[1] += 1,
                '2' => count[2] += 1,
                 x  => println!("Error, unrecognised character '{}'!", x),
            }
        }
        result.push((count[0], count[1] * count[2]));
    }

    let (_, part1) = result.iter().min_by_key(|(x, _)| x).unwrap();
    println!("Part 1: {}", part1);



    let mut message: HashMap<(usize, usize), char> = HashMap::new();
    for i in 0..height {
        for j in 0..width {
            message.entry((i, j)).or_insert('2');
        }
    }

    for layer in 0..(contents.len() / (width * height)) {
        for i in 0..height {
            for j in 0..width {
                if *message.get(&(i, j)).unwrap() == '2' && chars[layer*(width*height) + (i * width) + j] != '2' {
                    message.insert((i, j), chars[layer*(width*height) + (i * width) + j]);
                }
            }
        }
    }

    println!("Part 2:");
    for i in 0..height {
        for j in 0..width {
            match *message.get(&(i, j)).unwrap() {
                '0' => print!(" "),
                '1' => print!("{}", '\u{2588}'),
                 _  => print!("?"),
            }
        }
        println!("");
    }

    svg::write_svg(message, "index.html");
}
