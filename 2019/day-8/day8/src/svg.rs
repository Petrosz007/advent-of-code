pub fn to_svg_rect(pos: &(usize, usize), color: &str) -> String {
    format!("<rect x=\"{}\" y=\"{}\" width=\"50\" height=\"50\" fill=\"{}\"/>",
        pos.1 * 50, pos.0 * 50, color
    )
}

pub fn wrap_svg(s: &str) -> String {
    format!("<html><body><svg width=\"{}\" height=\"{}\">{}</svg></body></html>", 
        25 * 50, 6 * 50, s
    )
}

pub fn write_svg(map: std::collections::HashMap<(usize, usize), char>, fname: &str) {
    let mut rects  = String::new();
    for ((x,y), color) in map.iter() {
        match color {
            '0' => rects += &to_svg_rect(&(*x, *y), "white"),
            '1' => rects += &to_svg_rect(&(*x, *y), "black"),
             _  => rects += "",
        };
    }

    let svg = wrap_svg(&rects);
    std::fs::write(fname, svg).expect("Error writing svg!");
}