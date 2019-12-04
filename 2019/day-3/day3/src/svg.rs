pub fn to_svg_line(xs: &std::collections::HashMap<(i32, i32), i32>, color: &str) -> String {
    xs.iter().map(|((x,y), _)| format!("<rect x=\"{}\" y=\"{}\" width=\"1\" height=\"1\" fill=\"{}\"/>", 
        x/10 + 1500, y/10 + 1500, color)).collect::<Vec<String>>().join("\n")
}

pub fn to_svg_circle(xs: &(&(i32, i32), &i32), color: &str) -> String {
    let ((x,y), _) = xs;
    format!("<circle cx=\"{}\" cy=\"{}\" r=\"2\" fill=\"{}\" />", x/10 + 1500, y/10 + 1500, color)
}
