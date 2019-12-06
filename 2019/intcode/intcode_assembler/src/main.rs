#[derive(Debug)]
enum Parameter {
    Literal(i32),
    Variable(String),
    Label(String),
}

#[derive(Debug)]
struct Operation {
    operator: String,
    parameters: Vec<Parameter>,
    label: Option<String>,
}

#[derive(Debug)]
enum Instruction {
    Operation(Operation),
    Label(String),
}

#[derive(Debug)]
struct Program {
    operations: Vec<Operation>,
    variables: Vec<String>,
}

impl Program {
    fn new() -> Program {
        return Program {
            operations: Vec::new(),
            variables: Vec::new(),
        }
    }
}

fn parse_parameter(s: &str) -> Parameter {
    match s.chars().nth(0).unwrap() {
        '$' => {
            Parameter::Literal(s[1..].parse::<i32>().unwrap())
        },
        '_' => {
            Parameter::Label(String::from(&s[1..]))
        },
        _ => {
            Parameter::Variable(String::from(&s[1..]))
        },
    }
}

fn parse_line(line: &str) -> Result<Instruction, String> {
    let op1 = vec!["var", "inp", "out", "die"];
    let op2 = vec![];
    let op3 = vec!["add", "mul", "var", "equ", "inp", "out", "die"];
    let parts: Vec<&str> = line.trim().split_ascii_whitespace().collect();

    match parts[0] {
        "label" => {
            let mut label = String::from(parts[1]);
            label.truncate(parts[1].len() - 1);
            Ok(Instruction::Label(label))
        },
        x if op1.contains(&x) => {
            Ok(Instruction::Operation(Operation {
                operator: (String::from(parts[0])),
                parameters: vec![parse_parameter(parts[1])],
                label: None
            }))
        },
        x if op2.contains(&x) => {
            Ok(Instruction::Operation(Operation {
                operator: (String::from(parts[0])),
                parameters: vec![parse_parameter(parts[1]), parse_parameter(parts[2])],
                label: None
            }))
        },
        x if op3.contains(&x) => {
            Ok(Instruction::Operation(Operation {
                operator: (String::from(parts[0])),
                parameters: vec![parse_parameter(parts[1]), parse_parameter(parts[2]), parse_parameter(parts[3])],
                label: None
            }))
        },
        x => {
            Err(format!("Unrecognised instruction: '{}'!", x))
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("You need to supply exactly one argument, the path to the iasm file!");
    }

    let contents = std::fs::read_to_string(&args[1])
        .expect(&format!("Error opening file '{}'!", args[1]));

    let lines: Vec<&str> = contents.lines().collect();
    let mut program: Program = Program::new();

    let mut prev_was_label = false;
    let mut prev_label = String::new();
    for i in 0..lines.len() {
        if lines[i].trim().len() == 0 {
            continue;
        }

        let instr = parse_line(lines[i]); 
        match instr {
            Ok(Instruction::Label(label)) => {
                prev_was_label = true;
                prev_label = label;
            },
            Ok(Instruction::Operation(op)) => {
                if prev_was_label {
                    let labelled_op = Operation {
                        label: Some(prev_label.clone()),
                        ..op
                    };
                    prev_was_label = false;
                    program.operations.push(labelled_op);
                } else {
                    program.operations.push(op);
                }
            },
            Err(err) => {
                println!("{}", err);
                break;
            }
        }
    }

    println!("{:?}", program);
}
