#[derive(Clone)]
struct Cpu {
    program: Vec<u8>,
    ip: usize,
    a: u128,
    b: u128,
    c: u128,
    output: Vec<u8>,
}

impl Cpu {
    pub fn new(a: u128, b: u128, c: u128, program: &[u8]) -> Cpu {
        Cpu {
            program: program.into(),
            ip: 0,
            a,
            b,
            c,
            output: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Vec<u8> {
        while self.ip < self.program.len() {
            self.step();
        }

        self.output.clone()
    }

    fn step(&mut self) {
        match self.program[self.ip] {
            0 => self.op_adv(),
            1 => self.op_bxl(),
            2 => self.op_bst(),
            3 => self.op_jnz(),
            4 => self.op_bxc(),
            5 => self.op_out(),
            6 => self.op_bdv(),
            7 => self.op_cdv(),
            unknown_opcode => panic!(
                "Unknown op code encountered '{unknown_opcode:x}' at program location {:x}",
                self.ip
            ),
        }
    }

    fn combo_operand(&self) -> u128 {
        match self.program[self.ip + 1] {
            n @ 0..=3 => n as u128,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            unknown_combo_operator => {
                panic!("Reserved combo operator '{unknown_combo_operator:x}' found!")
            }
        }
    }

    #[inline]
    fn literal_operand(&self) -> u128 {
        self.program[self.ip + 1] as u128
    }

    fn op_adv(&mut self) {
        self.a = self.a / (2_u128.pow(self.combo_operand() as u32));
        self.ip += 2;
    }

    fn op_bxl(&mut self) {
        self.b = self.b ^ self.literal_operand();
        self.ip += 2;
    }

    fn op_bst(&mut self) {
        self.b = self.combo_operand() % 8;
        self.ip += 2;
    }

    fn op_jnz(&mut self) {
        if self.a == 0 {
            self.ip += 2
        } else {
            self.ip = self.literal_operand() as usize;
        }
    }

    fn op_bxc(&mut self) {
        self.b = self.b ^ self.c;
        self.ip += 2;
    }

    fn op_out(&mut self) {
        self.output.push((self.combo_operand() % 8_u128) as u8);
        self.ip += 2;
    }

    fn op_bdv(&mut self) {
        self.b = self.a / (2_u128.pow(self.combo_operand() as u32));
        self.ip += 2;
    }

    fn op_cdv(&mut self) {
        self.c = self.a / (2_u128.pow(self.combo_operand() as u32));
        self.ip += 2;
    }
}

// Part 1 solution: 1,6,7,4,3,0,5,0,6
fn part1() {
    let mut cpu = Cpu::new(
        63687530,
        0,
        0,
        &vec![2, 4, 1, 3, 7, 5, 0, 3, 1, 5, 4, 1, 5, 5, 3, 0],
    );
    let result = cpu.run();

    println!(
        "Result: {}",
        result
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn disassemble_op(op: &u8, operand: &u8) -> String {
    let combo = match operand {
        n @ 0..=3 => n.to_string(),
        4 => "a".to_string(),
        5 => "b".to_string(),
        6 => "c".to_string(),
        unknown_combo_operator => {
            panic!("Reserved combo operator '{unknown_combo_operator:x}' found!")
        }
    };

    match op {
        0 => format!("a = a / (2 ^ {combo})\t// adv"),
        1 => format!("b = b ^ {operand}\t\t// bxl"),
        2 => format!("b = {combo} % 8\t\t// bst"),
        3 => format!("if a != 0 then jump to {operand}\t\t// jnz"),
        4 => format!("b = b ^ c\t\t// bcx"),
        5 => format!("out {combo}\t\t// out"),
        6 => format!("b = a / (2 ^ {combo})\t// bdv"),
        7 => format!("c = a / (2 ^ {combo})\t// cdv"),
        unknown_opcode => panic!("Unknown op code encountered '{unknown_opcode:x}'"),
    }
}

fn disassemble(program: &[u8]) {
    for i in (0..program.len()).step_by(2) {
        println!("{i:02} | {}", disassemble_op(&program[i], &program[i + 1]));
    }
}

fn part2() -> u128 {
    // let input: Vec<u8> = vec![0, 3, 5, 4, 3, 0];
    let input: Vec<u8> = vec![2, 4, 1, 3, 7, 5, 0, 3, 1, 5, 4, 1, 5, 5, 3, 0];

    disassemble(&input);

    0
}

fn main() {
    print!("Part 2: {}", part2());
}
