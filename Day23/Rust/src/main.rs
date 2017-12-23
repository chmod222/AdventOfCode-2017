use std::io::{self, BufRead};
use std::collections::HashMap;

type Word = i64;
type Register = char;

#[derive(Copy, Clone, Debug)]
enum Operand {
    Const(Word),
    Register(Register)
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Set(Register, Operand),
    Sub(Register, Operand),
    Mul(Register, Operand),
    Jnz(Operand, Operand)
}

fn parse_operand(input: &str) -> Option<Operand> {
    match input.parse() {
        Err(_) => Some(Operand::Register(parse_register(input)?)),
        Ok(n) => Some(Operand::Const(n))
    }
}

fn parse_register(input: &str) -> Option<Register> {
    input.parse().ok()
}

impl Instruction {
    fn parse(input: &str) -> Option<Instruction> {
        let mut words = input.split(' ');

        let mnem = words.next()?;

        Some(match mnem {
            "set" => Instruction::Set(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "sub" => Instruction::Sub(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "mul" => Instruction::Mul(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "jnz" => Instruction::Jnz(parse_operand(words.next()?)?, parse_operand(words.next()?)?),

            _ => return None
        })
    }
}

struct CPU {
    regs: HashMap<char, Word>
}

impl CPU {
    fn new() -> CPU {
        let regs = HashMap::new();

        CPU::new_with_regs(regs)
    }

    fn new_with_regs(regs: HashMap<char, Word>)  -> CPU {
        CPU { regs }
    }

    fn run_program(&mut self, prog: &[Instruction]) {
        let mut pc: isize = 0;
        let mut mul_counter = 0;

        while pc >= 0 && (pc as usize) < prog.len() {
            match &prog[pc as usize] {
                &Instruction::Set(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg = v;
                },

                &Instruction::Sub(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg -= v;
                },

                &Instruction::Mul(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg *= v;

                    mul_counter += 1;
                },

                &Instruction::Jnz(ref op1, ref op2) => {
                    let v = operand_value(&op1, &mut self.regs);
                    let offset = operand_value(&op2, &mut self.regs);

                    if v != 0 {
                        pc += offset as isize;
                        continue;
                    }
                }
            }

            pc += 1;
        }

        println!("Muls: {}", mul_counter);
    }
}

fn operand_value(op: &Operand, registers: &HashMap<char, Word>) -> Word {
    match op {
        &Operand::Const(ref n) => *n,
        &Operand::Register(ref r) => *registers.get(r).unwrap_or(&0)
    }
}

fn run_program() {
}

fn main() {
    let stdin = io::stdin();
    let instrs = stdin
        .lock()
        .lines()
        .filter_map(|c| c.ok().and_then(|l| Instruction::parse(&l)))
        .collect::<Vec<Instruction>>();

    let mut cpu = CPU::new();

    cpu.run_program(&instrs);
}
