use std::io::{self, BufRead};
use std::collections::HashMap;
use std::thread;
use std::sync::mpsc::{Sender, Receiver, channel};
use std::sync::Arc;
use std::time::Duration;

type Word = i64;
type Register = char;

#[derive(Debug)]
enum Operand {
    Const(Word),
    Register(Register)
}

#[derive(Debug)]
enum Instruction {
    Snd(Operand),
    Set(Register, Operand),
    Add(Register, Operand),
    Mul(Register, Operand),
    Mod(Register, Operand),
    Rcv(Register),
    Jgz(Operand, Operand)
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
            "snd" => Instruction::Snd(parse_operand(words.next()?)?),
            "set" => Instruction::Set(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "add" => Instruction::Add(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "mul" => Instruction::Mul(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "mod" => Instruction::Mod(parse_register(words.next()?)?, parse_operand(words.next()?)?),
            "rcv" => Instruction::Rcv(parse_register(words.next()?)?),
            "jgz" => Instruction::Jgz(parse_operand(words.next()?)?, parse_operand(words.next()?)?),

            _ => return None
        })
    }
}

struct CPU {
    regs: HashMap<char, Word>,
    rx: Receiver<Word>,
    tx: Sender<Word>,
    id: usize
}

impl CPU {
    fn new(rx: Receiver<Word>, tx: Sender<Word>, id: usize) -> CPU {
        let mut regs = HashMap::new();

        regs.insert('p', id as Word);

        CPU { regs, rx, tx, id }
    }

    fn run_program(&mut self, prog: Arc<Vec<Instruction>>) {
        let mut pc: isize = 0;
        let mut sndcnt = 0;

        while pc >= 0 && (pc as usize) < prog.len() {
            match &prog[pc as usize] {
                &Instruction::Snd(ref op) => {
                    let v = operand_value(&op, &self.regs);

                    self.tx.send(v).unwrap();
                    sndcnt += 1;

                    println!("Prog {}: --> {} (#{})", self.id, v, sndcnt);
                },

                &Instruction::Set(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg = v;
                },

                &Instruction::Add(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg += v;
                },

                &Instruction::Mul(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg *= v;
                },

                &Instruction::Mod(ref rd, ref op) => {
                    let v = operand_value(&op, &mut self.regs);
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    *reg %= v;
                },

                &Instruction::Rcv(ref rd) => {
                    let reg: &mut Word = self.regs.entry(*rd).or_insert(0);

                    match self.rx.recv_timeout(Duration::from_secs(3)) {
                        Ok(v) => *reg = v,

                        Err(_) => {
                            println!("Prog {} has been stuck for 3 seconds -- deadlock probable", self.id);
                            break;                                
                        }
                    }

                    //println!("Prog {}: <-- {}", self.id, *reg);
                },

                &Instruction::Jgz(ref op1, ref op2) => {
                    let v = operand_value(&op1, &mut self.regs);
                    let offset = operand_value(&op2, &mut self.regs);

                    if v > 0 {
                        pc += offset as isize;
                        continue;
                    }
                }
            }

            pc += 1;
        }
    }
}

fn operand_value(op: &Operand, registers: &HashMap<char, Word>) -> Word {
    match op {
        &Operand::Const(ref n) => *n,
        &Operand::Register(ref r) => *registers.get(r).unwrap_or(&0)
    }
}

fn main() {
    let stdin = io::stdin();
    let instrs = stdin
        .lock()
        .lines()
        .filter_map(|c| c.ok().and_then(|l| Instruction::parse(&l)))
        .collect::<Vec<Instruction>>();

    let (tx1, rx1) = channel();
    let (tx2, rx2) = channel();

    let mut cpu1 = CPU::new(rx1, tx2, 0);
    let mut cpu2 = CPU::new(rx2, tx1, 1);

    let cpy1 = Arc::new(instrs);
    let cpy2 = Arc::clone(&cpy1);

    let h1 = thread::spawn(move || cpu1.run_program(cpy1));
    let h2 = thread::spawn(move || cpu2.run_program(cpy2));

    h1.join().unwrap();
    h2.join().unwrap();
}
