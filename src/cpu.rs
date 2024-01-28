use crate::cpu::Instruction::ADD;
use crate::registers::Registers;

enum Instruction {
    ADD(ArithmeticTarget),
}

enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    HL,
    D8,
}

#[allow(dead_code)]
pub struct CPU {
    registers: Registers,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            registers: Registers::new(),
        }
    }

    fn exec(&mut self, instruction: Instruction) {
        match instruction {
            ADD(target) => self.add(target),
        };
    }

    fn add(&mut self, target: ArithmeticTarget) -> u8 {
        let (new, overflow) = match target {
            ArithmeticTarget::A => self.registers.a.overflowing_add(self.registers.a),
            ArithmeticTarget::B => self.registers.b.overflowing_add(self.registers.b),
            ArithmeticTarget::C => self.registers.c.overflowing_add(self.registers.c),
            ArithmeticTarget::D => self.registers.d.overflowing_add(self.registers.d),
            ArithmeticTarget::E => self.registers.e.overflowing_add(self.registers.e),
            ArithmeticTarget::F => {
                Into::<u8>::into(self.registers.f).overflowing_add(self.registers.f.into())
            }
            ArithmeticTarget::H => self.registers.h.overflowing_add(self.registers.h),
            ArithmeticTarget::L => self.registers.l.overflowing_add(self.registers.l),
            _ => unimplemented!(),
            // ArithmeticTarget::HL => self.registers.HL.overflowing_add(self.registers.hl),
            // ArithmeticTarget::D8 => self.registers.D8.overflowing_add(self.registers.d8),
        };
        self.registers.f.zero = overflow;
        new
    }

    fn read_register(&self, target: ArithmeticTarget) -> u8 {
        match target {
            ArithmeticTarget::A => self.registers.a,
            ArithmeticTarget::B => self.registers.b,
            ArithmeticTarget::C => self.registers.c,
            ArithmeticTarget::D => self.registers.d,
            ArithmeticTarget::E => self.registers.e,
            ArithmeticTarget::F => self.registers.f.into(),
            ArithmeticTarget::H => self.registers.h,
            ArithmeticTarget::L => self.registers.l,
            // ArithmeticTarget::HL => self.registers.h, What the fuck?
            // ArithmeticTarget::D8 => self.registers.l,
            _ => unimplemented!(),
        }
    }
}
