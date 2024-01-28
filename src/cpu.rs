use crate::cpu::Instruction::ADD;
use crate::registers::Registers;

#[allow(dead_code)]
enum Instruction {
    ADD(ArithmeticTarget),
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
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

#[allow(dead_code)]
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

    #[inline(always)]
    fn add(&mut self, target: ArithmeticTarget) {
        // read a value from the register
        let value = self.read_register(&target);

        let (new_value, overflow) = self.registers.a.overflowing_add(value);

        // Remember to set the flags
        // TODO: Figure out what half_carry actually means
        self.registers.f.zero = new_value == 0;
        self.registers.f.carry = overflow;
        self.registers.f.subtract = false;

        // The half-carry flag is checked during addition to see if there's a carry from the 4th bit to the 5th bit.
        // It is used for BCD (Binary Coded Decimal) operations. The half-carry is set if the addition
        // of the lower nibbles (4 bits) of the accumulator and the value is greater than 0xF, indicating
        // an overflow from the lower nibble to the upper nibble. We use `0x10` to check the 5th bit because
        // if this bit is set, it indicates the lower nibble overflowed after addition.
        // Example: If the lower nibble of `a` is `0b1111` (0xF) and we add `0b0001` (1) to it,
        // the result is `0b1 0000` (0x10), which sets the 5th bit, indicating a half-carry.
        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;

        // Update register `a`
        // I guess this function will only handle adding for register `a`
        // SP, DE, R8, BC might need to be handled elsewhere
        self.registers.a = new_value;
    }

    #[inline(always)]
    fn read_register(&self, target: &ArithmeticTarget) -> u8 {
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
