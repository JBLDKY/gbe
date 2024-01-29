use crate::cpu::Instruction::ADD;
use crate::registers::Registers;

#[allow(dead_code)]
enum Instruction {
    ADD(Arithmetic8BitTarget),
    ADDHL(Arithmetic16BitTarget),
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
enum Arithmetic16BitTarget {
    HL,
    BC,
    SP,
    DE,
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
enum Arithmetic8BitTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    D8,
}

#[allow(dead_code)]
pub struct CPU {
    registers: Registers,
    sp: u16, // Stack Pointer
    pc: u16, // Program Counter
}

#[allow(dead_code)]
impl CPU {
    pub fn new() -> CPU {
        CPU {
            registers: Registers::new(),
            sp: 0x00,
            pc: 0x00,
        }
    }

    fn exec(&mut self, instruction: Instruction) {
        match instruction {
            ADD(target) => self.add(target),
            ADDHL(target) => self.add_hl(target),
        };
    }

    #[inline(always)]
    fn add(&mut self, target: Arithmetic8BitTarget) {
        // read a value from the register
        let value = self.read_8bit_register(&target);

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
    fn add_hl(&mut self, target: Arithmetic16BitTarget) {
        let value = self.read_16bit_register(target);
        let (new_value, overflow) = self.registers.get_hl().overflowing_add(value);

        // Don't set 0 flag
        self.registers.f.carry = overflow;
        self.registers.f.subtract = false;

        self.registers.f.half_carry =
            ((self.registers.get_hl() & 0xfff) + (value & 0xfff)) & 0x100 == 0x100;

        self.registers.set_hl(new_value);
    }

    #[inline(always)]
    fn read_16bit_register(&mut self, target: Arithmetic16BitTarget) -> u16 {
        match target {
            Arithmetic16BitTarget::HL => self.registers.get_hl(),
            Arithmetic16BitTarget::BC => self.registers.get_bc(),
            Arithmetic16BitTarget::DE => self.registers.get_de(),
            Arithmetic16BitTarget::SP => self.sp,
        }
    }

    #[inline(always)]
    fn read_8bit_register(&self, target: &Arithmetic8BitTarget) -> u8 {
        match target {
            Arithmetic8BitTarget::A => self.registers.a,
            Arithmetic8BitTarget::B => self.registers.b,
            Arithmetic8BitTarget::C => self.registers.c,
            Arithmetic8BitTarget::D => self.registers.d,
            Arithmetic8BitTarget::E => self.registers.e,
            Arithmetic8BitTarget::F => self.registers.f.into(),
            Arithmetic8BitTarget::H => self.registers.h,
            Arithmetic8BitTarget::L => self.registers.l,
            // ArithmeticTarget::HL => self.registers.h, What the fuck?
            // ArithmeticTarget::D8 => self.registers.l,
            _ => unimplemented!(),
        }
    }
}