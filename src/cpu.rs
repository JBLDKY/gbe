use crate::cpu::Instruction::{ADC, ADD, ADDHL, AND, CP, DEC, INC, OR, SBC, SUB};
use crate::registers::Registers;

// TODO fix linting issues
#[allow(dead_code)]
enum Instruction {
    ADD(Arithmetic8BitTarget),
    ADC(Arithmetic8BitTarget),
    ADDHL(Arithmetic16BitTarget),
    SUB(Arithmetic8BitTarget),
    SBC(Arithmetic8BitTarget),
    AND(Arithmetic8BitTarget),
    OR(Arithmetic8BitTarget),
    CP(Arithmetic8BitTarget),
    INC(Arithmetic8BitTarget),
    DEC(Arithmetic8BitTarget),
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
            ADC(target) => self.add_with_carry(target),
            ADDHL(target) => self.add_hl(target),
            SUB(target) => self.subtract(target),
            SBC(target) => self.subtract_with_carry(target),
            AND(target) => self.and(target),
            OR(target) => self.or(target),
            CP(target) => self.compare(target),
            INC(target) => self.increment(target),
            DEC(target) => self.decrement(target),
        };
    }

    #[inline(always)]
    fn modify_8bit_register<F>(&mut self, target: Arithmetic8BitTarget, modifier: F) -> u8
    where
        F: FnOnce(u8) -> u8,
    {
        let value = self.read_8bit_register(&target);
        let new_value = modifier(value);

        match target {
            Arithmetic8BitTarget::A => self.registers.a = new_value,
            Arithmetic8BitTarget::B => self.registers.b = new_value,
            Arithmetic8BitTarget::C => self.registers.c = new_value,
            Arithmetic8BitTarget::D => self.registers.d = new_value,
            Arithmetic8BitTarget::E => self.registers.e = new_value,
            Arithmetic8BitTarget::H => self.registers.h = new_value,
            Arithmetic8BitTarget::L => self.registers.l = new_value,
            _ => {
                panic!(
                    "Incrementing invalid register, probably F or D8 with value: {}",
                    value
                )
            }
        }
        new_value
    }

    #[inline(always)]
    fn decrement(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement for D8, HL, DE, BC, SP,
        let original_value = self.read_8bit_register(&target);
        let new_value = self.modify_8bit_register(target, |value| value.wrapping_sub(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (original_value & 0x0F) == 0;
        // self.registers.f.carry
    }

    #[inline(always)]
    fn increment(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement for D8, HL, DE, BC, SP,
        let new_value = self.modify_8bit_register(target, |value| value.wrapping_add(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (new_value & 0xf) == 0x1;
        // self.registers.f.carry
    }

    /// Doesn't set anything other than flags.
    #[inline(always)]
    fn compare(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement OR for HL
        let new_value = self.registers.a ^ self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn xor(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement OR for HL
        let new_value = self.registers.a ^ self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn or(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement OR for HL
        let new_value = self.registers.a | self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn and(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement ADD for HL
        let new_value = self.registers.a & self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn subtract_with_carry(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement SBC for HL
        let value = self.read_8bit_register(&target);
        let (new_value, overflow) = self
            .registers
            .a
            .overflowing_sub(value + self.registers.f.carry as u8);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry =
            (self.registers.a & 0xf) < ((value & 0xf) + self.registers.f.carry as u8);
        self.registers.f.carry = overflow;

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn subtract(&mut self, target: Arithmetic8BitTarget) {
        // TODO implement SUB for HL
        let value = self.read_8bit_register(&target);
        let (new_value, overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (self.registers.a & 0xf) < (value & 0xf);
        self.registers.f.carry = overflow;

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn add_with_carry(&mut self, target: Arithmetic8BitTarget) {
        // TODO: Implement ADC for HL
        let value = self.read_8bit_register(&target);
        let (new_value, overflow) = self
            .registers
            .a
            .overflowing_add(value + self.registers.f.carry as u8);

        self.registers.f.zero = new_value == 0;
        self.registers.f.carry = overflow;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn add(&mut self, target: Arithmetic8BitTarget) {
        // TODO: Implement ADD for SP, i8
        // TODO: Implement ADD for HL

        // read a value from the register
        let value = self.read_8bit_register(&target);

        let new_value = self.registers.a.wrapping_add(value);

        // Remember to set the flags
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;

        // The half-carry flag is checked during addition to see if there's a carry from the 4th bit to the 5th bit.
        // It is used for BCD (Binary Coded Decimal) operations. The half-carry is set if the addition
        // of the lower nibbles (4 bits) of the accumulator and the value is greater than 0xf, indicating
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
