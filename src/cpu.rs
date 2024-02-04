use crate::instruction::Arithmetic16BitTarget;
use crate::instruction::Arithmetic8BitTarget;
use crate::instruction::Instruction;
use crate::instruction::Instruction::{
    AdcHli, AddHli, AndHli, SbcHli, SubHli, ADC, ADD, ADDHL, AND, BIT, CCF, CP, CPL, DEC, INC, OR,
    RESET, RL, RLA, RLC, RLCA, RR, RRA, RRC, RRCA, SBC, SCF, SET, SLA, SRA, SRL, SUB, SWAP, XOR,
};
use crate::registers::Registers;

#[allow(dead_code)]
struct Mem {
    address: [u8; 0xFFFF],
}

#[allow(dead_code)]
impl Mem {
    fn read(&self, hex: u16) -> u8 {
        self.address[hex as usize]
    }

    fn write(&mut self, hex: u16, value: u8) {
        self.address[hex as usize] = value;
    }
}

#[allow(dead_code)]
pub struct CPU {
    registers: Registers,
    mem: Mem,
    sp: u16, // Stack Pointer
    pc: u16, // Program Counter
}

#[allow(dead_code)]
impl CPU {
    pub fn new() -> CPU {
        CPU {
            registers: Registers::new(),
            mem: Mem {
                address: [0x00; 0xFFFF],
            },
            sp: 0x00,
            pc: 0x00,
        }
    }

    // fn step(&mut self) {
    //     let mut instruction_byte = self.bus.read_byte(self.pc);
    // }

    fn exec(&mut self, instruction: Instruction) {
        match instruction {
            ADD(target) => self.add(target),
            AddHli => self.add_hli(),
            ADC(target) => self.add_with_carry(target),
            AdcHli => self.add_with_carry_hli(),
            ADDHL(target) => self.add_hl(target),
            SUB(target) => self.subtract(target),
            SubHli => self.subtract_hli(),
            SBC(target) => self.subtract_with_carry(target),
            SbcHli => self.subtract_with_carry_hli(),
            AND(target) => self.and(target),
            AndHli => self.and_hli(),
            OR(target) => self.or(target),
            XOR(target) => self.xor(target),
            CP(target) => self.compare(target),
            INC(target) => self.increment(target),
            DEC(target) => self.decrement(target),
            CCF(_) => self.set_unset(), // toggle carry flag
            SCF(_) => self.set_carry(), // set carry flag to 1
            RRA => self.rra(),          // rotate right & carry register A
            RLA => self.rla(),          // rotate left & carry register A
            RRCA => self.rrca(),        // rotate right without carry register A
            RLCA => self.rlca(),        // rotate left without carry register A
            CPL => self.cpl(),          // complement a (toggle all)
            BIT(target, idx) => self.bit(target, idx), // check if bit is set
            RESET(target, idx) => self.reset(target, idx), // set bit to 0
            SET(target, idx) => self.set(target, idx), // set bit to 1
            SRL(target) => self.srl(target), // shift right logical
            RR(target) => self.rr(target), // rotate right & carry
            RL(target) => self.rl(target), // rotate left & carry
            RRC(target) => self.rrc(target), // rotate right without carry
            RLC(target) => self.rlc(target), // rotate left without carry
            SRA(target) => self.sra(target), // shift right arithmetically
            SLA(target) => self.sla(target), // shift left arithmetically
            SWAP(target) => self.swap(target), // swap upper & lower nibble
        };
    }

    #[inline(always)]
    fn and_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a & value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn subtract_with_carry_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
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
    fn subtract_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let (new_value, overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (self.registers.a & 0xf) < (value & 0xf);
        self.registers.f.carry = overflow;

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn add_with_carry_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
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
    fn add_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a.wrapping_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;
        self.registers.a = new_value;
    }

    #[inline(always)]
    fn swap(&mut self, target: Arithmetic8BitTarget) {
        // Swap bits 0-3 with 4-7
        let value = self.read_8bit_register(&target);
        let upper = value & 0b1111;
        let lower = value >> 4;
        let new_value = (upper << 4) | lower;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn sra(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let new_value = (value as i8) >> 1;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x01) != 0;

        self.modify_8bit_register(target, |_| new_value as u8);
    }

    #[inline(always)]
    fn sla(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let new_value = value << 1;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x80) != 0;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn rrc(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let right_bit = 0b1 & value;

        let new_value = value.rotate_right(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = right_bit != 0;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn rlc(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let left_bit = 0b1000_0000 & value;

        let new_value = value.rotate_left(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_bit != 0;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn rl(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let left_bit = 0b1000_0000 & value;
        let mut new_value = value.rotate_left(1);
        if self.registers.f.carry == true {
            new_value |= 0b0000_0001
        }

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_bit != 0;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn rr(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
        let mut new_value = value.rotate_right(1); // rotate everything right
        if self.registers.f.carry == true {
            new_value |= 0b1000_0000 // add the carry flag to the 7th bit
        }

        self.registers.f.zero = new_value == 0; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn srl(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let new_value = value >> 1;
        self.modify_8bit_register(target, |_| new_value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        // last bit of original value == 1
        self.registers.f.carry = (value & 0x01) != 0;
    }

    #[inline(always)]
    fn set(&mut self, target: Arithmetic8BitTarget, idx: u8) {
        assert!(idx < 8);

        let value = self.read_8bit_register(&target);
        let mask = 1 << idx;
        let new_value = value | mask;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn reset(&mut self, target: Arithmetic8BitTarget, idx: u8) {
        assert!(idx < 8);

        let value = self.read_8bit_register(&target);
        let mask = !(1 << idx);
        let new_value = value ^ mask;

        self.modify_8bit_register(target, |_| new_value);
    }

    #[inline(always)]
    fn bit(&mut self, target: Arithmetic8BitTarget, idx: u8) {
        assert!(idx < 8);

        let value = self.read_8bit_register(&target);
        let mask = 1 << idx;
        let bit = value & mask;

        self.registers.f.zero = bit == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        // self.registers.f.carry N.A.
    }

    #[inline(always)]
    fn cpl(&mut self) {
        // self.registers.f.zero N.A.
        self.registers.f.subtract = true; // Something something BCD
        self.registers.f.half_carry = true; // Something something BCD
                                            // self.registers.f.carry N.A.
        self.registers.a = !self.registers.a;
    }

    #[inline(always)]
    fn rrca(&mut self) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
                                     //
        let new_value = value.rotate_right(1); // rotate everything right

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn rlca(&mut self) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let left_bit = 0b1000_0000 & value; // Save the last bit by using `and` with 0000_0001
                                            //
        let new_value = value.rotate_left(1); // rotate everything left

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = left_bit != 0; // save the left-most bit into the carry flag

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn rra(&mut self) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
        let mut new_value = value.rotate_right(1); // rotate everything right
        if self.registers.f.carry == true {
            // new_value = new_value + 128;
            new_value |= 0b1000_0000 // add the carry flag to the 7th bit
        }

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn rla(&mut self) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let left_bit = 0b1000_0000 & value;
        let mut new_value = value.rotate_left(1);
        if self.registers.f.carry == true {
            new_value |= 0b0000_0001
        }

        self.registers.f.zero = false;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_bit != 0;

        self.registers.a = new_value;
    }

    #[inline(always)]
    fn set_carry(&mut self) {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = true;
    }

    #[inline(always)]
    fn set_unset(&mut self) {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = !self.registers.f.carry;
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
        let value = self.read_8bit_register(&target);

        let new_value = self.registers.a.wrapping_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;

        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;

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
