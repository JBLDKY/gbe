use crate::instruction::AnyTarget;
use crate::instruction::Arithmetic16BitTarget;
use crate::instruction::Arithmetic8BitTarget;
use crate::instruction::Instruction;
use crate::instruction::Instruction::{
    AdcHli, AddHli, AndHli, CpHli, JpHli, OrHli, SbcHli, SubHli, XorHli, ADC, ADD, ADDHL, ADDSPN,
    AND, BIT, CALL, CCF, CP, CPL, DAA, DEC, DI, EI, HALT, INC, JP, JR, LD, NOP, OR, POP, PUSH,
    RESET, RET, RETI, RL, RLA, RLC, RLCA, RR, RRA, RRC, RRCA, RST, SBC, SCF, SET, SLA, SRA, SRL,
    SUB, SWAP, XOR,
};
use crate::instruction::JumpCondition;
use crate::instruction::LoadTarget;
use crate::instruction::LoadVariant;
use crate::instruction::PrefixTarget;
use crate::instruction::StackTarget;
use crate::registers::Registers;

/// Memory banks:
///
/// Interrupt register  - 0xFFFF
/// High RAM            - 0xFF80    - 0xFFFE
/// Restricted          - 0xFF4C    - 0xFF7F
/// I/O                 - 0xFF00    - 0xFF4B
/// Restricted          - 0xFEA0    - 0xFEFF
/// Sprite Attributes   - 0xFE00    - 0xFE9F
/// Restricted          - 0xE000    - 0xFDFF
/// Internal RAM        - 0xC000    - 0xDFFF
/// Switchable RAM Bank - 0xA000    - 0xBFFF
/// Video RAM           - 0x8000    - 0x9FFF
/// Switchable ROM Bank - 0x4000    - 0x7FFF
/// ROM                 - 0x0000    - 0x3FFF

#[allow(dead_code)]
struct Mem {
    address: [u8; 0xFFFF],
}

#[allow(dead_code)]
impl Mem {
    fn read(&self, hex: u16) -> u8 {
        self.address[hex as usize]
    }

    fn modify<F>(&mut self, hex: u16, modifier: F)
    where
        F: FnOnce(u8) -> u8,
    {
        let value = self.read(hex);
        let new_value = modifier(value);
        self.write(hex, new_value);
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

    fn exec(&mut self, instruction: Instruction) {
        match instruction {
            ADD(target) => self.add(target),
            AddHli => self.add_hli(),
            ADDSPN => self.add_sp_n(),
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
            OrHli => self.or_hli(),
            XOR(target) => self.xor(target),
            XorHli => self.xor_hli(),
            CP(target) => self.compare(target),
            CpHli => self.compare_hli(),
            INC(target) => self.match_increment_target(target),
            DEC(target) => self.match_decrement_target(target),
            CCF => self.set_unset(), // toggle carry flag
            SCF => self.set_carry(), // set carry flag to 1
            RRA => self.rra(),       // rotate right & carry register A
            RLA => self.rla(),       // rotate left & carry register A
            RRCA => self.rrca(),     // rotate right without carry register A
            RLCA => self.rlca(),     // rotate left without carry register A
            CPL => self.cpl(),       // complement a (toggle all)
            BIT(target, idx) => self.bit(target, idx.into()), // check if bit is set
            RESET(target, idx) => self.reset(target, idx.into()), // set bit to 0
            SET(target, idx) => self.set(target, idx.into()), // set bit to 1
            SRL(target) => self.srl(target), // shift right logical
            RR(target) => self.rr(target), // rotate right & carry
            RL(target) => self.rl(target), // rotate left & carry
            RRC(target) => self.rrc(target), // rotate right without carry
            RLC(target) => self.rlc(target), // rotate left without carry
            SRA(target) => self.sra(target), // shift right arithmetically
            SLA(target) => self.sla(target), // shift left arithmetically
            SWAP(target) => self.swap(target), // swap upper & lower nibble
            DAA => self.daa(),       // does some weird shit
            JP(condition) => self.jump(condition), // Jump to
            JR(condition) => self.jump_relative(condition), // jump relative from PC
            JpHli => self.jump_hli(), // jump to address in HL
            // Move this shit
            PUSH(target) => {
                let value = match target {
                    StackTarget::AF => self.registers.get_af(),
                    StackTarget::BC => self.registers.get_bc(),
                    StackTarget::DE => self.registers.get_de(),
                    StackTarget::HL => self.registers.get_hl(),
                };

                self.push(value);
            } // Push onto stack one of the 16 bit registers
            POP(target) => {
                let popped = self.pop();
                match target {
                    StackTarget::AF => self.registers.set_af(popped),
                    StackTarget::BC => self.registers.set_bc(popped),
                    StackTarget::DE => self.registers.set_de(popped),
                    StackTarget::HL => self.registers.set_hl(popped),
                }
            }
            CALL(condition) => self.call(condition), // jump relative from PC
            RET(condition) => self.r#return(condition), // Pop the top of the stack basically
            RETI => self.return_from_interrupt(),    // Return from interrupt
            RST(location) => {
                self.rst();
                // need to return this or somehow deal with the stack
                location.to_hex();
            }
            NOP => {
                (self.pc.wrapping_add(1), 4);
            }
            HALT => {
                // self.is_halted = true;
                (self.pc.wrapping_add(1), 4);
            }
            DI => {
                // self.interrupts_enabled = false;
                (self.pc.wrapping_add(1), 4);
            }

            EI => {
                // self.interrupts_enabled = true;
                (self.pc.wrapping_add(1), 4);
            }
            LD(variant) => match variant {
                _ => unimplemented!(),
            },
        }
    }

    #[inline(always)]
    fn rst(&mut self) {
        self.push(self.pc.wrapping_add(1));
    }

    #[inline(always)]
    fn return_from_interrupt(&mut self) {
        // probably return
        self.pop();
    }

    #[inline(always)]
    fn r#return(&mut self, condition: JumpCondition) {
        match condition {
            JumpCondition::Zero if !self.registers.f.zero => {
                let _ = self.pc.wrapping_add(1);
                return;
            }
            JumpCondition::Carry if !self.registers.f.carry => {
                let _ = self.pc.wrapping_add(1);
                return;
            }
            JumpCondition::NotZero if self.registers.f.zero => {
                let _ = self.pc.wrapping_add(1);
                return;
            }
            JumpCondition::NotCarry if self.registers.f.carry => {
                let _ = self.pc.wrapping_add(1);
                return;
            }
            _ => {}
        }
        self.pop();
    }

    #[inline(always)]
    fn push(&mut self, value: u16) {
        // Higher byte is written first
        self.sp = self.sp.wrapping_sub(1);
        self.mem.write(self.sp, ((value & 0xFF00) >> 8) as u8);

        // Followed by the lower byte
        self.sp = self.sp.wrapping_sub(1);
        self.mem.write(self.sp, (value & 0xFF) as u8);
        // Return value?
    }

    #[inline(always)]
    fn call(&mut self, condition: JumpCondition) {
        let next_position = self.pc.wrapping_add(3);

        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return,
            JumpCondition::Carry if !self.registers.f.carry => return,
            JumpCondition::NotZero if self.registers.f.zero => return,
            JumpCondition::NotCarry if self.registers.f.carry => return,
            _ => {}
        }

        self.push(next_position);
        // maybe return instead
        self.pc = self.next_nn();
    }

    #[inline(always)]
    fn pop(&mut self) -> u16 {
        // Might need to swap lower and upper
        let upper = self.mem.read(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        let lower = self.mem.read(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        (upper << 8) | lower
    }

    fn jump_hli(&self) {
        // Just return this
        self.registers.get_hl();
    }

    /// Jump to the address that is N removed from pc.
    fn jump_relative(&mut self, condition: JumpCondition) {
        // Advance 2 bytes:
        // Byte 1: Jump instruction
        // Byte 2: Relative jump distance
        let new_position = self.pc.wrapping_add(2);

        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return,
            JumpCondition::Carry if !self.registers.f.carry => return,
            JumpCondition::NotZero if self.registers.f.zero => return,
            JumpCondition::NotCarry if self.registers.f.carry => return,
            _ => {}
        }

        let offset = self.next() as i8;
        // Probably needs returning someday
        let _pc = if offset >= 0 {
            new_position.wrapping_add(offset as u16)
        } else {
            new_position.wrapping_sub(offset.abs() as u16)
        };
    }

    /// Jump to the address that is stored in the next 16 bits in memory. See self.next_nn()
    fn jump(&mut self, condition: JumpCondition) {
        // need to return next bytes basically
        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return,
            JumpCondition::Carry if !self.registers.f.carry => return,
            JumpCondition::NotZero if self.registers.f.zero => return,
            JumpCondition::NotCarry if self.registers.f.carry => return,
            _ => {}
        }

        self.next_nn();
    }

    /// Source: http://z80-heaven.wikidot.com/instructions-set:daa
    /// When this instruction is executed, the A register is BCD corrected using the contents of the flags. The exact process is the following:
    /// if the least significant four bits of A contain a non-BCD digit (i. e. it is greater than 9)
    /// or the H flag is set, then $06 is added to the register.
    ///
    /// Then the four most significant bits are checked.
    /// If this more significant digit also happens to be greater than 9 or the C flag is set, then $60 is added.
    fn daa(&mut self) {
        // TODO test
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);

        if value & 0xF > 0b1001 || self.registers.f.half_carry == true {
            self.modify_8bit_register(Arithmetic8BitTarget::A, |value| value + 0b0110);
        }

        if value >> 4 > 0b1001 {
            self.modify_8bit_register(Arithmetic8BitTarget::A, |value| value + 0b0110_0000);
        }
    }

    fn add_sp_n(&mut self) {
        // TODO: Verify these casts to be working as inteded
        let value = self.next() as i8 as i16;
        let new_value = (self.sp as i16).wrapping_add(value) as u16;
        let sp = self.sp as i16;

        self.registers.f.zero = false;
        self.registers.f.subtract = false;
        // Edge case explaination https://stackoverflow.com/questions/57958631/game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8
        self.registers.f.half_carry = ((sp & 0xF) + (value & 0xF)) > 0x10;
        self.registers.f.carry = ((sp & 0xFF) + (value & 0xFF)) > 0x100;

        self.sp = new_value as u16;
    }

    #[inline(always)]
    fn next_nn(&mut self) -> u16 {
        let n1 = self.mem.read(self.pc + 1);

        let n2 = self.mem.read(self.pc + 2);

        (n2 << 8) as u16 | n1 as u16
    }

    #[inline(always)]
    fn next(&self) -> u8 {
        self.mem.read(self.pc + 1)
    }

    #[inline(always)]
    fn compare_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a ^ value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn xor_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a ^ value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn or_hli(&mut self) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a | value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
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
    fn swap(&mut self, target: PrefixTarget) {
        // Swap bits 0-3 with 4-7
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };
        let upper = value & 0b1111;
        let lower = value >> 4;
        let new_value = (upper << 4) | lower;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn sra(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };
        let new_value = (value as i8) >> 1;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x01) != 0;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value as u8);
            }
            _ => self.write_prefix_target_register(target, new_value as u8),
        };
    }

    #[inline(always)]
    fn sla(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };
        let new_value = value << 1;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x80) != 0;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn rrc(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };
        let right_bit = 0b1 & value;

        let new_value = value.rotate_right(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = right_bit != 0;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn rlc(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

        let left_bit = 0b1000_0000 & value;

        let new_value = value.rotate_left(1);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_bit != 0;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn rl(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

        let left_bit = 0b1000_0000 & value;
        let mut new_value = value.rotate_left(1);
        if self.registers.f.carry == true {
            new_value |= 0b0000_0001
        }

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_bit != 0;

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn rr(&mut self, target: PrefixTarget) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
        let mut new_value = value.rotate_right(1); // rotate everything right
        if self.registers.f.carry == true {
            new_value |= 0b1000_0000 // add the carry flag to the 7th bit
        }

        self.registers.f.zero = new_value == 0; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
            }
            _ => self.write_prefix_target_register(target, new_value),
        };
    }

    #[inline(always)]
    fn srl(&mut self, target: PrefixTarget) {
        let (new_value, value) = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value >> 1;
                self.mem.write(address, new_value);
                (new_value, value)
            }
            _ => {
                let value = self.read_prefix_target_register(&target);
                let new_value = value >> 1;
                self.modify_prefix_target_register(target, |_| new_value);
                (new_value, value)
            }
        };

        // Set flags
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x01) != 0;
    }

    #[inline(always)]
    fn set(&mut self, target: PrefixTarget, idx: u8) {
        assert!(idx < 8);
        let mask = 1 << idx;

        match target {
            PrefixTarget::HLI => {
                self.mem
                    .modify(self.registers.get_hl(), |value| value | mask);
            }
            _ => {
                self.modify_prefix_target_register(target, |value| value | mask);
            }
        };
    }

    #[inline(always)]
    fn reset(&mut self, target: PrefixTarget, idx: u8) {
        assert!(idx < 8);

        let mask = !(1 << idx);

        match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value ^ mask;
                self.mem.write(address, new_value);
            }
            _ => {
                self.modify_prefix_target_register(target, |value| value ^ mask);
            }
        };
    }

    #[inline(always)]
    fn bit(&mut self, target: PrefixTarget, idx: u8) {
        assert!(idx < 8);

        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

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
    fn modify_16bit_register<F>(&mut self, target: Arithmetic16BitTarget, modifier: F) -> u16
    where
        F: FnOnce(u16) -> u16,
    {
        let value = self.read_16bit_register(target);
        let new_value = modifier(value);

        match target {
            Arithmetic16BitTarget::HL => self.registers.set_hl(value),
            Arithmetic16BitTarget::BC => self.registers.set_bc(value),
            Arithmetic16BitTarget::DE => self.registers.set_de(value),
            Arithmetic16BitTarget::SP => self.sp = value,
        }
        new_value
    }

    #[inline(always)]
    fn write_prefix_target_register(&mut self, target: PrefixTarget, new_value: u8) {
        match target {
            PrefixTarget::A => self.registers.a = new_value,
            PrefixTarget::B => self.registers.b = new_value,
            PrefixTarget::C => self.registers.c = new_value,
            PrefixTarget::D => self.registers.d = new_value,
            PrefixTarget::E => self.registers.e = new_value,
            PrefixTarget::H => self.registers.h = new_value,
            PrefixTarget::L => self.registers.l = new_value,
            _ => {}
        }
    }

    #[inline(always)]
    fn modify_prefix_target_register<F>(&mut self, target: PrefixTarget, modifier: F) -> u8
    where
        F: FnOnce(u8) -> u8,
    {
        let value = self.read_prefix_target_register(&target);
        let new_value = modifier(value);

        match target {
            PrefixTarget::A => self.registers.a = new_value,
            PrefixTarget::B => self.registers.b = new_value,
            PrefixTarget::C => self.registers.c = new_value,
            PrefixTarget::D => self.registers.d = new_value,
            PrefixTarget::E => self.registers.e = new_value,
            PrefixTarget::H => self.registers.h = new_value,
            PrefixTarget::L => self.registers.l = new_value,
            _ => {
                panic!(
                    "Incrementing invalid register, probably HL with value: {}",
                    value
                )
            }
        }
        new_value
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
    fn match_decrement_target(&mut self, target: AnyTarget) {
        match target {
            AnyTarget::A => self.decrement_8bit(Arithmetic8BitTarget::A),
            AnyTarget::B => self.decrement_8bit(Arithmetic8BitTarget::B),
            AnyTarget::D => self.decrement_8bit(Arithmetic8BitTarget::D),
            AnyTarget::H => self.decrement_8bit(Arithmetic8BitTarget::H),
            AnyTarget::C => self.decrement_8bit(Arithmetic8BitTarget::C),
            AnyTarget::E => self.decrement_8bit(Arithmetic8BitTarget::E),
            AnyTarget::L => self.decrement_8bit(Arithmetic8BitTarget::L),
            AnyTarget::BC => self.decrement_16bit(Arithmetic16BitTarget::BC),
            AnyTarget::DE => self.decrement_16bit(Arithmetic16BitTarget::DE),
            AnyTarget::HL => self.decrement_16bit(Arithmetic16BitTarget::HL),
            AnyTarget::SP => self.decrement_16bit(Arithmetic16BitTarget::SP),
            AnyTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value.wrapping_sub(1);
                self.mem.write(address, new_value);
                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value & 0xf) == 0x1;
            }
        }
    }

    #[inline(always)]
    fn decrement_8bit(&mut self, target: Arithmetic8BitTarget) {
        let original_value = self.read_8bit_register(&target);
        let new_value = self.modify_8bit_register(target, |value| value.wrapping_sub(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (original_value & 0x0F) == 0;
        // self.registers.f.carry
    }

    #[inline(always)]
    fn decrement_16bit(&mut self, target: Arithmetic16BitTarget) {
        let original_value = self.read_16bit_register(target);
        let new_value = self.modify_16bit_register(target, |value| value.wrapping_sub(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (original_value & 0x0F) == 0;
        // self.registers.f.carry
    }

    #[inline(always)]
    fn match_increment_target(&mut self, target: AnyTarget) {
        match target {
            AnyTarget::A => self.increment_8bit(Arithmetic8BitTarget::A),
            AnyTarget::B => self.increment_8bit(Arithmetic8BitTarget::B),
            AnyTarget::D => self.increment_8bit(Arithmetic8BitTarget::D),
            AnyTarget::H => self.increment_8bit(Arithmetic8BitTarget::H),
            AnyTarget::C => self.increment_8bit(Arithmetic8BitTarget::C),
            AnyTarget::E => self.increment_8bit(Arithmetic8BitTarget::E),
            AnyTarget::L => self.increment_8bit(Arithmetic8BitTarget::L),
            AnyTarget::BC => self.increment_16bit(Arithmetic16BitTarget::BC),
            AnyTarget::DE => self.increment_16bit(Arithmetic16BitTarget::DE),
            AnyTarget::HL => self.increment_16bit(Arithmetic16BitTarget::HL),
            AnyTarget::SP => self.increment_16bit(Arithmetic16BitTarget::SP),
            AnyTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value.wrapping_add(1);
                self.mem.write(address, new_value);
                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value & 0xf) == 0x1;
            }
        }
    }

    #[inline(always)]
    fn increment_16bit(&mut self, target: Arithmetic16BitTarget) {
        let new_value = self.modify_16bit_register(target, |value| value.wrapping_add(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (new_value & 0xf) == 0x1;
        // self.registers.f.carry
    }

    #[inline(always)]
    fn increment_8bit(&mut self, target: Arithmetic8BitTarget) {
        let new_value = self.modify_8bit_register(target, |value| value.wrapping_add(1));

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (new_value & 0xf) == 0x1;
        // self.registers.f.carry
    }

    /// Doesn't set anything other than flags.
    #[inline(always)]
    fn compare(&mut self, target: Arithmetic8BitTarget) {
        let new_value = self.registers.a ^ self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn xor(&mut self, target: Arithmetic8BitTarget) {
        let new_value = self.registers.a ^ self.read_8bit_register(&target);
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
    }

    #[inline(always)]
    fn or(&mut self, target: Arithmetic8BitTarget) {
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
    fn read_prefix_target_register(&self, target: &PrefixTarget) -> u8 {
        match target {
            PrefixTarget::A => self.registers.a,
            PrefixTarget::B => self.registers.b,
            PrefixTarget::C => self.registers.c,
            PrefixTarget::D => self.registers.d,
            PrefixTarget::E => self.registers.e,
            PrefixTarget::H => self.registers.h,
            PrefixTarget::L => self.registers.l,
            _ => unimplemented!(),
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
            _ => unimplemented!(),
        }
    }
}
