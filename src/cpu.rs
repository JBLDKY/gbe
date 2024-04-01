#![allow(dead_code)]
use log::{debug, info};

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

/// Memory banks
///
/// Interrupt register  - 0xFFFF
/// High RAM            - 0xFF80    - 0xFFFE
/// Restricted          - 0xFF4C    - 0xFF7F
/// I/O                 - 0xFF00    - 0xFF7F
/// Restricted          - 0xFEA0    - 0xFEFF
/// Sprite Attributes   - 0xFE00    - 0xFE9F
/// Restricted          - 0xE000    - 0xFDFF
/// Internal RAM        - 0xC000    - 0xDFFF
/// Switchable RAM Bank - 0xA000    - 0xBFFF
/// Video RAM           - 0x8000    - 0x9FFF
/// Switchable ROM Bank - 0x4000    - 0x7FFF
/// Game ROM            - 0x0100    - 0x03FF
/// Boot ROM            - 0x0000    - 0x00FF

const INTERRUPT_REGISTER: usize = 0xFFFF;

const HIGH_RAM_START: usize = 0xFF80;
const HIGH_RAM_END: usize = 0xFFFE;
const HIGH_RAM: usize = HIGH_RAM_END - HIGH_RAM_START + 1;

// const RESTRICTED_HIGH_START: usize = 0xFF80;
// const RESTRICTED_HIGH_END: usize = 0xFFFE;
// const RESTRICTED_HIGH: usize = RESTRICTED_HIGH_END - RESTRICTED_HIGH_START + 1;

const INPUT_OUTPUT: usize = INPUT_OUTPUT_END - INPUT_OUTPUT_START + 1;
const INPUT_OUTPUT_START: usize = 0xFF00;
const INPUT_OUTPUT_END: usize = 0xFF7F;

const RESTRICTED_MID_START: usize = 0xFEA0;
const RESTRICTED_MID_END: usize = 0xFEFF;
const RESTRICTED_MID: usize = RESTRICTED_MID_END - RESTRICTED_MID_START + 1;

const SPRITE: usize = SPRITE_END - SPRITE_START + 1;
const SPRITE_START: usize = 0xFE00;
const SPRITE_END: usize = 0xFE9F;

const RESTRICTED_LOW_START: usize = 0xE000;
const RESTRICTED_LOW_END: usize = 0xFDFF;
const RESTRICTED_LOW: usize = RESTRICTED_LOW_END - RESTRICTED_LOW_START + 1;

const INTERNAL_RAM: usize = INTERNAL_RAM_END - INTERNAL_RAM_START + 1;
const INTERNAL_RAM_START: usize = 0xC000;
const INTERNAL_RAM_END: usize = 0xDFFF;

const SWITCH_RAM: usize = SWITCH_RAM_END - SWITCH_RAM_START + 1;
const SWITCH_RAM_START: usize = 0xA000;
const SWITCH_RAM_END: usize = 0xBFFF;

const VIDEO_RAM: usize = VIDEO_RAM_END - VIDEO_RAM_START + 1;
const VIDEO_RAM_START: usize = 0x8000;
const VIDEO_RAM_END: usize = 0x9FFF;

const SWITCH_ROM: usize = SWITCH_ROM_END - SWITCH_ROM_START + 1;
const SWITCH_ROM_START: usize = 0x4000;
const SWITCH_ROM_END: usize = 0x7FFF;

const GAME_ROM: usize = ROM_END - ROM_START + 1;
const GAME_ROM_START: usize = 0x0100;
const GAME_ROM_END: usize = 0x39FF;

const ROM: usize = ROM_END - ROM_START + 1;
const ROM_START: usize = 0x0000;
const ROM_END: usize = 0x00FF;

#[derive(Debug)]
pub struct Mem {
    rom: Option<[u8; ROM]>,
    switch_rom: [u8; SWITCH_ROM],
    pub vram: [u8; VIDEO_RAM],
    switch_ram: [u8; SWITCH_RAM],
    internal_ram: [u8; INTERNAL_RAM],
    restricted_low: [u8; RESTRICTED_LOW],
    sprite: [u8; SPRITE],
    restricted_mid: [u8; RESTRICTED_MID],
    input_output: [u8; INPUT_OUTPUT],
    // restricted_high: [u8; RESTRICTED_HIGH],
    high_ram: [u8; HIGH_RAM],
    interrupt: usize,
}

impl Mem {
    fn new(boot_rom: &[u8], game_rom: &[u8]) -> Mem {
        // BOOT ARRAY
        let mut boot_array = [0u8; ROM];
        let boot_length = boot_rom.len().min(ROM);

        // Load the boot rom into the boot array, no need
        // to slice our source because we know its only around 256 bits
        boot_array[..boot_length].copy_from_slice(&boot_rom[..boot_length]);

        // Moving onto the gamerom, we want to know how much of the game will fit
        // in in the rom memory. This would be the min of our game_rom and the ROM Bank size
        // after accounting for the space the boot rom takes up.
        let game_length = game_rom.len();
        let space = game_length.min(ROM - boot_length);

        // Starting at the boot_rom end index we copy over as much of the game_rom
        // as we can fit in the space partially occupied by the boot_rom.
        boot_array[boot_length..].copy_from_slice(&game_rom[..space]);

        // Initializ the switch rom array
        let mut switch_rom_array = [0u8; SWITCH_ROM];

        // Copy the rest to switch_rom_array
        let remaining_game_length = game_length - boot_length;

        if remaining_game_length > ROM {
            switch_rom_array
                .copy_from_slice(&game_rom[ROM - boot_length..((ROM - boot_length) + SWITCH_ROM)]);
        }

        Mem {
            rom: Some(boot_array),
            switch_rom: switch_rom_array,
            vram: [0; VIDEO_RAM],
            switch_ram: [0; SWITCH_RAM],
            internal_ram: [0; INTERNAL_RAM],
            restricted_low: [0; RESTRICTED_LOW],
            sprite: [0; SPRITE],
            restricted_mid: [0; RESTRICTED_MID],
            input_output: [0; INPUT_OUTPUT],
            // restricted_high: [0; RESTRICTED_HIGH],
            high_ram: [0; HIGH_RAM],
            interrupt: 0,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x00FF => {
                if let Some(rom) = self.rom {
                    rom[addr as usize]
                } else {
                    self.switch_rom[addr as usize]
                }
            }
            0x0100..=0x3FFF => self.switch_rom[addr as usize],
            0x4000..=0x7FFF => {
                let switchable_addr = addr as usize - 0x4000;
                self.switch_rom[switchable_addr % self.switch_rom.len()]
            }
            0x8000..=0x9FFF => self.vram[addr as usize - 0x8000],
            0xA000..=0xBFFF => self.switch_ram[addr as usize - 0xA000],
            0xC000..=0xDFFF => self.internal_ram[addr as usize - 0xC000],
            0xE000..=0xFDFF => self.internal_ram[addr as usize - 0xE000],
            0xFE00..=0xFE9F => self.sprite[addr as usize - 0xFE00],
            0xFEA0..=0xFEFF => 0, // restricted
            0xFF00..=0xFF7F => self.input_output[addr as usize - 0xFF00],
            // 0xFF4C..=0xFF7F => 0, // restricted
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80],
            0xFFFF => self.interrupt as u8,
        }
    }

    fn modify<F>(&mut self, hex: u16, modifier: F)
    where
        F: FnOnce(u8) -> u8,
    {
        let value = self.read(hex);
        let new_value = modifier(value);
        self.write(hex, new_value);
    }

    fn write(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x3FFF => self.switch_rom[addr as usize] = value,
            0x4000..=0x7FFF => {
                let switchable_addr = addr as usize - 0x4000;
                self.switch_rom[switchable_addr % self.switch_rom.len()] = value
            }
            0x8000..=0x9FFF => self.vram[addr as usize - 0x8000] = value,
            0xA000..=0xBFFF => self.switch_ram[addr as usize - 0xA000] = value,
            0xC000..=0xDFFF => self.internal_ram[addr as usize - 0xC000] = value,
            0xE000..=0xFDFF => self.internal_ram[addr as usize - 0xE000] = value,
            0xFE00..=0xFE9F => self.sprite[addr as usize - 0xFE00] = value,
            0xFEA0..=0xFEFF => {} // restricted
            0xFF00..=0xFF7F => self.input_output[addr as usize - 0xFF00] = value,
            0xFF80..=0xFFFE => self.high_ram[addr as usize - 0xFF80] = value,
            0xFFFF => self.interrupt = value.into(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct CPU {
    pub registers: Registers,
    pub mem: Mem,
    pub is_halted: bool,
    pub is_interrupted: bool,
    pub interrupts_enabled: bool,
    pub sp: u16, // Stack Pointer
    pub pc: u16, // Program Counter
}

#[allow(dead_code)]
impl CPU {
    pub fn new(boot_rom: &[u8], rom: &[u8]) -> CPU {
        CPU {
            registers: Registers::new(),
            mem: Mem::new(boot_rom, rom),
            is_halted: false,
            is_interrupted: true,
            interrupts_enabled: false,
            sp: 0x00,
            pc: 0x0,
        }
    }

    pub fn step(&mut self) -> u8 {
        // Next instruction
        let mut instruction_byte = self.mem.read(self.pc);
        let instruction;
        let mut extra_cycles = 0;
        let _extra_pc = 0;

        debug!(
            "A: {:02X} F: {:02X} B: {:02X} C: {:02X} D: {:02X} E: {:02X} H: {:02X} L: {:02X} SP: {:04X} PC: 00:{:04X} ({:02X} {:02X} {:02X} {:02X})",
            self.registers.a,
            u8::from(self.registers.f),  // Assuming `f` is the flag register
            self.registers.b,
            self.registers.c,
            self.registers.d,
            self.registers.e,
            self.registers.h,
            self.registers.l,
            self.sp,
            self.pc,
            self.mem.read(self.pc),
            self.mem.read(self.pc.wrapping_add(1)),
            self.mem.read(self.pc.wrapping_add(2)),
            self.mem.read(self.pc.wrapping_add(3)),
        );

        if instruction_byte == 0xcb {
            // self.pc = self.pc.wrapping_add(1);
            extra_cycles += 4;
            instruction_byte = self.mem.read(self.pc.wrapping_add(1));
            instruction = Instruction::from_byte_prefixed(instruction_byte);
        } else {
            instruction = Instruction::from_byte(instruction_byte);
        }

        // What the fuck do I do with cycles
        let (next_pc, cycles) = if let Some(instruction) = instruction {
            self.exec(instruction)
        } else {
            // pls no
            info!("\n\n Dumping processor data because of imminent panic.");
            info!("Program counter: {:#04x}", self.pc);
            info!("Stack pointer: {:#04x}", self.sp);
            info!("Registers: {:#04x?}", self.registers);
            info!("Halted: {}", self.is_halted);
            info!("Interrupted: {}", self.is_interrupted);
            info!("END DUMP \n\n");
            panic!("UNKOWN INSTRUCTION FOUND FOR: {:#04x}", instruction_byte);
        };

        if self.interrupts_enabled {
            let interrupt_enable = self.mem.read(0xFFFF);
            let interrupt_flag = self.mem.read(0xFF0F);
            let interrupt_requested = interrupt_enable & interrupt_flag;

            if interrupt_requested != 0 && interrupt_requested & 0x01 != 0 {
                // VBLANK
                self.push(self.pc);

                self.pc = 0x0040; // VBlank address, todo create a const

                let interrupt_flag = self.mem.read(0xFF0F);
                self.mem.write(0xFF0F, interrupt_flag & 0xFE);
            }
        }

        self.pc = next_pc;

        cycles + extra_cycles
    }

    fn exec(&mut self, instruction: Instruction) -> (u16, u8) {
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
                (self.pc.wrapping_add(1), 16)
            } // Push onto stack one of the 16 bit registers
            POP(target) => {
                let popped = self.pop();
                match target {
                    StackTarget::AF => self.registers.set_af(popped),
                    StackTarget::BC => self.registers.set_bc(popped),
                    StackTarget::DE => self.registers.set_de(popped),
                    StackTarget::HL => self.registers.set_hl(popped),
                }

                (self.pc.wrapping_add(1), 12)
            }
            CALL(condition) => self.call(condition), // jump relative from PC
            RET(condition) => self.r#return(condition), // Pop the top of the stack basically
            RETI => self.return_from_interrupt(),    // Return from interrupt
            RST(location) => self.rst(location.to_hex()),
            NOP => (self.pc.wrapping_add(1), 4),
            HALT => {
                self.is_halted = true;
                (self.pc.wrapping_add(1), 4)
            }
            DI => {
                self.interrupts_enabled = false;
                (self.pc.wrapping_add(1), 4)
            }

            EI => {
                self.interrupts_enabled = true;
                (self.pc.wrapping_add(1), 4)
            }
            LD(variant) => match variant {
                LoadVariant::RegToReg(destination, source) => {
                    self.load_register_into_register(destination, source);
                    (self.pc.wrapping_add(1), 4)
                }

                LoadVariant::MemIndirectToReg(destination, source) => {
                    self.load_mem_indirect_to_reg(destination, source);
                    (self.pc.wrapping_add(1), 8)
                }

                LoadVariant::MemIndirectToRegIncHL(destination, source) => {
                    self.load_mem_indirect_to_reg_inc_hl(destination, source);
                    (self.pc.wrapping_add(1), 8)
                }

                LoadVariant::MemIndirectToRegDecHL(destination, source) => {
                    self.load_mem_indirect_to_reg_dec_hl(destination, source);
                    (self.pc.wrapping_add(1), 8)
                }

                LoadVariant::RegToMemIndirect(destination, source) => {
                    let (mut pc, mut cycles) = (1, 8);
                    let address = match destination {
                        LoadTarget::HLI => self.registers.get_hl(),
                        LoadTarget::BC => self.registers.get_bc(),
                        LoadTarget::DE => self.registers.get_de(),
                        LoadTarget::C => {
                            pc = 1;
                            cycles = 8;
                            0xFF00 | (self.registers.c as u16)
                        }
                        _ => unimplemented!(),
                    };

                    let value = self.read_load_target_register(&source);
                    self.mem.write(address, value);
                    (self.pc.wrapping_add(pc), cycles)
                }

                LoadVariant::ImmToReg(destination) => {
                    let value = self.next();
                    self.write_load_target_register(&destination, value);
                    (self.pc.wrapping_add(2), 8)
                }

                LoadVariant::ImmToMemIndirect(destination) => {
                    let value = self.next();
                    self.write_load_target_register(&destination, value);
                    (self.pc.wrapping_add(2), 12)
                }

                LoadVariant::RegToMemIndirectInc(_, source) => {
                    let value = self.read_load_target_register(&source);
                    let address = self.registers.get_hl();
                    self.registers.set_hl(address.wrapping_add(1));
                    self.mem.write(address, value);
                    (self.pc.wrapping_add(1), 8)
                }

                LoadVariant::RegToMemIndirectDec => {
                    let value = self.read_load_target_register(&LoadTarget::A);
                    let address = self.registers.get_hl();
                    self.registers.set_hl(address.wrapping_sub(1));
                    self.mem.write(address, value);
                    (self.pc.wrapping_add(1), 8)
                }

                LoadVariant::MemOffsetToReg(target) => {
                    println!("{:#02x?}", self.mem.input_output);
                    panic!("");
                    let offset = self.next();
                    let address = 0xFF00 + offset as u16;
                    let value = self.mem.read(address);
                    self.write_load_target_register(&target, value);
                    (self.pc.wrapping_add(2), 12)
                }

                LoadVariant::RegToMemOffset(target) => {
                    let offset = self.next();
                    let address = 0xFF00 + offset as u16;
                    let value = self.read_load_target_register(&target);
                    self.mem.write(address, value);
                    (self.pc.wrapping_add(2), 12)
                }

                LoadVariant::MemToRegA16(target) => {
                    let lower = self.next();
                    let upper = self.next();
                    let address = ((upper as u16) << 8) | lower as u16;
                    let value = self.mem.read(address);
                    self.write_load_target_register(&target, value);
                    (self.pc.wrapping_add(3), 16)
                }

                LoadVariant::RegAToMemA16(target) => {
                    let lower = self.next();
                    let upper = self.next();
                    let address = ((upper as u16) << 8) | lower as u16;
                    let value = self.read_load_target_register(&target);
                    self.mem.write(address, value);
                    (self.pc.wrapping_add(3), 16)
                }

                LoadVariant::ImmWordToReg(target) => {
                    let value = self.next_nn();

                    match target {
                        LoadTarget::HL => self.registers.set_hl(value),
                        LoadTarget::DE => self.registers.set_de(value),
                        LoadTarget::BC => self.registers.set_bc(value),
                        LoadTarget::SP => self.sp = value,
                        _ => unimplemented!(),
                    }
                    (self.pc.wrapping_add(3), 12)
                }

                LoadVariant::StackPointerToMem => {
                    let address = self.next_nn();

                    let lower = (self.sp & 0xFF) as u8;
                    self.mem.write(address, lower);

                    let higher = (self.sp >> 8) as u8;
                    self.mem.write(address.wrapping_add(1), higher);

                    (self.pc.wrapping_add(3), 20)
                }

                _ => unimplemented!(),
            },
        }
    }

    #[inline(always)]
    fn load_reg_to_mem_indirect(&mut self, _: LoadTarget, source: LoadTarget) {
        let value = self.read_load_target_register(&source);
        let address = self.registers.get_hl();
        self.mem.write(address, value);
    }

    #[inline(always)]
    fn load_mem_indirect_to_reg_dec_hl(&mut self, _: LoadTarget, _: LoadTarget) {
        let value = self.registers.get_hl();
        self.registers.set_hl(value.wrapping_sub(1));
        self.mem.read(value);
    }

    #[inline(always)]
    fn load_mem_indirect_to_reg_inc_hl(&mut self, _: LoadTarget, _: LoadTarget) {
        let value = self.registers.get_hl();
        self.registers.set_hl(value.wrapping_add(1));
        self.mem.read(value);
    }

    #[inline(always)]
    fn load_mem_indirect_to_reg(&mut self, destination: LoadTarget, source: LoadTarget) {
        let address = self.get_address_from_load_target(&source);
        let value = self.mem.read(address);
        self.write_load_target_register(&destination, value);
    }

    #[inline(always)]
    fn get_address_from_load_target(&self, source: &LoadTarget) -> u16 {
        match source {
            LoadTarget::HLI => self.registers.get_hl(),
            LoadTarget::HL => self.registers.get_hl(),
            LoadTarget::DE => self.registers.get_de(),
            LoadTarget::BC => self.registers.get_bc(),
            LoadTarget::C => (self.mem.read(0xFF00) + self.registers.c).into(),
            _ => panic!(
                "Trying to get address for LoadTarget during load_mem_indirect_to_reg instruction"
            ),
        }
    }

    #[inline(always)]
    fn load_register_into_register(&mut self, destination: LoadTarget, source: LoadTarget) {
        let value = self.read_load_target_register(&source);
        self.write_load_target_register(&destination, value);
    }

    #[inline(always)]
    fn rst(&mut self, address: u16) -> (u16, u8) {
        self.push(self.pc.wrapping_add(1));
        (address, 16)
    }

    #[inline(always)]
    fn return_from_interrupt(&mut self) -> (u16, u8) {
        (self.pop(), 16)
    }

    #[inline(always)]
    fn r#return(&mut self, condition: JumpCondition) -> (u16, u8) {
        match condition {
            JumpCondition::Zero if !self.registers.f.zero => {
                return (self.pc.wrapping_add(1), 8);
            }
            JumpCondition::Carry if !self.registers.f.carry => {
                return (self.pc.wrapping_add(1), 8);
            }
            JumpCondition::NotZero if self.registers.f.zero => {
                return (self.pc.wrapping_add(1), 8);
            }
            JumpCondition::NotCarry if self.registers.f.carry => {
                return (self.pc.wrapping_add(1), 8);
            }
            JumpCondition::Unconditional => {
                return (self.pop(), 16); // Unconditional RET
            }
            _ => {}
        }

        (self.pop(), 20)
    }

    #[inline(always)]
    fn push(&mut self, value: u16) {
        // Higher byte is written first
        self.sp = self.sp.wrapping_sub(1);
        self.mem.write(self.sp, ((value & 0xFF00) >> 8) as u8);

        // Followed by the lower byte
        self.sp = self.sp.wrapping_sub(1);
        self.mem.write(self.sp, (value & 0xFF) as u8);
    }

    #[inline(always)]
    fn call(&mut self, condition: JumpCondition) -> (u16, u8) {
        let next_position = self.pc.wrapping_add(3);

        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return (next_position, 12),
            JumpCondition::Carry if !self.registers.f.carry => return (next_position, 12),
            JumpCondition::NotZero if self.registers.f.zero => return (next_position, 12),
            JumpCondition::NotCarry if self.registers.f.carry => return (next_position, 12),
            _ => {}
        }

        self.push(next_position);
        // maybe return instead
        (self.next_nn(), 24)
    }

    #[inline(always)]
    fn pop(&mut self) -> u16 {
        // Read the lower byte first (Game Boy is little-endian)
        let lower = self.mem.read(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        // Then read the upper byte
        let upper = self.mem.read(self.sp) as u16;
        self.sp = self.sp.wrapping_add(1);

        // Combine them into a 16-bit value with upper as the high byte and lower as the low byte
        (upper << 8) | lower
    }

    fn jump_hli(&self) -> (u16, u8) {
        // Just return this
        self.registers.get_hl();

        (self.pc.wrapping_add(1), 4)
    }

    /// Jump to the address that is N removed from pc.
    fn jump_relative(&mut self, condition: JumpCondition) -> (u16, u8) {
        // Advance 2 bytes:
        // Byte 1: Jump instruction
        // Byte 2: Relative jump distance
        let new_position = self.pc.wrapping_add(2);

        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return (self.pc.wrapping_add(2), 12),
            JumpCondition::Carry if !self.registers.f.carry => {
                return (self.pc.wrapping_add(2), 12)
            }
            JumpCondition::NotZero if self.registers.f.zero => {
                return (self.pc.wrapping_add(2), 12)
            }
            JumpCondition::NotCarry if self.registers.f.carry => {
                return (self.pc.wrapping_add(2), 12)
            }
            _ => {}
        }

        let offset = self.next() as i8;
        // Probably needs returning someday
        let pc = if offset >= 0 {
            new_position.wrapping_add(offset as u16)
        } else {
            new_position.wrapping_sub(offset.unsigned_abs() as u16)
        };

        (pc, 12)
    }

    /// Jump to the address that is stored in the next 16 bits in memory. See self.next_nn()
    fn jump(&mut self, condition: JumpCondition) -> (u16, u8) {
        // need to return next bytes basically
        match condition {
            JumpCondition::Unconditional => {}
            JumpCondition::Zero if !self.registers.f.zero => return (self.pc.wrapping_add(3), 16),
            JumpCondition::Carry if !self.registers.f.carry => {
                return (self.pc.wrapping_add(3), 16)
            }
            JumpCondition::NotZero if self.registers.f.zero => {
                return (self.pc.wrapping_add(3), 16)
            }
            JumpCondition::NotCarry if self.registers.f.carry => {
                return (self.pc.wrapping_add(3), 16)
            }
            _ => {}
        }

        self.next_nn();

        (self.pc.wrapping_add(3), 16)
    }

    /// Source: http://z80-heaven.wikidot.com/instructions-set:daa
    /// When this instruction is executed, the A register is BCD corrected using the contents of the flags. The exact process is the following:
    /// if the least significant four bits of A contain a non-BCD digit (i. e. it is greater than 9)
    /// or the H flag is set, then $06 is added to the register.
    ///
    /// Then the four most significant bits are checked.
    /// If this more significant digit also happens to be greater than 9 or the C flag is set, then $60 is added.
    fn daa(&mut self) -> (u16, u8) {
        // TODO test
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);

        if value & 0xF > 0b1001 || self.registers.f.half_carry {
            self.modify_8bit_register(Arithmetic8BitTarget::A, |value| value + 0b0110);
        }

        if value >> 4 > 0b1001 {
            self.modify_8bit_register(Arithmetic8BitTarget::A, |value| value + 0b0110_0000);
        }

        (self.pc.wrapping_add(1), 4)
    }

    fn add_sp_n(&mut self) -> (u16, u8) {
        // TODO: Verify these casts to be working as inteded
        let value = self.next() as i8 as i16;
        let new_value = (self.sp as i16).wrapping_add(value) as u16;
        let sp = self.sp as i16;

        self.registers.f.zero = false;
        self.registers.f.subtract = false;
        // Edge case explaination https://stackoverflow.com/questions/57958631/game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8
        self.registers.f.half_carry = ((sp & 0xF) + (value & 0xF)) > 0x10;
        self.registers.f.carry = ((sp & 0xFF) + (value & 0xFF)) > 0x100;

        self.sp = new_value;

        (self.pc.wrapping_add(2), 16)
    }

    #[inline(always)]
    fn next_nn(&mut self) -> u16 {
        let n1 = self.mem.read(self.pc.wrapping_add(1)) as u16;
        let n2 = self.mem.read(self.pc.wrapping_add(2)) as u16;

        (n2 << 8) | n1
    }

    #[inline(always)]
    fn next(&self) -> u8 {
        self.mem.read(self.pc.wrapping_add(1))
    }

    #[inline(always)]
    fn compare_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a ^ value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn xor_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a ^ value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn or_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a | value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn and_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a & value;

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn subtract_with_carry_hli(&mut self) -> (u16, u8) {
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

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn subtract_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let (new_value, overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (self.registers.a & 0xf) < (value & 0xf);
        self.registers.f.carry = overflow;

        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn add_with_carry_hli(&mut self) -> (u16, u8) {
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
        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn add_hli(&mut self) -> (u16, u8) {
        let address = self.registers.get_hl();
        let value = self.mem.read(address);
        let new_value = self.registers.a.wrapping_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;
        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 8)
    }

    #[inline(always)]
    fn swap(&mut self, target: PrefixTarget) -> (u16, u8) {
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
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn sra(&mut self, target: PrefixTarget) -> (u16, u8) {
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
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value as u8);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn sla(&mut self, target: PrefixTarget) -> (u16, u8) {
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
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn rrc(&mut self, target: PrefixTarget) -> (u16, u8) {
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
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);

                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn rlc(&mut self, target: PrefixTarget) -> (u16, u8) {
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
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    // #[inline(always)]
    // fn rl(&mut self, target: PrefixTarget) -> (u16, u8) {
    //     // Read value of PrefixTarget (register C)
    //     let value = self.read_prefix_target_register(&target);
    //
    //     let left_bit = 0b1000_0000 & value; // save the last bit (if 1) to shift to register F
    //     let mut new_value = value.rotate_left(1); // perform rotation
    //     if self.registers.f.carry {
    //         new_value |= 0b0000_0001
    //     }
    //
    //     self.registers.f.zero = new_value == 0; // if the resulting value is 0, set the zero flag
    //     self.registers.f.subtract = false; // Always unset subtract
    //     self.registers.f.half_carry = false; // Always unset half_carry
    //     self.registers.f.carry = left_bit != 0; // if leftmost bit is 1, set the carry flag
    //
    //     self.write_prefix_target_register(target, new_value); // update register C with the new
    //                                                           // value
    //
    //     // return the new Program Counter value & the amount of cycles this takes on the original
    //     // Z80 CPU (for accurate emulation we need to count original cycles
    //     (self.pc.wrapping_add(2), 8) // wrapping add basically allows overflow to continue from 0
    // }

    #[inline(always)]
    fn rl(&mut self, target: PrefixTarget) -> (u16, u8) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

        let left_most_bit = value & 0b1000_0000; // save the left most bit
        let mut new_value = value << 1; // shift left by 1
        new_value |= self.registers.f.carry as u8; // set the right most bit to value f_carry

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_most_bit != 0; // set f_carry to the value of the left most bit

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn rr(&mut self, target: PrefixTarget) -> (u16, u8) {
        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                self.mem.read(address)
            }
            _ => self.read_prefix_target_register(&target),
        };

        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
        let mut new_value = value.rotate_right(1); // rotate everything right
        if self.registers.f.carry {
            new_value |= 0b1000_0000 // add the carry flag to the 7th bit
        }

        self.registers.f.zero = new_value == 0; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        match target {
            PrefixTarget::HLI => {
                self.mem.write(self.registers.get_hl(), new_value);
                (self.pc.wrapping_add(2), 8)
            }
            _ => {
                self.write_prefix_target_register(target, new_value);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn srl(&mut self, target: PrefixTarget) -> (u16, u8) {
        let pc;
        let cycles;

        let (new_value, value) = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value >> 1;
                self.mem.write(address, new_value);
                (pc, cycles) = (2, 16);
                (new_value, value)
            }
            _ => {
                let value = self.read_prefix_target_register(&target);
                let new_value = value >> 1;
                self.modify_prefix_target_register(target, |_| new_value);
                (pc, cycles) = (2, 8);
                (new_value, value)
            }
        };

        // Set flags
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = (value & 0x01) != 0;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn set(&mut self, target: PrefixTarget, idx: u8) -> (u16, u8) {
        assert!(idx < 8);
        let mask = 1 << idx;

        match target {
            PrefixTarget::HLI => {
                self.mem
                    .modify(self.registers.get_hl(), |value| value | mask);

                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.modify_prefix_target_register(target, |value| value | mask);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn reset(&mut self, target: PrefixTarget, idx: u8) -> (u16, u8) {
        assert!(idx < 8);

        let mask = !(1 << idx);

        match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value ^ mask;
                self.mem.write(address, new_value);
                (self.pc.wrapping_add(2), 16)
            }
            _ => {
                self.modify_prefix_target_register(target, |value| value ^ mask);
                (self.pc.wrapping_add(2), 8)
            }
        }
    }

    #[inline(always)]
    fn bit(&mut self, target: PrefixTarget, idx: u8) -> (u16, u8) {
        let pc;
        let cycles;

        let value = match target {
            PrefixTarget::HLI => {
                let address = self.registers.get_hl();
                (pc, cycles) = (2, 16);
                self.mem.read(address)
            }
            _ => {
                (pc, cycles) = (2, 8);
                self.read_prefix_target_register(&target)
            }
        };

        let mask = 1 << idx;
        let bit = value & mask;

        assert_eq!(bit, value & mask);

        self.registers.f.zero = bit == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        // self.registers.f.carry N.A.

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn cpl(&mut self) -> (u16, u8) {
        // self.registers.f.zero N.A.
        self.registers.f.subtract = true; // Something something BCD
        self.registers.f.half_carry = true; // Something something BCD
                                            // self.registers.f.carry N.A.
        self.registers.a = !self.registers.a;

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn rrca(&mut self) -> (u16, u8) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
                                     //
        let new_value = value.rotate_right(1); // rotate everything right

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn rlca(&mut self) -> (u16, u8) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let left_bit = 0b1000_0000 & value; // Save the last bit by using `and` with 0000_0001
                                            //
        let new_value = value.rotate_left(1); // rotate everything left

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = left_bit != 0; // save the left-most bit into the carry flag

        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn rra(&mut self) -> (u16, u8) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);
        let right_bit = 0b1 & value; // Save the last bit by using `and` with 0000_0001
        let mut new_value = value.rotate_right(1); // rotate everything right
        if self.registers.f.carry {
            // new_value = new_value + 128;
            new_value |= 0b1000_0000 // add the carry flag to the 7th bit
        }

        self.registers.f.zero = false; // result is never 0
        self.registers.f.subtract = false; // we're not subtracting
        self.registers.f.half_carry = false; // half carry is not applicable when accumulating
        self.registers.f.carry = right_bit != 0; // save the right-most bit into the carry flag

        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn rla(&mut self) -> (u16, u8) {
        let value = self.read_8bit_register(&Arithmetic8BitTarget::A);

        let left_most_bit = value & 0b1000_0000; // save the left most bit
        let mut new_value = value << 1; // shift left by 1
        new_value |= self.registers.f.carry as u8; // set the right most bit to value f_carry

        self.registers.f.zero = false;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = left_most_bit != 0;

        self.registers.a = new_value;

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn set_carry(&mut self) -> (u16, u8) {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = true;
        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn set_unset(&mut self) -> (u16, u8) {
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = !self.registers.f.carry;
        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn modify_16bit_register<F>(&mut self, target: Arithmetic16BitTarget, modifier: F) -> u16
    where
        F: FnOnce(u16) -> u16,
    {
        let value = self.read_16bit_register(target);
        let new_value = modifier(value);

        match target {
            Arithmetic16BitTarget::HL => self.registers.set_hl(new_value),
            Arithmetic16BitTarget::BC => self.registers.set_bc(new_value),
            Arithmetic16BitTarget::DE => self.registers.set_de(new_value),
            Arithmetic16BitTarget::SP => self.sp = new_value,
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
    fn match_decrement_target(&mut self, target: AnyTarget) -> (u16, u8) {
        let pc;
        let cycles;
        match target {
            AnyTarget::A => {
                self.decrement_8bit(Arithmetic8BitTarget::A);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::B => {
                self.decrement_8bit(Arithmetic8BitTarget::B);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::D => {
                self.decrement_8bit(Arithmetic8BitTarget::D);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::H => {
                self.decrement_8bit(Arithmetic8BitTarget::H);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::C => {
                self.decrement_8bit(Arithmetic8BitTarget::C);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::E => {
                self.decrement_8bit(Arithmetic8BitTarget::E);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::L => {
                self.decrement_8bit(Arithmetic8BitTarget::L);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::BC => {
                self.decrement_16bit(Arithmetic16BitTarget::BC);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::DE => {
                self.decrement_16bit(Arithmetic16BitTarget::DE);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::HL => {
                self.decrement_16bit(Arithmetic16BitTarget::HL);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::SP => {
                self.decrement_16bit(Arithmetic16BitTarget::SP);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::HLI => {
                (pc, cycles) = (1, 12);
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value.wrapping_sub(1);
                self.mem.write(address, new_value);
                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value & 0xf) == 0x1;
            }
        }

        (self.pc.wrapping_add(pc), cycles)
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
    fn match_increment_target(&mut self, target: AnyTarget) -> (u16, u8) {
        let pc;
        let cycles;
        match target {
            AnyTarget::A => {
                self.increment_8bit(Arithmetic8BitTarget::A);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::B => {
                self.increment_8bit(Arithmetic8BitTarget::B);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::D => {
                self.increment_8bit(Arithmetic8BitTarget::D);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::H => {
                self.increment_8bit(Arithmetic8BitTarget::H);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::C => {
                self.increment_8bit(Arithmetic8BitTarget::C);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::E => {
                self.increment_8bit(Arithmetic8BitTarget::E);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::L => {
                self.increment_8bit(Arithmetic8BitTarget::L);
                (pc, cycles) = (1, 4);
            }
            AnyTarget::BC => {
                self.increment_16bit(Arithmetic16BitTarget::BC);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::DE => {
                self.increment_16bit(Arithmetic16BitTarget::DE);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::HL => {
                self.increment_16bit(Arithmetic16BitTarget::HL);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::SP => {
                self.increment_16bit(Arithmetic16BitTarget::SP);
                (pc, cycles) = (1, 8);
            }
            AnyTarget::HLI => {
                (pc, cycles) = (1, 12);
                let address = self.registers.get_hl();
                let value = self.mem.read(address);
                let new_value = value.wrapping_add(1);
                self.mem.write(address, new_value);
                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.half_carry = (new_value & 0xf) == 0x1;
            }
        }

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn increment_16bit(&mut self, target: Arithmetic16BitTarget) {
        self.modify_16bit_register(target, |value| value.wrapping_add(1));

        // self.registers.f.zero
        // self.registers.f.subtract
        // self.registers.f.half_carry
        // self.registers.f.carry
    }

    #[inline(always)]
    fn increment_8bit(&mut self, target: Arithmetic8BitTarget) {
        let value = self.read_8bit_register(&target);
        let new_value = value.wrapping_add(1);
        self.write_8bit_register(&target, new_value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = (value & 0xf) == 0xf;
        // self.registers.f.carry
    }

    #[inline(always)]
    fn compare(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };

        self.registers.f.zero = self.registers.a == value;
        self.registers.f.subtract = true;
        // Half Carry Flag (H): Checks if the lower nibble of A is less than that of value (indicating a borrow in the lower 4 bits).
        self.registers.f.half_carry = (self.registers.a & 0xF) < (value & 0xF);
        // Carry Flag (C): Checks if A is less than value.
        self.registers.f.carry = self.registers.a < value;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn xor(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };
        let new_value = self.registers.a ^ value;
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn or(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };

        let new_value = self.registers.a | value;
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn and(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };
        let new_value = self.registers.a & value;
        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn subtract_with_carry(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
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

        (self.pc.wrapping_add(1), 4)
    }

    #[inline(always)]
    fn subtract(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };
        let (new_value, overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = true;
        self.registers.f.half_carry = (self.registers.a & 0xf) < (value & 0xf);
        self.registers.f.carry = overflow;

        self.registers.a = new_value;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn add_with_carry(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };

        let (new_value, overflow) = self
            .registers
            .a
            .overflowing_add(value + self.registers.f.carry as u8);

        self.registers.f.zero = new_value == 0;
        self.registers.f.carry = overflow;
        self.registers.f.subtract = false;
        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;

        self.registers.a = new_value;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn add(&mut self, target: Arithmetic8BitTarget) -> (u16, u8) {
        let pc;
        let cycles;
        let value = match target {
            Arithmetic8BitTarget::D8 => {
                (pc, cycles) = (2, 8);
                self.next()
            }
            _ => {
                (pc, cycles) = (1, 4);
                self.read_8bit_register(&target)
            }
        };

        let new_value = self.registers.a.wrapping_add(value);

        self.registers.f.zero = new_value == 0;
        self.registers.f.subtract = false;

        self.registers.f.half_carry = ((self.registers.a & 0xf) + (value & 0xf)) & 0x10 == 0x10;

        self.registers.a = new_value;

        (self.pc.wrapping_add(pc), cycles)
    }

    #[inline(always)]
    fn add_hl(&mut self, target: Arithmetic16BitTarget) -> (u16, u8) {
        let value = self.read_16bit_register(target);
        let (new_value, overflow) = self.registers.get_hl().overflowing_add(value);

        // Don't set 0 flag
        self.registers.f.carry = overflow;
        self.registers.f.subtract = false;

        self.registers.f.half_carry =
            ((self.registers.get_hl() & 0xfff) + (value & 0xfff)) & 0x100 == 0x100;

        self.registers.set_hl(new_value);
        (self.pc.wrapping_add(1), 8)
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
    fn read_load_target_register(&self, target: &LoadTarget) -> u8 {
        match target {
            LoadTarget::A => self.registers.a,
            LoadTarget::B => self.registers.b,
            LoadTarget::C => self.registers.c,
            LoadTarget::D => self.registers.d,
            LoadTarget::E => self.registers.e,
            LoadTarget::H => self.registers.h,
            LoadTarget::L => self.registers.l,
            _ => unimplemented!(),
        }
    }

    #[inline(always)]
    fn write_load_target_register(&mut self, target: &LoadTarget, value: u8) {
        match target {
            LoadTarget::A => self.registers.a = value,
            LoadTarget::B => self.registers.b = value,
            LoadTarget::C => self.registers.c = value,
            LoadTarget::D => self.registers.d = value,
            LoadTarget::E => self.registers.e = value,
            LoadTarget::H => self.registers.h = value,
            LoadTarget::L => self.registers.l = value,
            _ => unimplemented!(),
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

    #[inline(always)]
    fn write_8bit_register(&mut self, target: &Arithmetic8BitTarget, value: u8) {
        match target {
            Arithmetic8BitTarget::A => self.registers.a = value,
            Arithmetic8BitTarget::B => self.registers.b = value,
            Arithmetic8BitTarget::C => self.registers.c = value,
            Arithmetic8BitTarget::D => self.registers.d = value,
            Arithmetic8BitTarget::E => self.registers.e = value,
            Arithmetic8BitTarget::F => self.registers.f = value.into(),
            Arithmetic8BitTarget::H => self.registers.h = value,
            Arithmetic8BitTarget::L => self.registers.l = value,
            _ => unimplemented!(),
        }
    }
}
