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

pub trait MemCtx {
    fn read(&self, addr: u16) -> u8;

    fn modify<F>(&mut self, hex: u16, modifier: F)
    where
        F: FnOnce(u8) -> u8;

    fn write(&mut self, addr: u16, value: u8);

    fn lcdc_is_on(&self) -> bool;

    fn get_vram_mut(&mut self) -> [u8; VIDEO_RAM];
}

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

impl MemCtx for Mem {
    fn read(&self, addr: u16) -> u8 {
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
            0x8000..=0x9FFF => {
                self.vram[addr as usize - 0x8000] = value;
            }
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

    fn lcdc_is_on(&self) -> bool {
        self.read(0xFF40) & 0b1000_0000 != 0
    }

    fn get_vram_mut(&mut self) -> [u8; VIDEO_RAM] {
        self.vram
    }
}

impl Mem {
    pub fn new(boot_rom: &[u8], game_rom: &[u8]) -> Mem {
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
}
