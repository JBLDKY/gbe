use crate::{
    emulator::{SCREEN_BITS, SCREEN_HEIGHT, SCREEN_WIDTH},
    mem::{Mem, MemCtx},
    sdl::{Tile, TileMap},
};

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    HBlank,
    VBlank,
    OAMSearch,
    Drawing,
}

#[derive(Debug)]
pub struct GPU {
    mode: Mode,
    cycles: u16,
    frame: u64,
    frame_buffer: [u8; SCREEN_BITS as usize],

    /// LCD Control Register (LCDC) - Address: 0xFF40
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - LCD Display Enable             (0=Off, 1=On)
    /// Bit 6 - Window Tile Map Area           (0=9800-9BFF, 1=9C00-9FFF)
    /// Bit 5 - Window Display Enable          (0=Off, 1=On)
    /// Bit 4 - BG & Window Tile Data Area     (0=8800-97FF, 1=8000-8FFF)
    /// Bit 3 - BG Tile Map Area               (0=9800-9BFF, 1=9C00-9FFF)
    /// Bit 2 - OBJ Size                       (0=8x8, 1=8x16)
    /// Bit 1 - OBJ Display Enable             (0=Off, 1=On)
    /// Bit 0 - BG & Window Display/Priority   (0=Off, 1=On)
    ///
    /// https://gbdev.io/pandocs/LCDC.html
    lcdc: u8,
    /// ---------------------------------------------------------
    ///
    /// LCD Status Register (STAT) - Address: 0xFF41
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - No special meaning
    /// Bit 6 - LYC=LY Interrupt              (0=Off, 1=On)
    /// Bit 5 - Mode 2 OAM Interrupt          (0=Off, 1=On)
    /// Bit 4 - Mode 1 V-Blank Interrupt      (0=Off, 1=On)
    /// Bit 3 - Mode 0 H-Blank Interrupt      (0=Off, 1=On)
    /// Bit 2 - LYC=LY Flag                   (0=Different, 1=Equal) Set if LYC == LY
    /// Bit 1-0 - Mode Flag                   (0=Off, 1=On) PPU Status
    ///           0: H-Blank
    ///           1: V-Blank
    ///           2: Searching OAM
    ///           3: Transferring Data to LCD Controller
    stat: u8,
    /// ---------------------------------------------------------
    ///
    /// Scroll X - Address: 0xFF42
    ///
    /// Determines the top-left X coordinate of the visible screen-sized area within
    /// the BG map.
    scx: u8,
    /// ---------------------------------------------------------
    ///
    /// Scroll Y - Address: 0xFF43
    ///
    /// Determines the top-left Y coordinate of the visible screen-sized area within
    /// the BG map.
    scy: u8,
    /// ---------------------------------------------------------
    ///
    /// LY: LCD Y Coordinate - Address: 0xFF44
    /// The current horizontal scanline.
    ly: u8,
    /// ---------------------------------------------------------
    ///
    /// LYC: LY compare - Address: 0xFF45
    ///
    /// Gameboy constantly compares LYC and LY. If they are equal, Bit 2 of 0xFF41 is set.
    /// That then triggers a STAT interrupt
    lyc: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    /// Window Y - Address: 0xFF4A
    ///
    /// Specifies the on-screen Y-coordinate of the Window's top-left pixel.
    wy: u8,
    /// ---------------------------------------------------------
    ///
    /// Window X position - Address: 0xFF4A
    ///
    /// You'd think wx = 0 is the origin but it is actually wx = 7
    ///
    /// Specifies the on-screen X-coordinate of the Window's top-left pixel.
    wx: u8,
}

impl GPU {
    pub fn get_frame_buffer(&self) -> [u8; SCREEN_BITS as usize] {
        self.frame_buffer
    }

    fn write_flags<T: MemCtx>(&self, mem: &mut T) {
        mem.write(0xFF40, self.lcdc); // LDCD Control

        let stat = (mem.read(0xFF41) & 0xF8) | (self.stat & 0x07);
        mem.write(0xFF41, stat);

        mem.write(0xFF42, self.scy);
        mem.write(0xFF43, self.scx);
        mem.write(0xFF44, self.ly);
        mem.write(0xFF45, self.lyc);
        mem.write(0xFF47, self.bgp);
        mem.write(0xFF48, self.obp0);
        mem.write(0xFF49, self.obp1);
        mem.write(0xFF4A, self.wy);
        mem.write(0xFF44, self.ly);
    }

    fn read_flags(&mut self, mem: &Mem) {
        self.lcdc = mem.read(0xFF40);
        self.stat = mem.read(0xFF41);
        self.scy = mem.read(0xFF42);
        self.scx = mem.read(0xFF43);
        self.ly = mem.read(0xFF44);
        self.lyc = mem.read(0xFF45);
        self.bgp = mem.read(0xFF47);
        self.obp0 = mem.read(0xFF48);
        self.obp1 = mem.read(0xFF49);
        self.wy = mem.read(0xFF4A);
        self.wx = mem.read(0xFF4B);
    }

    pub fn new() -> Self {
        GPU {
            ly: 0,
            mode: Mode::OAMSearch,
            cycles: 0,
            frame: 0,
            scx: 0,
            scy: 0,
            frame_buffer: [0; SCREEN_BITS as usize],

            lcdc: 0,
            stat: 0,
            lyc: 0,
            bgp: 0,
            obp0: 0,
            obp1: 0,
            wy: 0,
            wx: 0,
        }
    }

    pub fn step(&mut self, mem: &mut Mem, cycles: usize) {
        // TODO: This sucks
        // Take the required values from memory that represent the
        // attributes of GPU.
        self.read_flags(mem);

        if !mem.lcdc_is_on() {
            // The gpu shouldn't do anything if the screen is not turned on
            // TODO: Research what happens if a game turns off the screen
            // Do we need to reset GPU values? do we need to remember any state?
            return;
        }

        // synchronize the GPU to the processor cycles
        self.cycles += cycles as u16;
        // println!(
        //     "cycles: {}, mode: {:#?}, ly: {}, frame: {}",
        //     self.cycles, self.mode, self.ly, self.frame
        // );

        if self.cycles >= 456 {
            // Marks the end of a scanline
            // one scanline = 456 cycles
            self.cycles -= 456;
            self.ly = (self.ly + 1) % 154; // After 154 lines, reset to 0

            if self.ly < SCREEN_HEIGHT as u8 {
                // We draw lines 0 - 143 (as many as there are horizontal pixels on the screen)
                // line 144 - 153 are VBlank and don't require drawing
                self.render_scanline(mem);
            }
        }

        if self.ly == 144 {
            // Enter VBlank
            self.mode = Mode::VBlank;
        } else if self.ly == 154 {
            // Exit VBlank
            self.ly = 0; // Back to line 0
            self.frame += 1; // Frame is finished
            self.mode = Mode::OAMSearch; // Start with OAM Search

            self.cycles = 0 // TODO: Not sure if this is correct
        }

        if self.frame % 100 == 0 {
            // Generic logging every 100th frame
            println!(
                "Frame: {}, LCDC: {:02X}, STAT: {:02X}, SCY: {:02X}, SCX: {:02X}",
                self.frame, self.lcdc, self.stat, self.scy, self.scx
            );
        }

        if self.ly < 144 {
            // Enter the correct mode based on line number
            // TODO: Implement extended draw mode
            // move this to the enter VBlank part?
            self.mode = match self.cycles {
                0..=80 => Mode::OAMSearch,
                81..=253 => Mode::Drawing,
                _ => Mode::HBlank,
            }
        };

        self.stat &= 0xFC;
        if self.ly == self.lyc {
            self.stat |= 0x04;
        }

        if self.ly == 143 {
            self.frame += 1;
        }

        // Write the updated attributes that were read from memory earlier
        // back to memory
        self.write_flags(mem);
    }

    fn render_scanline<T: MemCtx>(&mut self, mem: &T) {
        let tile_map_addr = if (self.lcdc & 0x08) != 0 {
            0x9C00
        } else {
            0x9800
        };
        let tile_data_addr = if (self.lcdc & 0x10) != 0 {
            0x8000
        } else {
            0x8800
        };

        for x in 0..SCREEN_WIDTH {
            let scroll_x = (x as u16 + self.scx as u16) % 256;
            let scroll_y = (self.ly as u16 + self.scy as u16) % 256;

            let tile_x = scroll_x / 8;
            let tile_y = scroll_y / 8;

            let tile_addr = tile_map_addr + tile_y * 32 + tile_x;
            let tile_num = mem.read(tile_addr);

            let tile_data_addr = if tile_data_addr == 0x8000 {
                tile_data_addr + (tile_num as u16) * 16
            } else {
                (tile_data_addr as i32 + ((tile_num as i8 as i32) * 16)) as u16
            };

            let line = (scroll_y % 8) * 2;
            let low_byte = mem.read(tile_data_addr + line);
            let high_byte = mem.read(tile_data_addr + line + 1);

            let bit = 7 - (scroll_x % 8);
            let color_num = ((high_byte >> bit) & 1) << 1 | ((low_byte >> bit) & 1);

            let color = match color_num {
                0 => 255,
                1 => 192,
                2 => 96,
                3 => 0,
                _ => unreachable!(),
            };

            let buffer_index = (self.ly as usize * SCREEN_WIDTH as usize + x as usize) * 4;
            self.frame_buffer[buffer_index] = color;
            self.frame_buffer[buffer_index + 1] = color;
            self.frame_buffer[buffer_index + 2] = color;
            self.frame_buffer[buffer_index + 3] = 255;
        }
    }
}
