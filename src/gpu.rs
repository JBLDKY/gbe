use crate::{
    emulator::{
        SCREEN_BITS, SCREEN_HEIGHT, SCREEN_WIDTH, TILEMAP_HEIGHT, TILEMAP_WIDTH, TILE_SIZE,
    },
    mem::{Mem, MemCtx},
    sdl::{Tile, TileMap},
};
use log::debug;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    HBlank,
    VBlank,
    OAMSearch,
    Drawing,
}

#[derive(Debug)]
pub struct GPU {
    oam_interrupt_enabled: bool,
    ly: u8,
    line_y_compare: u8,
    mode: Mode,
    lcd_status: u8,
    cycles: u16,
    frame: u64,
    tiles: Vec<Tile>,
    tilemap: TileMap,
    scx: u8,
    scy: u8,
    frame_buffer: [u8; SCREEN_BITS as usize],

    lcdc: u8,
    stat: u8,
    lyc: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,
}

impl GPU {
    pub fn get_frame_buffer(&self) -> [u8; SCREEN_BITS as usize] {
        self.frame_buffer
    }
    fn write_flags<T: MemCtx>(&self, mem: &mut T) {
        mem.write(0xFF40, self.lcdc);

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
            oam_interrupt_enabled: false,
            ly: 0,
            line_y_compare: 0,
            mode: Mode::OAMSearch,
            lcd_status: 0,
            cycles: 0,
            frame: 0,
            tiles: vec![Tile::default(); 384], // 384 tiles in VRAM
            tilemap: TileMap::new(),
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
            println!("LCD is off (LCDC: {:02X})", self.lcdc);
            return;
        }

        self.cycles += cycles as u16;
        println!(
            "cycles: {}, mode: {:#?}, ly: {}, frame: {}",
            self.cycles, self.mode, self.ly, self.frame
        );

        if self.cycles >= 456 {
            self.cycles -= 456;
            println!("Next line");
            self.ly = (self.ly + 1) % 154;

            if self.ly < SCREEN_HEIGHT as u8 {
                self.render_scanline(mem);
            }
        }

        if self.ly == 144 {
            println!("Entered VBLANK");
            self.mode = Mode::VBlank;
        } else if self.ly == 154 {
            self.ly = 0;
            self.frame += 1;
            self.mode = Mode::OAMSearch;
            self.cycles = 0
        }

        if self.ly == 0 && self.cycles == 0 && self.frame % 100 == 0 {
            // Generic debugging
            println!(
                "Frame: {}, LCDC: {:02X}, STAT: {:02X}, SCY: {:02X}, SCX: {:02X}",
                self.frame, self.lcdc, self.stat, self.scy, self.scx
            );
        }

        if self.ly < 144 {
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

    pub fn write_vram<T: MemCtx>(&mut self, mem: &mut T, address: u16, value: u8) {
        let vram_address = address as usize - 0x8000;
        let mut vram = mem.get_vram_mut();

        vram[vram_address] = value;

        if vram_address < 0x1800 {
            // Tile Datae
            let tile_index = vram_address / 16;
            let tile_byte = vram_address % 16;

            let tile_data = [
                vram[tile_index * 16],
                vram[tile_index * 16 + 1],
                vram[tile_index * 16 + 2],
                vram[tile_index * 16 + 3],
                vram[tile_index * 16 + 4],
                vram[tile_index * 16 + 5],
                vram[tile_index * 16 + 6],
                vram[tile_index * 16 + 7],
                vram[tile_index * 16 + 8],
                vram[tile_index * 16 + 9],
                vram[tile_index * 16 + 10],
                vram[tile_index * 16 + 11],
                vram[tile_index * 16 + 12],
                vram[tile_index * 16 + 13],
                vram[tile_index * 16 + 14],
                vram[tile_index * 16 + 15],
            ];
            self.tiles[tile_index] = Tile::from(tile_data);
        } else {
            // TileMap
            let tilemap_index = vram_address - 0x1800;
            let x = tilemap_index % TILEMAP_WIDTH;
            let y = tilemap_index / TILEMAP_WIDTH;
            self.tilemap.set_tile(x, y, value);
        }
    }
}
