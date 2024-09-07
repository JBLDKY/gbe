use crate::{
    emulator::{
        SCREEN_BITS, SCREEN_HEIGHT, SCREEN_WIDTH, TILEMAP_HEIGHT, TILEMAP_WIDTH, TILE_SIZE,
    },
    mem::MemCtx,
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
}

impl GPU {
    pub fn get_frame_buffer(&self) -> [u8; SCREEN_BITS as usize] {
        self.frame_buffer
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
        }
    }

    pub fn step<T: MemCtx>(&mut self, mem: &mut T, cycles: usize) {
        if !mem.lcdc_is_on() {
            return;
        }
        debug!(
            "cycles: {}, mode: {:#?}, ly: {}, frame: {}",
            self.cycles, self.mode, self.ly, self.frame
        );

        self.cycles += cycles as u16;

        if self.cycles >= 456 {
            self.cycles -= 456;
            self.ly += 1;

            if self.ly < SCREEN_HEIGHT as u8 {
                self.render_scanline();
            }
        }

        if self.ly == 144 {
            self.mode = Mode::VBlank;
        } else if self.ly == 154 {
            self.ly = 0;
            self.frame += 1;
            self.mode = Mode::OAMSearch;
            self.cycles = 0
        }

        if self.ly < 144 {
            self.mode = match self.cycles {
                0..=80 => Mode::OAMSearch,
                81..=253 => Mode::Drawing,
                _ => Mode::HBlank,
            }
        };
    }

    fn render_scanline(&mut self) {
        // very clean
        for x in 0..SCREEN_WIDTH {
            let map_x = (x as u16 + self.scx as u16) % (TILEMAP_WIDTH as u16 * TILE_SIZE as u16);
            let map_y =
                (self.ly as u16 + self.scy as u16) % (TILEMAP_HEIGHT as u16 * TILE_SIZE as u16);

            let tile_x = (map_x / TILE_SIZE as u16) as usize;
            let tile_y = (map_y / TILE_SIZE as u16) as usize;

            let tile_index = self.tilemap.tiles[tile_y][tile_x] as usize;
            let tile = &self.tiles[tile_index];

            let pixel_x = (map_x % TILE_SIZE as u16) as usize;
            let pixel_y = (map_y % TILE_SIZE as u16) as usize;

            let pixel = tile.lines[pixel_y][pixel_x];
            let color = pixel.as_rgb();

            let buffer_index = (self.ly as usize * SCREEN_WIDTH as usize + x as usize) * 4;
            self.frame_buffer[buffer_index] = color.r;
            self.frame_buffer[buffer_index + 1] = color.g;
            self.frame_buffer[buffer_index + 2] = color.b;
            self.frame_buffer[buffer_index + 3] = 255; // TODO: This should be transparent
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
