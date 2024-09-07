use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use std::fmt;
use std::time::Duration;

const TILEMAP_HEIGHT: usize = 32;
const TILEMAP_WIDTH: usize = 32;
const TILE_SIZE: u32 = 8;
const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;
const SCALE: u32 = 3;

#[derive(Debug)]
pub struct TileMap {
    pub tiles: [[u8; TILEMAP_WIDTH]; TILEMAP_HEIGHT],
}

impl TileMap {
    pub fn new() -> Self {
        Self {
            tiles: [[0; TILEMAP_WIDTH]; TILEMAP_HEIGHT],
        }
    }

    pub fn set_tile(&mut self, x: usize, y: usize, tile_index: u8) {
        self.tiles[y][x] = tile_index;
    }

    fn render(
        &self,
        canvas: &mut sdl2::render::Canvas<sdl2::video::Window>,
        tiles: &[Tile],
        scx: u8,
        scy: u8,
    ) -> Result<(), String> {
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let map_x = (x as u16 + scx as u16) % (TILEMAP_WIDTH as u16 * TILE_SIZE as u16);
                let map_y = (y as u16 + scy as u16) % (TILEMAP_WIDTH as u16 * TILE_SIZE as u16);

                let tile_x = (map_x / TILE_SIZE as u16) as usize;
                let tile_y = (map_y / TILE_SIZE as u16) as usize;

                let tile_index = self.tiles[tile_y][tile_x] as usize;
                let tile = &tiles[tile_index];

                let pixel_x = map_x % TILE_SIZE as u16;
                let pixel_y = map_y % TILE_SIZE as u16;

                let pixel = tile.lines[pixel_y as usize][pixel_x as usize];
                let color = pixel.as_rgb();

                canvas.set_draw_color(color);
                canvas.fill_rect(Rect::new(
                    (x * SCALE) as i32,
                    (y * SCALE) as i32,
                    SCALE,
                    SCALE,
                ))?;
            }
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub enum Pixel {
    Zero,
    One,
    Two,
    #[default]
    Three,
}

impl Pixel {
    pub fn as_rgb(&self) -> Color {
        match self {
            Pixel::Zero => Color::RGB(255, 255, 255),
            Pixel::One => Color::RGB(192, 192, 192),
            Pixel::Two => Color::RGB(96, 96, 96),
            Pixel::Three => Color::RGB(0, 0, 0),
        }
    }
}

impl fmt::Display for Pixel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Zero => write!(f, "{}", 0),
            Self::One => write!(f, "{}", 1),
            Self::Two => write!(f, "{}", 2),
            Self::Three => write!(f, "{}", 3),
        }
    }
}

impl Pixel {
    fn new(top_bit: bool, bot_bit: bool) -> Self {
        match (top_bit, bot_bit) {
            (true, true) => Self::Three,
            (false, true) => Self::One,
            (true, false) => Self::Two,
            (false, false) => Self::Zero,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Tile {
    pub lines: [[Pixel; 8]; 8],
}

impl Tile {
    fn render(&self, canvas: &mut sdl2::render::Canvas<sdl2::video::Window>) -> Result<(), String> {
        for (y, line) in self.lines.iter().enumerate() {
            for (x, pixel) in line.iter().enumerate() {
                let color = pixel.as_rgb();
                canvas.set_draw_color(color);
                canvas.fill_rect(Rect::new(
                    (x as u32 * SCALE) as i32,
                    (y as u32 * SCALE) as i32,
                    SCALE,
                    SCALE,
                ))?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in self.lines {
            writeln!(
                f,
                "{} {} {} {} {} {} {} {}",
                line[0], line[1], line[2], line[3], line[4], line[5], line[6], line[7],
            )
            .unwrap();
        }

        Ok(())
    }
}

impl From<[u8; 16]> for Tile {
    fn from(sixteen_bytes: [u8; 16]) -> Self {
        let mut tile = Self::default();

        for i in 0..8 {
            let mut mask = 128u8;
            let idx = i * 2;
            let top_byte = sixteen_bytes[idx];
            let bot_byte = sixteen_bytes[idx + 1];
            for j in 0..8 {
                let top_is_set = top_byte & mask != 0;
                let bot_is_set = bot_byte & mask != 0;
                mask >>= 1;
                tile.lines[i][j] = Pixel::new(top_is_set, bot_is_set);
            }
        }

        tile
    }
}

pub fn run_sdl() -> Result<(), String> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let window = video_subsystem
        .window(
            "rust-sdl2 demo: Video",
            SCREEN_WIDTH * SCALE,
            SCREEN_HEIGHT * SCALE,
        )
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;

    let mut event_pump = sdl_context.event_pump()?;

    let mut tiles = [(); 32].map(|_| {
        Tile::from([
            0xFF, 0x00, 0x7E, 0xFF, 0x85, 0x81, 0x89, 0x83, 0x93, 0x85, 0xA5, 0x8B, 0xC9, 0x97,
            0x7E, 0xFF,
        ])
    });

    tiles[0] = Tile::default();
    tiles[2] = Tile::default();
    tiles[4] = Tile::default();
    tiles[6] = Tile::default();
    tiles[8] = Tile::default();
    tiles[10] = Tile::default();
    tiles[12] = Tile::default();
    tiles[14] = Tile::default();
    tiles[16] = Tile::default();
    tiles[18] = Tile::default();
    tiles[20] = Tile::default();
    tiles[22] = Tile::default();
    tiles[24] = Tile::default();
    tiles[26] = Tile::default();
    tiles[28] = Tile::default();
    tiles[30] = Tile::default();

    let mut tile_map = TileMap::new();
    for y in 0..TILEMAP_HEIGHT {
        for x in 0..TILEMAP_WIDTH {
            tile_map.set_tile(x, y, ((x + y) % 2) as u8);
        }
    }
    let mut scx = 0;
    let mut scy = 0;

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        canvas.clear();
        tile_map.render(&mut canvas, &tiles, scx, scy)?;
        canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));

        // enables scrolling
        scx = scx.wrapping_add(1);
        scy = scy.wrapping_add(1);
    }

    Ok(())
}
