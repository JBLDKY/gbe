use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::rect::Rect;
use std::fmt;
use std::time::Duration;

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;
const SCALE: u32 = 3;

#[derive(Copy, Clone, Debug)]
enum Pixel {
    Zero,
    One,
    Two,
    Three,
}

impl Pixel {
    fn as_rgb(&self) -> Color {
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

#[derive(Debug)]
struct Tile {
    lines: [[Pixel; 8]; 8],
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

impl Default for Tile {
    fn default() -> Self {
        Self {
            lines: [[Pixel::Zero; 8]; 8],
        }
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

    let tile = Tile::from([
        0xFF, 0x00, 0x7E, 0xFF, 0x85, 0x81, 0x89, 0x83, 0x93, 0x85, 0xA5, 0x8B, 0xC9, 0x97, 0x7E,
        0xFF,
    ]);

    println!("{}", tile);

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
        tile.render(&mut canvas)?;
        canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));
    }

    Ok(())
}
