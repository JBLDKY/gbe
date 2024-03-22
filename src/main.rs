mod cpu;
mod flags_registers;
mod instruction;
mod registers;
use crate::cpu::CPU;
use ::image::*;
use ctrlc;
use env_logger::{Builder, Env};
use log::LevelFilter;
use log::{debug, error, info, warn};
use std::fs::File;
use std::io::{self, Read, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::sleep;
use std::time::Duration;

use graphics::{clear, Image, Transformed};
use opengl_graphics::{Filter, GlGraphics, OpenGL};
use piston_window::*;

use gfx_device_gl::Resources as GlResources;

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;
const TILE_SIZE: u32 = 8;
const TILE_DATA_SIZE: u32 = 16;

const VIDEO_RAM: u32 = VIDEO_RAM_END - VIDEO_RAM_START + 1;
const VIDEO_RAM_START: u32 = 0x8000;
const VIDEO_RAM_END: u32 = 0x9FFF;

use piston_window::TextureSettings;

fn run(mut cpu: CPU) {
    let opengl = OpenGL::V3_2;
    let mut window: PistonWindow =
        WindowSettings::new("Game Boy Emulator", [SCREEN_WIDTH, SCREEN_HEIGHT])
            .graphics_api(opengl)
            .exit_on_esc(true)
            .build()
            .unwrap();

    let mut gl = GlGraphics::new(opengl);
    let vram = [0u8; 0x2000]; // Replace this with actual VRAM data
    let mut texture = vram_to_texture(&mut window, &cpu.mem.vram);
    let mut canvas = ImageBuffer::new(SCREEN_WIDTH, SCREEN_HEIGHT);

    let mut texture_context = TextureContext {
        factory: window.factory.clone(),
        encoder: window.factory.create_command_buffer().into(),
    };
    // let mut texture: G2dTexture =
    //     Texture::from_image(&mut texture_context, &canvas, &TextureSettings::new()).unwrap();
    while let Some(e) = window.next() {
        sleep(Duration::from_nanos(20));
        cpu.step();
        if e.render_args().is_some() {
            texture.update(&mut texture_context, &canvas).unwrap();
            window.draw_2d(&e, |c, g, device| {
                // Update texture before rendering.
                texture_context.encoder.flush(device);

                clear([1.0; 4], g);
                image(&texture, c.transform, g);
            });
        }
    }
}

fn vram_to_texture(window: &mut PistonWindow, vram: &[u8]) -> Texture<GlResources> {
    let mut buffer: Vec<u8> = vec![0; (SCREEN_WIDTH * SCREEN_HEIGHT * 4) as usize];

    for tile_y in 0..(SCREEN_HEIGHT / TILE_SIZE) {
        for tile_x in 0..(SCREEN_WIDTH / TILE_SIZE as u32) {
            let tile_index = tile_y * (SCREEN_WIDTH / TILE_SIZE as u32) + tile_x;

            // VRAM starts at 0 in your array, so subtract VIDEO_RAM_START
            let tile_addr = tile_index * TILE_DATA_SIZE as u32;

            for row in 0..TILE_SIZE {
                // Now, tile_addr correctly points to the start of each tile in VRAM
                let byte1 = vram[(tile_addr as usize + (row * 2) as usize) as usize];
                let byte2 = vram[(tile_addr as usize + (row * 2 + 1) as usize) as usize];

                for col in 0..TILE_SIZE {
                    let color_bit = ((byte1 >> (7 - col)) & 1) | (((byte2 >> (7 - col)) & 1) << 1);
                    let color = map_color_bit_to_rgba(color_bit);
                    let buffer_index = ((tile_y * TILE_SIZE as u32 + row as u32) * SCREEN_WIDTH
                        + tile_x * TILE_SIZE
                        + col)
                        * 4;
                    buffer[buffer_index as usize..(buffer_index + 4) as usize]
                        .copy_from_slice(&color);
                }
            }
        }
    }

    let mut texture_context = TextureContext {
        factory: window.factory.clone(),
        encoder: window.factory.create_command_buffer().into(),
    };

    let mut texture_settings = TextureSettings::new();

    Texture::from_memory_alpha(
        &mut texture_context,
        &buffer,
        SCREEN_WIDTH,
        SCREEN_HEIGHT,
        &texture_settings,
    )
    .unwrap()
}

fn map_color_bit_to_rgba(bit: u8) -> [u8; 4] {
    match bit {
        0 => [0, 0, 0, 255],       // Black
        1 => [85, 85, 85, 255],    // Dark Gray
        2 => [170, 170, 170, 255], // Light Gray
        3 => [255, 255, 255, 255], // White
        _ => [0, 0, 0, 255],       // Default to Black
    }
}

fn main() {
    info!("starting up");
    let env = Env::new().filter("MY_LOG").write_style("MY_LOG_STYLE");

    let target = Box::new(File::create("./log.txt").expect("Can't create file"));

    // let running = Arc::new(AtomicBool::new(true));
    // let r = running.clone();
    // ctrlc::set_handler(move || r.store(false, Ordering::SeqCst)).expect("could not init ctrlc");

    Builder::new()
        .format(|buf, record| {
            writeln!(
                buf,
                "{}:{} [{}] - {}",
                record.file().unwrap_or("unknown"),
                record.line().unwrap_or(0),
                record.level(),
                record.args()
            )
        })
        .parse_env(env)
        .target(env_logger::Target::Pipe(target))
        .filter_level(LevelFilter::Debug)
        .init();

    let boot_rom =
        buffer_from_file("/home/jord/projects/gbe/roms/dmg_boot.bin").expect("boot_rom rom");

    let rom =
        buffer_from_file("/home/jord/projects/gbe/roms/pokemon_blue.gb").expect("invalid rom");

    let mut cpu = CPU::new(&boot_rom, &rom);
    let mut cycles = 0;
    loop {
        // count cycles
        cycles += cpu.step() as usize;

        // Dont run as if on steroids
        // sleep(Duration::from_nanos(20));
    }
}

fn buffer_from_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
