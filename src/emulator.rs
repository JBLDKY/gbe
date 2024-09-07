use crate::{cpu::CPU, gpu::GPU, mem::MemCtx};
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::Texture;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::{event::Event, render::Canvas};
use std::time::{Duration, Instant};

pub const TILEMAP_HEIGHT: usize = 32;
pub const TILEMAP_WIDTH: usize = 32;
pub const TILE_SIZE: u32 = 8;
pub const SCREEN_WIDTH: u32 = 160;
pub const SCREEN_HEIGHT: u32 = 144;
pub const SCREEN_BITS: u32 = SCREEN_WIDTH * SCREEN_HEIGHT * 4;
pub const SCALE: u32 = 3;

enum EventResult {
    Continue,
    Stop,
}
pub struct Screen {
    canvas: Canvas<Window>,
    event_pump: EventPump,
}

impl Screen {
    fn new() -> Self {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();

        let window = video_subsystem
            .window(
                "rust-sdl2 demo: Video",
                SCREEN_WIDTH * SCALE,
                SCREEN_HEIGHT * SCALE,
            )
            .position_centered()
            .opengl()
            .build()
            .map_err(|e| e.to_string())
            .unwrap();

        let canvas = window
            .into_canvas()
            .build()
            .map_err(|e| e.to_string())
            .unwrap();

        let event_pump = ctx.event_pump().unwrap();

        Self { canvas, event_pump }
    }

    fn handle_event(&mut self) -> EventResult {
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return EventResult::Stop,
                _ => {}
            }
        }

        EventResult::Continue
    }

    fn render(&mut self, texture: &mut Texture, frame_buffer: &[u8]) {
        texture
            .update(None, frame_buffer, SCREEN_WIDTH as usize * 4)
            .unwrap();

        self.canvas.clear();
        self.canvas
            .copy(
                texture,
                None,
                Some(Rect::new(0, 0, SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE)),
            )
            .unwrap();
        self.canvas.present();
    }
}

pub struct Emulator<T: MemCtx> {
    pub cpu: CPU,
    pub gpu: GPU,
    pub mem: T,
}

impl<T: MemCtx> Emulator<T> {
    pub fn new(mem: T) -> Self {
        Emulator {
            cpu: CPU::new(),
            gpu: GPU::new(),
            mem,
        }
    }
}

impl<T: MemCtx> Emulator<T> {
    pub fn run(mut self) {
        const CYCLES_PER_SECOND: f64 = 4_194_304.0; // Game Boy CPU speed
        const FPS: f64 = 59.7;
        const CYCLES_PER_FRAME: usize = (CYCLES_PER_SECOND / FPS) as usize;

        let mut cycles_this_frame = 0;
        let mut last_update = Instant::now();

        let mut screen = Screen::new();
        let texture_creator = screen.canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::RGBA32, SCREEN_WIDTH, SCREEN_HEIGHT)
            .unwrap();

        'running: loop {
            let cycles = self.cpu.step(&mut self.mem) as usize;

            cycles_this_frame += cycles;

            self.gpu.step(&mut self.mem, cycles);

            if cycles_this_frame >= CYCLES_PER_FRAME {
                cycles_this_frame = 0;

                screen.render(&mut texture, &self.gpu.get_frame_buffer());

                // Frame timing synchronization
                let now = Instant::now();
                let elapsed = now.duration_since(last_update);
                let frame_duration = Duration::from_secs_f64(1.0 / FPS);

                if elapsed < frame_duration {
                    std::thread::sleep(frame_duration - elapsed);
                }

                last_update = now;
            }

            match screen.handle_event() {
                EventResult::Continue => (),
                EventResult::Stop => break 'running,
            }
        }
    }
}
