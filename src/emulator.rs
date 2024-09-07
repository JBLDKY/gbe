use crate::{cpu::CPU, gpu::GPU, mem::MemCtx};
use sdl2::keyboard::Keycode;
use sdl2::video::Window;
use sdl2::{event::Event, render::Canvas};
use sdl2::{EventPump, Sdl};
use std::time::{Duration, Instant};

const TILEMAP_HEIGHT: usize = 32;
const TILEMAP_WIDTH: usize = 32;
const TILE_SIZE: u32 = 8;
const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;
const SCALE: u32 = 3;

enum EventResult {
    Continue,
    Stop,
}
pub struct Screen {
    canvas: Canvas<Window>,
    event_pump: EventPump,
    ctx: Sdl,
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

        Self {
            canvas,
            event_pump,
            ctx,
        }
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

    fn step(&mut self) {
        self.canvas.clear();
        self.canvas.present();
        std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 30));
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

        'running: loop {
            let cycles = self.cpu.step(&mut self.mem) as usize;

            cycles_this_frame += cycles;

            self.gpu.step(&mut self.mem, cycles);

            if cycles_this_frame >= CYCLES_PER_FRAME {
                cycles_this_frame = 0;

                // Frame timing synchronization
                let now = Instant::now();
                let elapsed = now.duration_since(last_update);
                let frame_duration = Duration::from_secs_f64(1.0 / FPS);

                if elapsed < frame_duration {
                    std::thread::sleep(frame_duration - elapsed);
                }

                last_update = now;
            }

            screen.step();
            match screen.handle_event() {
                EventResult::Continue => (),
                EventResult::Stop => break 'running,
            }
        }
    }
}
