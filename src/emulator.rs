use crate::apu::APU;
use crate::mem::Mem;
use crate::{cpu::CPU, gpu::GPU};
use sdl2::audio::AudioCallback;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::render::Texture;
use sdl2::video::Window;
use sdl2::EventPump;
use sdl2::{event::Event, render::Canvas};
use std::time::{Duration, Instant};

pub const SCREEN_WIDTH: u32 = 160;
pub const SCREEN_HEIGHT: u32 = 144;
pub const SCREEN_BITS: u32 = SCREEN_WIDTH * SCREEN_HEIGHT * 4;
pub const SCALE: u32 = 3;

impl AudioCallback for APU {
    type Channel = i16;

    fn callback(&mut self, out: &mut [i16]) {
        for chunk in out.chunks_mut(2) {
            let (left, right) = self.get_sample();
            chunk[0] = left;
            chunk[1] = right;
        }
    }
}

enum EventResult {
    Continue,
    Stop,
    ToggleTestScreen,
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
                Event::KeyDown {
                    keycode: Some(Keycode::T),
                    ..
                } => return EventResult::ToggleTestScreen,
                _ => {}
            }
        }

        EventResult::Continue
    }

    fn test_render(&mut self, texture: &mut Texture, _: &[u8]) {
        let mut test_buffer = vec![0; (SCREEN_WIDTH * SCREEN_HEIGHT * 4) as usize];
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let index = ((y * SCREEN_WIDTH + x) * 4) as usize;
                test_buffer[index] = (x % 256) as u8;
                test_buffer[index + 1] = (y % 256) as u8;
                test_buffer[index + 2] = 128;
                test_buffer[index + 3] = 255;
            }
        }

        texture
            .update(None, &test_buffer, SCREEN_WIDTH as usize * 4)
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

    fn render(&mut self, texture: &mut Texture, frame_buffer: &[u8]) {
        // dbg!(frame_buffer);
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

pub struct Emulator {
    pub cpu: CPU,
    pub gpu: GPU,
    pub mem: Mem,
}

impl Emulator {
    pub fn new(mem: Mem) -> Self {
        Emulator {
            cpu: CPU::new(),
            gpu: GPU::new(),
            mem,
        }
    }
}

impl Emulator {
    pub fn run(mut self) {
        const CYCLES_PER_SECOND: f64 = 4_194_304.0; // Game Boy CPU speed
        const FPS: f64 = 59.7;
        const CYCLES_PER_FRAME: usize = (CYCLES_PER_SECOND / FPS) as usize;

        let mut cycles_this_frame = 0;
        let mut last_update = Instant::now();

        let mut render_test_screen = false;

        let mut screen = Screen::new();
        let texture_creator = screen.canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture_streaming(PixelFormatEnum::RGBA32, SCREEN_WIDTH, SCREEN_HEIGHT)
            .unwrap();

        let mut apu = APU::default();
        'running: loop {
            let cycles = self.cpu.step(&mut self.mem) as usize;

            cycles_this_frame += cycles;

            apu.step(&mut self.mem, cycles);
            self.gpu.step(&mut self.mem, cycles);

            if cycles_this_frame >= CYCLES_PER_FRAME {
                cycles_this_frame = 0;

                if render_test_screen {
                    screen.test_render(&mut texture, &self.gpu.get_frame_buffer());
                } else {
                    screen.render(&mut texture, &self.gpu.get_frame_buffer());
                }

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
                EventResult::ToggleTestScreen => render_test_screen = !render_test_screen,
                EventResult::Stop => break 'running,
            }
        }
    }
}
