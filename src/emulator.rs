use crate::{cpu::CPU, gpu::GPU, mem::MemCtx};
use std::time::{Duration, Instant};

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

        loop {
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
        }
    }
}
