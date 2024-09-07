use crate::{
    emulator::{SCREEN_BITS, SCREEN_HEIGHT, SCREEN_WIDTH},
    mem::MemCtx,
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

        // self.mode = match self.ly {
        //     144..=153 => Mode::VBlank,
        //     _ => match self.cycles {
        //         0..=80 => Mode::OAMSearch,
        //         81..=253 => Mode::Drawing,
        //         254..=456 => Mode::HBlank,
        //         _ => panic!("GPU Cycles are out of bounds"),
        //     },
        // };

        if self.cycles >= 456 {
            self.cycles -= 456;
            self.ly += 1;
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
}

// fn handle_hblank<T: MemCtx>(&mut self, mem: &mut T) {
//     self.cycles %= HBLANK_CYCLES;
//     self.increment_line_y(mem);
//     self.mode = if self.ly >= 144 {
//         Mode::VBlank
//     } else {
//         Mode::OAMSearch
//     };
// }
//
// fn handle_vblank<T: MemCtx>(&mut self, mem: &mut T) {
//     self.cycles %= VBLANK_CYCLES;
//     self.increment_line_y(mem);
//     if self.ly > 153 {
//         self.ly = 0;
//         self.mode = Mode::OAMSearch;
//         if self.oam_interrupt_enabled {
//             // TODO: something with setting a VBLank interrupt
//         }
//     }
//     mem.write(0xFF0F, mem.read(0xFF0F) | 0x01);
// }
//
// fn handle_oam_search(&mut self) {
//     self.cycles %= OAM_CYCLES;
//     self.mode = Mode::Drawing;
// }
//
// fn handle_vram_transfer(&mut self) {
//     self.cycles %= VRAM_XFER_CYCLES;
//     self.mode = Mode::HBlank;
// }
//
// fn enter_vblank_mode<T: MemCtx>(&mut self, mem: &mut T) {
//     self.mode = Mode::VBlank;
//     // The following toggles on or off
//     mem.write(0xFF0F, mem.read(0xFF0F) | 0x01);
// }
//
// fn increment_line_y<T: MemCtx>(&mut self, mem: &mut T) {
//     self.ly = self.ly.wrapping_add(1);
//     mem.write(0xFF44, self.ly);
//     if self.ly == 144 {
//         self.enter_vblank_mode(mem);
//     } else if self.ly > 153 {
//         self.ly = 0;
//     }
// }
