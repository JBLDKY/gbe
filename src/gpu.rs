use crate::mem::MemCtx;

const OAM_CYCLES: u16 = 80;
const VRAM_XFER_CYCLES: u16 = 172;
const HBLANK_CYCLES: u16 = 200;
const VBLANK_CYCLES: u16 = 456;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    HBlank,
    VBlank,
    OAMSearch,
    VRAMTransfer,
}

#[derive(Debug)]
pub struct GPU {
    vblank_interrupt_enabled: bool,
    hblank_interrupt_enabled: bool,
    oam_interrupt_enabled: bool,
    lyc_interrupt_enabled: bool,
    scroll_x: u8,
    scroll_y: u8,
    bg_palette: u8,
    obj_palette0: u8,
    obj_palette1: u8,
    line_y: u8,
    line_y_compare: u8,
    mode: Mode,
    lcd_status: u8,
    cycles: u16,
}

impl GPU {
    pub fn new() -> Self {
        GPU {
            vblank_interrupt_enabled: false,
            hblank_interrupt_enabled: false,
            oam_interrupt_enabled: false,
            lyc_interrupt_enabled: false,
            scroll_x: 0,
            scroll_y: 0,
            bg_palette: 0,
            obj_palette0: 0,
            obj_palette1: 0,
            line_y: 0,
            line_y_compare: 0,
            mode: Mode::HBlank,
            lcd_status: 0,
            cycles: 0,
        }
    }

    pub fn step<T: MemCtx>(&mut self, mem: &mut T, cycles: usize) {
        self.cycles += cycles as u16;

        match self.mode {
            Mode::HBlank if self.cycles >= HBLANK_CYCLES => self.handle_hblank(mem),
            Mode::VBlank if self.cycles >= VBLANK_CYCLES => self.handle_vblank(mem),
            Mode::OAMSearch if self.cycles >= OAM_CYCLES => self.handle_oam_search(),
            Mode::VRAMTransfer if self.cycles >= VRAM_XFER_CYCLES => self.handle_vram_transfer(),
            _ => (),
        }
    }

    fn handle_hblank<T: MemCtx>(&mut self, mem: &mut T) {
        self.cycles %= HBLANK_CYCLES;
        self.increment_line_y(mem);
        self.mode = if self.line_y >= 144 {
            Mode::VBlank
        } else {
            Mode::OAMSearch
        };
    }

    fn handle_vblank<T: MemCtx>(&mut self, mem: &mut T) {
        self.cycles %= VBLANK_CYCLES;
        self.increment_line_y(mem);
        if self.line_y > 153 {
            self.line_y = 0;
            self.mode = Mode::OAMSearch;
            if self.oam_interrupt_enabled {
                // TODO: something with setting a VBLank interrupt
            }
        }
        mem.write(0xFF0F, mem.read(0xFF0F) | 0x01);
    }

    fn handle_oam_search(&mut self) {
        self.cycles %= OAM_CYCLES;
        self.mode = Mode::VRAMTransfer;
    }

    fn handle_vram_transfer(&mut self) {
        self.cycles %= VRAM_XFER_CYCLES;
        self.mode = Mode::HBlank;
    }

    fn enter_vblank_mode<T: MemCtx>(&mut self, mem: &mut T) {
        self.mode = Mode::VBlank;
        // The following toggles on or off
        mem.write(0xFF0F, mem.read(0xFF0F) | 0x01);
    }

    fn increment_line_y<T: MemCtx>(&mut self, mem: &mut T) {
        self.line_y = self.line_y.wrapping_add(1);
        mem.write(0xFF44, self.line_y);
        if self.line_y == 144 {
            self.enter_vblank_mode(mem);
        } else if self.line_y > 153 {
            self.line_y = 0;
        }
    }
}
