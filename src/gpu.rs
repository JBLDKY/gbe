const SPRITE: usize = SPRITE_END - SPRITE_START + 1;
const SPRITE_START: usize = 0xFE00;
const SPRITE_END: usize = 0xFE9F;

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
            mode: Mode::OAMSearch,
            lcd_status: 0,
        }
    }
}
