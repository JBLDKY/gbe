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
    oam: [u8; SPRITE],     // Define Sprite struct and OAM_SIZE constant
    vram: [u8; VRAM_SIZE], // Define VRAM_SIZE constant
    lcd_status: u8,
}
