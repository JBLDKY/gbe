// fn get_bc(&self) -> u16 {
//     // How to create a 16 bit register from two 8 bit registers.
//     //
//     // Pad the byte in the `b` register with 8 more bits
//     // e.g.:
//     // self.b = 0101 0101
//     // self.c = 0110 0110
//     // 0101 0101 0000 000 <-- add padding by bitshifting 8 to the left
//     //
//     // Now the last 8 bits have space to fit the contents of register c
//     // This is done by using the bitwise `or`. Since register c is only 8 bits
//     // and register b ends with 8 unset bits, we can fit the entire c register
//     // into our new 16 bit result.
//     //
//     // 0101 0101 0000 000 | 0110 0110  <-- bitwise `or`
//     //
//     //0101 0101 0110 0110
//
//     self.b << 8 | self.c
// }

// fn set_bc(&mut self, value: u16) -> u16 {
//     // Set two 8-bit registers as if they were a 16 bit register.
//     // e.g.:
//     //
//     // self.b = 0000 0000
//     // self.c = 0000 0000
//     // value = 1011 0000 0000 0001
//     //
//     // We take the first 8 bits of `value` and use the `and) operator
//     // to see what bits remain set when comparing with 0xFF00.
//     // 1011 0000 0000 0001 & 1111 1111 0000 0000 = 1011 0000 0000 0000
//     //
//     // The result is shifted 8 to the right and cast to a byte so that it can be stored in register b.
//     // 1011 0000 0000 0000 >> 8 = 1011 0000  // store in b
//     //
//     // The same happens for `c` but since we only care about the last 8 bits we don't need
//     // to do any bitshifting.
//
//     self.b = ((value & 0xFF00) >> 8) as u8;
//     self.c = (value & 0x00) as u8;
//

use crate::flags_registers::FlagsRegister;

#[allow(dead_code)]
#[derive(Copy, Clone)]
pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsRegister,
    pub h: u8,
    pub l: u8,
}

#[allow(dead_code)]
impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: FlagsRegister::new(),
            h: 0,
            l: 0,
        }
    }

    // AF
    pub fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((value & 0xFF) as u8);
    }

    pub fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | (u8::from(self.f) as u16)
    }

    // BC
    pub fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) >> 8) as u8;
        self.c = (value & 0xFF) as u8;
    }

    pub fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | (self.c as u16)
    }

    // DE
    pub fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) >> 8) as u8;
        self.e = (value & 0xFF) as u8;
    }

    pub fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | (self.e as u16)
    }

    // HL
    pub fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) >> 8) as u8;
        self.l = (value & 0xFF) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | (self.l as u16)
    }
}
