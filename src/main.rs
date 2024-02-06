mod cpu;
mod flags_registers;
mod instruction;
mod registers;
use crate::cpu::CPU;
use std::fs::File;
use std::io::{self, Read};
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let rom = buffer_from_file("/home/jord/projects/gbe/roms/yu_gi_oh_dungeon_dice_monsters.gba")
        .expect("invalid rom");

    let mut cpu = CPU::new(&rom);
    let mut cycles = 0;

    loop {
        // count cycles
        cycles += cpu.step() as usize;

        // Dont run as if on steroids
        sleep(Duration::from_nanos(20));

        if cycles > 1000 {
            // Stop early in case pc blows up
            break;
        };
    }

    dbg!(cycles);
}

fn buffer_from_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
