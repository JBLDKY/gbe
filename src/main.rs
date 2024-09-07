mod cpu;
mod emulator;
mod flags_registers;
mod gpu;
mod instruction;
mod mem;
mod registers;
mod sdl;
mod timer;
mod units;

use env_logger::{Builder, Env};
use log::info;
use log::LevelFilter;
use sdl::run_sdl;
use std::fs::File;
use std::io::{self, Read, Write};

use crate::emulator::Emulator;
use crate::mem::Mem;

fn main() {
    info!("starting up");
    let env = Env::new().filter("MY_LOG").write_style("MY_LOG_STYLE");

    let target = Box::new(File::create("./log.txt").expect("Can't create file"));

    Builder::new()
        .format(|buf, record| {
            writeln!(
                buf,
                "{}:{} [{}] - {}",
                record.file().unwrap_or("unknown"),
                record.line().unwrap_or(0),
                record.level(),
                record.args()
            )
        })
        .parse_env(env)
        .target(env_logger::Target::Pipe(target))
        .filter_level(LevelFilter::Debug)
        .init();

    let boot_rom =
        buffer_from_file("/home/jord/projects/gbe/roms/dmg_boot.bin").expect("boot_rom rom");

    let game_rom =
        buffer_from_file("/home/jord/projects/gbe/roms/pokemon_blue.gb").expect("invalid rom");

    let mem = Mem::new(&boot_rom, &game_rom);
    let emu = Emulator::new(mem);

    // run_sdl();

    emu.run();
}

fn buffer_from_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
