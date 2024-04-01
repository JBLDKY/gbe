mod cpu;
mod flags_registers;
mod instruction;
mod registers;
mod timer;
use crate::cpu::CPU;

use env_logger::{Builder, Env};
use log::info;
use log::LevelFilter;
use std::fs::File;
use std::io::{self, Read, Write};

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

    let rom =
        buffer_from_file("/home/jord/projects/gbe/roms/pokemon_blue.gb").expect("invalid rom");

    let mut cpu = CPU::new(&boot_rom, &rom);
    let mut cycles = 0;

    let mut debug_value = cpu.mem.read(0xFF44);
    loop {
        // count cycles

        cycles += cpu.step() as usize;
        if debug_value != cpu.mem.read(0xFF44) {
            println!("{:4x}", debug_value);
            panic!("{}", cycles);
        };

        // Dont run as if on steroids
        // sleep(Duration::from_nanos(20));
    }
}

fn buffer_from_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
