mod cpu;
mod flags_registers;
mod registers;
mod instruction;
use std::fs::File;
use std::io::{self, Read};

fn main() {
    let a: u8 = 0b1000_1011;
    dbg!(a as i8);

    let right = a.rotate_right(1);
    println!("{}", format!("{right:b}"));

    // bit #6
}

fn buffer_from_file(path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}
