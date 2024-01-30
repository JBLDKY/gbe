mod cpu;
mod flags_registers;
mod registers;

fn main() {
    let a: u8 = 0b0000_1011;
    let right = a.rotate_right(1);
    println!("{}", format!("{right:b}"));

    // println!(format!("{x:b}"), a.rotate_right(1).as_bytes());
    // dbg!(a.overflowing_sub(b));
    //
    // dbg!(a.wrapping_sub(b));

    // Checking half-carry
    // ((a & 0xf) + (value & 0xf)) & 0x10
    //
    // All of this is just to unset the first 4 bits {
    // Compares the last 4 bits of the current value in the `a` register with 1111
    // It will essentially return a new byte where the last 4 bits are only set
    // if they were also set on register `a`.
    // (a & 0xf)
    //
    // Idem for the new value.
    // (value & 0xf)
    //
    // The next step is to add them together
    // (a & 0xf) + (value & 0xf)
    // }
    //
    // Now we just have the sum of the last 4 bits of the two values.
    //
    // Logically, all we need to do now, if the 5th bit is set on the result. This is simply
    // done by comparing to 0x10. We don't need to worry about the 5th bit not being set
    // because it carries over to the 6th bit as we kept only the last 4 bits of `a` and the value
    // we're adding.
    // e.g.: 0b1111 + 0b1111 = 0b0001 1110 // in the biggest possible addition, we still dont set
    // bit #6
}
