///TAC: Timer control
///
///Memory address 0xFF07
///
///First 5 bits are not used.
///
///bit 6 indicates whether it is enabled or not.
///
///bit 7 and 8 indicate the Clock select.
///
///Clock select:
///00:
///Increment every 256 M-cycles
///Speed: 4_096
///
///01:
///Increment every 4 M-cycles
///Speed: 262_144
///
///10:
///Increment every 16 M-cycles
///Speed: 65_536
///
///11:
///Increment every 64 M-cycles
///Speed: 16_384

const BASE_CLOCK_SPEED: usize = 4_194_304;

#[derive(Debug)]
enum Frequency {
    Zero,
    One,
    Two,
    Three,
}

impl Frequency {
    fn herz(&self) -> usize {
        match self {
            Frequency::Zero => 4_096,
            Frequency::One => 262_144,
            Frequency::Two => 65_536,
            Frequency::Three => 16_384,
        }
    }
}

#[derive(Debug)]
pub struct Timer {
    cycles: usize,
    counter: u8,
    modulo: u8,
    tac: bool,
    frequency: Frequency,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            cycles: 0,                  // FF04 - Divider Register
            counter: 0,                 // FF05 - Timer Counter (TIMA register)
            modulo: 0,                  // FF06 - Timer Modulo
            tac: false,                 // FF07 on/off
            frequency: Frequency::Zero, // FF07 mode
        }
    }

    pub fn step(&mut self, cycle_increment: u8) -> bool {
        // to always increment, or to not always increment... that is the...
        self.cycles = self.cycles.wrapping_add(cycle_increment as usize);

        if !self.tac {
            return false;
        }

        // number of cycles = base clock speed / current clock speed
        let cycles_per_increment = BASE_CLOCK_SPEED / self.frequency.herz();

        // Not enough cycles have passed for an interrupt
        if self.cycles < cycles_per_increment {
            return false;
        }

        // Extra cycles go wasted
        self.cycles -= cycles_per_increment;

        // Increment the counter (TIMA) and check for overflow.
        let (new_counter, overflowed) = self.counter.overflowing_add(1);

        if overflowed {
            // This has to be set to the modulo value
            self.counter = self.modulo;
            //and signal an interrupt if overflowed.
            return true;
        }

        // Not yet time to interrupt
        self.counter = new_counter;
        false
    }
}
