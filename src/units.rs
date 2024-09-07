#[derive(Debug, Clone, Copy)]
pub struct Dots(pub u64);

#[derive(Debug, Clone, Copy)]
pub struct Cycles(pub u64);

#[derive(Debug, Clone, Copy)]
pub struct Hertz(pub f64);

#[derive(Debug, Clone, Copy)]
pub struct MegaHertz(pub f64);

// We only need to implement From in one direction
impl From<Dots> for Cycles {
    fn from(dots: Dots) -> Self {
        Cycles(dots.0 / 4) // 4 dots per cycle
    }
}

impl From<Dots> for Hertz {
    fn from(dots: Dots) -> Self {
        Hertz(4_194_304.0 / dots.0 as f64)
    }
}

impl From<Hertz> for MegaHertz {
    fn from(hz: Hertz) -> Self {
        MegaHertz(hz.0 / 1_000_000.0)
    }
}

impl From<Hertz> for Cycles {
    fn from(hz: Hertz) -> Self {
        Cycles((4_194_304.0 / hz.0) as u64) // GameBoy CPU frequency is 4.194304 MHz
    }
}

// Constants for common GameBoy values
pub const DOTS_PER_FRAME: Dots = Dots(70224);
pub const CYCLES_PER_FRAME: Cycles = Cycles(17556);
pub const FRAME_RATE: Hertz = Hertz(59.7275);
pub const CPU_SPEED: MegaHertz = MegaHertz(4.194304);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    /// One Cycle is Four Dots
    /// Cycle:Dot
    /// 4:1
    fn test_dots_to_cycles_conversion() {
        let dots = Dots(100);
        let cycles: Cycles = dots.into();
        assert_eq!(cycles.0, 25);
    }

    #[test]
    /// One MegaHertz is Four Hertz
    /// MegaHertz:Hertz
    /// 1_000_000:1
    fn test_hertz_to_megahertz_conversion() {
        let hz = Hertz(1_000_000.0);
        let mhz: MegaHertz = hz.into();
        assert!((mhz.0 - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    /// One Hertz is  1/5
    /// MegaHertz:Hertz
    /// 1_000_000:1
    /// 69905 / 50
    fn test_hertz_to_cycles_conversion() {
        let hz = Hertz(60.0);
        let cycles: Cycles = hz.into();
        assert_eq!(cycles.0, 69_905);
    }

    #[test]
    fn test_dots_per_frame_constant() {
        assert_eq!(DOTS_PER_FRAME.0, 70224);
    }

    #[test]
    fn test_cycles_per_frame_constant() {
        assert_eq!(CYCLES_PER_FRAME.0, 17556);
    }

    #[test]
    fn test_frame_rate_constant() {
        assert!((FRAME_RATE.0 - 59.7275).abs() < f64::EPSILON);
    }

    #[test]
    fn test_cpu_speed_constant() {
        assert!((CPU_SPEED.0 - 4.194304).abs() < f64::EPSILON);
    }

    #[test]
    fn test_consistency_between_dots_and_cycles_per_frame() {
        let cycles_from_dots: Cycles = DOTS_PER_FRAME.into();
        assert_eq!(cycles_from_dots.0, CYCLES_PER_FRAME.0);
    }

    #[test]
    fn test_frame_rate_to_cycles_consistency() {
        let cycles_per_frame: Cycles = FRAME_RATE.into();
        assert!((cycles_per_frame.0 as f64 - CYCLES_PER_FRAME.0 as f64).abs() < 1.0);
    }
}
