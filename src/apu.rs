#![allow(clippy::upper_case_acronyms)]

use crate::mem::{Mem, MemCtx};

#[derive(Debug, Default)]
struct ChannelOne {
    ///
    /// NR10 Channel 1 Sweep - Address: 0xFF10
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7
    /// Bit 6 - Pace                Interval of sweep iterations.
    /// Bit 5 - Pace
    /// Bit 4 - Pace
    /// Bit 3 - Direction           (0=Addition, 1=Subsraction)
    /// Bit 2 - Individual Step     Compute L
    /// Bit 1 - Individual Step
    /// Bit 0 - Individual Step
    ///
    ///
    /// 0     - Volume level is 1 (quiet)
    /// 7     - Volume level is 8 (no reduction)
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    sweep: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 11 Channel 1 Length Timer & Duty Cycle - Address: 0xFF11
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Wave Duty
    /// Bit 6 - Wave Duty
    /// Bit 5 - Initial Length Timer
    /// Bit 4 - Initial Length Timer
    /// Bit 3 - Initial Length Timer
    /// Bit 2 - Initial Length Timer
    /// Bit 1 - Initial Length Timer
    /// Bit 0 - Initial Length Timer
    ///
    /// Wave Duty translates to a Duty Cycle percentage. This is the
    /// total amount of time the Wave Form is spent in the `On` state.
    ///
    /// 00  -  12.5 %
    /// 01  -  25 %
    /// 10  -  50 %
    /// 11  -  75 %
    ///
    /// The Initial Length Timer is a value that determines how fast the channel is cut.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    length_timer_and_duty_cycle: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 12 Channel 1 Volume & Envelope - Address: 0xFF12
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Initial Volume
    /// Bit 6 - Initial Volume
    /// Bit 5 - Initial Volume
    /// Bit 4 - Initial Volume
    /// Bit 3 - Env Dir                 (0=Decrease Over Time, 1= Increase Over Time)
    /// Bit 2 - Sweep pace
    /// Bit 1 - Sweep pace
    /// Bit 0 - Sweep pace
    ///
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    volume_and_envelope: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 13 Channel 1 Period Low - Address: 0xFF13
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Period
    /// Bit 6 - Period
    /// Bit 5 - Period
    /// Bit 4 - Period
    /// Bit 3 - Period
    /// Bit 2 - Period
    /// Bit 1 - Period
    /// Bit 0 - Period
    /// Lower 8 bits of the period value. The upper 3 bits are stored in the low 3 bits
    /// of NR14.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    period_low: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 14 Channel 1 Period High & Control - Address: 0xFF14
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Trigger
    /// Bit 6 - Length Enable
    /// Bit 5
    /// Bit 4
    /// Bit 3
    /// Bit 2 - Period
    /// Bit 1 - Period
    /// Bit 0 - Period
    ///
    /// Upper 3 bits of the period value. The lower 8 bits are stored in NR14
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    period_high_and_control: u8,
}

/// Identical to Channel 1
///
/// Map as folows:
/// NR21 (0xFF16) -> NR11
/// NR22 (0xFF17) -> NR12
/// NR23 (0xFF18) -> NR13
/// NR24 (0xFF19) -> NR14
#[derive(Debug, Default)]
struct ChannelTwo {
    length_timer_and_duty_cycle: u8, // NR 21
    volume_and_envelope: u8,         // NR 22
    period_low: u8,                  // NR 23
    period_high_and_control: u8,     // NR 24
}

#[derive(Debug, Default)]
struct ChannelThree {
    /// NR 30 Channel 3 DAC Enable - Address: 0xFF1A
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - DAC On/Off          (1=On, 0=Off)
    /// Bit 6
    /// Bit 5
    /// Bit 4
    /// Bit 3
    /// Bit 2
    /// Bit 1
    /// Bit 0
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    dac_enable: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 31 Channel 3 Length Timer - Address: 0xFF1B
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Initial Length Timer
    /// Bit 6 - Initial Length Timer
    /// Bit 5 - Initial Length Timer
    /// Bit 4 - Initial Length Timer
    /// Bit 3 - Initial Length Timer
    /// Bit 2 - Initial Length Timer
    /// Bit 1 - Initial Length Timer
    /// Bit 0 - Initial Length Timer
    ///
    /// The Initial Length Timer is a value that determines how fast the channel is cut.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    length_timer: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 32 Channel 3 Output Level - Address: 0xFF1C
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7
    /// Bit 6 - Output Level
    /// Bit 5 - Output Level
    /// Bit 4
    /// Bit 3
    /// Bit 2
    /// Bit 1
    /// Bit 0
    ///
    ///
    /// 00  -  Mute
    /// 01  -  100 % Volume
    /// 10  -  50 % Volume
    /// 11  -  25 % Volume
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    output_level: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 33 Channel 3 Period Low - Address: 0xFF1D
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Lower 8 bits of this channel's period value.
    /// Upper 3 bits are stored in the low 3 bits of NR34.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    period_low: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 33 Channel 3 Period High - Address: 0xFF1E
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Trigger
    /// Bit 6 - Length Enable
    /// Bit 5
    /// Bit 4
    /// Bit 3
    /// Bit 2 - Period
    /// Bit 1 - Period
    /// Bit 0 - Period
    ///
    ///
    /// Lower 8 bits are stored in NR34.
    /// Upper 3 bits of this channel's period value.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    period_high_and_control: u8,
    /// ---------------------------------------------------------
    ///
    /// Wave Patterm RAM - Address: 0xFF30 - 0xFF3F
    ///
    /// Reads RAM from left to right:
    /// 0xFF30 - Upper Nibble
    /// 0xFF30 - Lower Nibble
    /// 0xFF31 - Upper Nibble
    /// ...
    /// 0xFF3F - Lower Nibble
    ///
    /// Each bytes holds two sambles, each 4 bits.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    wave_ram: [u8; 16],
}

#[derive(Debug, Default)]
struct ChannelFour {
    /// NR 41 Channel 4 Length Timer - Address: 0xFF20
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7
    /// Bit 6
    /// Bit 5 - Initial Length Timer
    /// Bit 4 - Initial Length Timer
    /// Bit 3 - Initial Length Timer
    /// Bit 2 - Initial Length Timer
    /// Bit 1 - Initial Length Timer
    /// Bit 0 - Initial Length Timer
    ///
    ///
    /// The Initial Length Timer is a value that determines how fast the channel is cut.
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    ///
    length_timer: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 42 Channel 4 Volume & Envelope- Address: 0xFF21
    ///
    /// Identical to NR 12.
    volume_and_envelope: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 43 Channel 4 Frequency & Randomness- Address: 0xFF22
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Clock Shift
    /// Bit 6 - Clock Shift
    /// Bit 5 - Clock Shift
    /// Bit 4 - Clock Shift
    /// Bit 3 - LFSR Width
    /// Bit 2 - Clock Divider
    /// Bit 1 - Clock Divider
    /// Bit 0 - Clock Divider
    frequency_and_randomness: u8,
    /// ---------------------------------------------------------
    ///
    /// NR 44 Channel 4 Control - Address: 0xFF23
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Trigger
    /// Bit 6 - Length Enable
    /// Bit 5
    /// Bit 4
    /// Bit 3
    /// Bit 2
    /// Bit 1
    /// Bit 0
    control: u8,
}

#[derive(Debug, Default)]
struct APU {
    /// NR52 Audio Master Control - Address: 0xFF26
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - APU On/Off                     (0=Off, 1=On)
    /// Bit 6
    /// Bit 5
    /// Bit 4
    /// Bit 3 - Channel 4 On/Off               (0=Off, 1=On)
    /// Bit 2 - Channel 3 On/Off               (0=Off, 1=On)
    /// Bit 1 - Channel 2 On/Off               (0=Off, 1=On)
    /// Bit 0 - Channel 1 On/Off               (0=Off, 1=On)
    ///
    /// Writing to bit 0, 1, 2 or 3 *should* not turn them on.
    /// To turn on a channel, the 7th bit of that channel should be set
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    audio_master_control: u8,
    /// ---------------------------------------------------------
    ///
    /// NR51 Sound Panning - Address: 0xFF25
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - Channel 4 <-
    /// Bit 6 - Channel 3 <-
    /// Bit 5 - Channel 2 <-
    /// Bit 4 - Channel 1 <-
    /// Bit 3 - Channel 4 ->
    /// Bit 2 - Channel 3 ->
    /// Bit 1 - Channel 2 ->
    /// Bit 0 - Channel 1 ->
    ///
    /// TODO: Implement audio pop
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    sound_panning: u8,
    /// ---------------------------------------------------------
    ///
    /// NR50 Master Volume & VIN Panning - Address: 0xFF24
    ///
    /// ┌───┬───┬───┬───┬───┬───┬───┬───┐
    /// │ 7 │ 6 │ 5 │ 4 │ 3 │ 2 │ 1 │ 0 │  Bit Index
    /// └───┴───┴───┴───┴───┴───┴───┴───┘
    ///
    /// Bit 7 - VIN Left     - Set to 0 if extenal hardware is not being used
    /// Bit 6 - Left Volume  - Controls the master volume
    /// Bit 5 - Left Volume
    /// Bit 4 - Left Volume
    /// Bit 3 - VIN Right
    /// Bit 2 - Right Volume
    /// Bit 1 - Right Volume
    /// Bit 0 - Right Volume
    ///
    ///
    /// 0     - Volume level is 1 (quiet)
    /// 7     - Volume level is 8 (no reduction)
    ///
    /// https://gbdev.io/pandocs/Audio_Registers.html
    master_volume_and_vin_panning: u8,

    channel_one: ChannelOne,
    channel_two: ChannelTwo,
    channel_three: ChannelThree,
    channel_four: ChannelFour,
}

impl APU {
    fn read_flags(&mut self, mem: &Mem) {
        // Global
        self.audio_master_control = mem.read(0xFF26);
        self.sound_panning = mem.read(0xFF25);
        self.master_volume_and_vin_panning = mem.read(0xFF24);

        // Channel 1
        self.channel_one.sweep = mem.read(0xFF10);
        self.channel_one.length_timer_and_duty_cycle = mem.read(0xFF11);
        self.channel_one.volume_and_envelope = mem.read(0xFF12);
        self.channel_one.period_low = mem.read(0xFF13);
        self.channel_one.period_high_and_control = mem.read(0xFF14);

        // Channel 2
        // No sweep
        self.channel_two.length_timer_and_duty_cycle = mem.read(0xFF16);
        self.channel_two.volume_and_envelope = mem.read(0xFF17);
        self.channel_two.period_low = mem.read(0xFF18);
        self.channel_two.period_high_and_control = mem.read(0xFF19);

        // Channel 3
        self.channel_three.dac_enable = mem.read(0xFF1A);
        self.channel_three.length_timer = mem.read(0xFF1B);
        self.channel_three.output_level = mem.read(0xFF1C);
        self.channel_three.period_low = mem.read(0xFF1D);
        self.channel_three.period_high_and_control = mem.read(0xFF1E);
        self.channel_three.wave_ram = [
            mem.read(0xFF30),
            mem.read(0xFF31),
            mem.read(0xFF32),
            mem.read(0xFF33),
            mem.read(0xFF34),
            mem.read(0xFF35),
            mem.read(0xFF36),
            mem.read(0xFF37),
            mem.read(0xFF38),
            mem.read(0xFF39),
            mem.read(0xFF3A),
            mem.read(0xFF3B),
            mem.read(0xFF3C),
            mem.read(0xFF3D),
            mem.read(0xFF3E),
            mem.read(0xFF3F),
        ];

        // Channel 4
        self.channel_four.length_timer = mem.read(0xFF41);
        self.channel_four.volume_and_envelope = mem.read(0xFF42);
        self.channel_four.frequency_and_randomness = mem.read(0xFF43);
        self.channel_four.control = mem.read(0xFF44);
    }
}
