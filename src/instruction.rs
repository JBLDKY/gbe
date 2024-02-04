#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum Arithmetic16BitTarget {
    HL,
    BC,
    SP,
    DE,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RSTLocation {
    X00,
    X08,
    X10,
    X18,
    X20,
    X28,
    X30,
    X38,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BitPosition {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
}

impl std::convert::From<BitPosition> for u8 {
    fn from(position: BitPosition) -> u8 {
        match position {
            BitPosition::B0 => 0,
            BitPosition::B1 => 1,
            BitPosition::B2 => 2,
            BitPosition::B3 => 3,
            BitPosition::B4 => 4,
            BitPosition::B5 => 5,
            BitPosition::B6 => 6,
            BitPosition::B7 => 7,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrefixTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

impl RSTLocation {
    pub fn to_hex(&self) -> u16 {
        match self {
            RSTLocation::X00 => 0x00,
            RSTLocation::X08 => 0x08,
            RSTLocation::X10 => 0x10,
            RSTLocation::X18 => 0x18,
            RSTLocation::X20 => 0x20,
            RSTLocation::X28 => 0x28,
            RSTLocation::X30 => 0x30,
            RSTLocation::X38 => 0x38,
        }
    }
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum StackTarget {
    HL,
    BC,
    AF,
    DE,
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum JumpCondition {
    NotZero,
    NotCarry,
    Zero,
    Carry,         // C, NN
    Unconditional, // NN (address)
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum AnyTarget {
    A,
    B,
    D,
    H,
    C,
    E,
    L,
    HLI,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum Arithmetic8BitTarget {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    D8,
}

#[allow(dead_code)]
pub enum Instruction {
    ADD(Arithmetic8BitTarget),
    AddHli,
    ADDSPN,
    ADC(Arithmetic8BitTarget),
    AdcHli,
    ADDHL(Arithmetic16BitTarget),
    SUB(Arithmetic8BitTarget),
    SubHli,
    SBC(Arithmetic8BitTarget),
    SbcHli,
    AND(Arithmetic8BitTarget),
    AndHli,
    OR(Arithmetic8BitTarget),
    OrHli,
    XOR(Arithmetic8BitTarget),
    XorHli,
    CP(Arithmetic8BitTarget),
    CpHli,
    INC(AnyTarget),
    DEC(AnyTarget),
    CCF,
    SCF,
    RRA,
    RLA,
    RRCA,
    RLCA,
    CPL,
    BIT(PrefixTarget, BitPosition),
    RESET(PrefixTarget, BitPosition),
    SET(PrefixTarget, BitPosition),
    SRL(PrefixTarget),
    RR(PrefixTarget),
    RL(PrefixTarget),
    RRC(PrefixTarget),
    RLC(PrefixTarget),
    SRA(PrefixTarget),
    SLA(PrefixTarget),
    SWAP(PrefixTarget),
    DAA,
    JP(JumpCondition),
    JR(JumpCondition),
    JpHli,
    PUSH(StackTarget),
    POP(StackTarget),
    CALL(JumpCondition),
    RET(JumpCondition),
    RETI,
    RST(RSTLocation),
    NOP,
    HALT,
    EI,
    DI,
}

#[allow(dead_code)]
impl Instruction {
    fn from_byte(byte: u8) -> Option<Instruction> {
        match byte {
            // Increment
            0x3c => Some(Instruction::INC(AnyTarget::A)),
            0x04 => Some(Instruction::INC(AnyTarget::B)),
            0x14 => Some(Instruction::INC(AnyTarget::D)),
            0x24 => Some(Instruction::INC(AnyTarget::H)),
            0x0c => Some(Instruction::INC(AnyTarget::C)),
            0x1c => Some(Instruction::INC(AnyTarget::E)),
            0x2c => Some(Instruction::INC(AnyTarget::L)),
            0x34 => Some(Instruction::INC(AnyTarget::HLI)),
            0x03 => Some(Instruction::INC(AnyTarget::BC)),
            0x13 => Some(Instruction::INC(AnyTarget::DE)),
            0x23 => Some(Instruction::INC(AnyTarget::HL)),
            0x33 => Some(Instruction::INC(AnyTarget::SP)),

            // Decrement
            0x3d => Some(Instruction::DEC(AnyTarget::A)),
            0x05 => Some(Instruction::DEC(AnyTarget::B)),
            0x0d => Some(Instruction::DEC(AnyTarget::C)),
            0x15 => Some(Instruction::DEC(AnyTarget::D)),
            0x1d => Some(Instruction::DEC(AnyTarget::E)),
            0x25 => Some(Instruction::DEC(AnyTarget::H)),
            0x2d => Some(Instruction::DEC(AnyTarget::L)),
            0x35 => Some(Instruction::DEC(AnyTarget::HLI)),
            0x0b => Some(Instruction::DEC(AnyTarget::BC)),
            0x1b => Some(Instruction::DEC(AnyTarget::DE)),
            0x2b => Some(Instruction::DEC(AnyTarget::HL)),
            0x3b => Some(Instruction::DEC(AnyTarget::SP)),

            // Add
            0x87 => Some(Instruction::ADD(Arithmetic8BitTarget::A)),
            0x80 => Some(Instruction::ADD(Arithmetic8BitTarget::B)),
            0x81 => Some(Instruction::ADD(Arithmetic8BitTarget::C)),
            0x82 => Some(Instruction::ADD(Arithmetic8BitTarget::D)),
            0x83 => Some(Instruction::ADD(Arithmetic8BitTarget::E)),
            0x84 => Some(Instruction::ADD(Arithmetic8BitTarget::H)),
            0x85 => Some(Instruction::ADD(Arithmetic8BitTarget::L)),
            0xc6 => Some(Instruction::ADD(Arithmetic8BitTarget::D8)),

            // Add memory adress in HL to A.
            0x86 => Some(Instruction::AddHli),

            // Add memory adress in HL to A.
            0xe8 => Some(Instruction::ADDSPN),

            // 16 bit add
            0x09 => Some(Instruction::ADDHL(Arithmetic16BitTarget::BC)),
            0x19 => Some(Instruction::ADDHL(Arithmetic16BitTarget::DE)),
            0x29 => Some(Instruction::ADDHL(Arithmetic16BitTarget::HL)),
            0x39 => Some(Instruction::ADDHL(Arithmetic16BitTarget::SP)),

            // Add with carry
            0x8f => Some(Instruction::ADC(Arithmetic8BitTarget::A)),
            0x88 => Some(Instruction::ADC(Arithmetic8BitTarget::B)),
            0x89 => Some(Instruction::ADC(Arithmetic8BitTarget::C)),
            0x8a => Some(Instruction::ADC(Arithmetic8BitTarget::D)),
            0x8b => Some(Instruction::ADC(Arithmetic8BitTarget::E)),
            0x8c => Some(Instruction::ADC(Arithmetic8BitTarget::H)),
            0x8d => Some(Instruction::ADC(Arithmetic8BitTarget::L)),
            0xce => Some(Instruction::ADC(Arithmetic8BitTarget::D8)),

            // Subtract memory address value in HL from a
            0x8e => Some(Instruction::AdcHli),

            // Subtract
            0x97 => Some(Instruction::SUB(Arithmetic8BitTarget::A)),
            0x90 => Some(Instruction::SUB(Arithmetic8BitTarget::B)),
            0x91 => Some(Instruction::SUB(Arithmetic8BitTarget::C)),
            0x92 => Some(Instruction::SUB(Arithmetic8BitTarget::D)),
            0x93 => Some(Instruction::SUB(Arithmetic8BitTarget::E)),
            0x94 => Some(Instruction::SUB(Arithmetic8BitTarget::H)),
            0x95 => Some(Instruction::SUB(Arithmetic8BitTarget::L)),
            0xd6 => Some(Instruction::SUB(Arithmetic8BitTarget::D8)),

            // Subtract memory address value in HL from a
            0x96 => Some(Instruction::SubHli),

            0x9f => Some(Instruction::SBC(Arithmetic8BitTarget::A)),
            0x98 => Some(Instruction::SBC(Arithmetic8BitTarget::B)),
            0x99 => Some(Instruction::SBC(Arithmetic8BitTarget::C)),
            0x9a => Some(Instruction::SBC(Arithmetic8BitTarget::D)),
            0x9b => Some(Instruction::SBC(Arithmetic8BitTarget::E)),
            0x9c => Some(Instruction::SBC(Arithmetic8BitTarget::H)),
            0x9d => Some(Instruction::SBC(Arithmetic8BitTarget::L)),
            0xde => Some(Instruction::SBC(Arithmetic8BitTarget::D8)),

            // Subtract memory address value in HL from a with carry
            0x9e => Some(Instruction::SbcHli),

            0xa7 => Some(Instruction::AND(Arithmetic8BitTarget::A)),
            0xa0 => Some(Instruction::AND(Arithmetic8BitTarget::B)),
            0xa1 => Some(Instruction::AND(Arithmetic8BitTarget::C)),
            0xa2 => Some(Instruction::AND(Arithmetic8BitTarget::D)),
            0xa3 => Some(Instruction::AND(Arithmetic8BitTarget::E)),
            0xa4 => Some(Instruction::AND(Arithmetic8BitTarget::H)),
            0xa5 => Some(Instruction::AND(Arithmetic8BitTarget::L)),
            0xe6 => Some(Instruction::AND(Arithmetic8BitTarget::D8)),

            // Logical AND with memory address value in HL
            0xa6 => Some(Instruction::AndHli),

            0xb7 => Some(Instruction::OR(Arithmetic8BitTarget::A)),
            0xb0 => Some(Instruction::OR(Arithmetic8BitTarget::B)),
            0xb1 => Some(Instruction::OR(Arithmetic8BitTarget::C)),
            0xb2 => Some(Instruction::OR(Arithmetic8BitTarget::D)),
            0xb3 => Some(Instruction::OR(Arithmetic8BitTarget::E)),
            0xb4 => Some(Instruction::OR(Arithmetic8BitTarget::H)),
            0xb5 => Some(Instruction::OR(Arithmetic8BitTarget::L)),
            0xf6 => Some(Instruction::OR(Arithmetic8BitTarget::D8)),

            // Logical OR with memory address value in HL
            0xb6 => Some(Instruction::OrHli),

            0xaf => Some(Instruction::XOR(Arithmetic8BitTarget::A)),
            0xa8 => Some(Instruction::XOR(Arithmetic8BitTarget::B)),
            0xa9 => Some(Instruction::XOR(Arithmetic8BitTarget::C)),
            0xaa => Some(Instruction::XOR(Arithmetic8BitTarget::D)),
            0xab => Some(Instruction::XOR(Arithmetic8BitTarget::E)),
            0xac => Some(Instruction::XOR(Arithmetic8BitTarget::H)),
            0xad => Some(Instruction::XOR(Arithmetic8BitTarget::L)),
            0xee => Some(Instruction::XOR(Arithmetic8BitTarget::D8)),

            // Logical XOR with memory address value in HL
            0xae => Some(Instruction::XorHli),

            // Compare values
            0xbf => Some(Instruction::CP(Arithmetic8BitTarget::A)),
            0xb8 => Some(Instruction::CP(Arithmetic8BitTarget::B)),
            0xb9 => Some(Instruction::CP(Arithmetic8BitTarget::C)),
            0xba => Some(Instruction::CP(Arithmetic8BitTarget::D)),
            0xbb => Some(Instruction::CP(Arithmetic8BitTarget::E)),
            0xbc => Some(Instruction::CP(Arithmetic8BitTarget::H)),
            0xbd => Some(Instruction::CP(Arithmetic8BitTarget::L)),
            0xfe => Some(Instruction::CP(Arithmetic8BitTarget::D8)),

            // Compare values with HLI
            0xbe => Some(Instruction::CpHli),

            // Straightforward rotations of specific registers
            0x3f => Some(Instruction::CCF),
            0x37 => Some(Instruction::SCF),
            0x1f => Some(Instruction::RRA),
            0x17 => Some(Instruction::RLA),
            0x0f => Some(Instruction::RRCA),
            0x07 => Some(Instruction::RLCA),
            0x2f => Some(Instruction::CPL),

            // Weird BCA Test that compares a to 9 and 154 (?)
            0x27 => Some(Instruction::DAA),

            // Jump to the memory address stored in pc + 1 | pc + 2
            0xc3 => Some(Instruction::JP(JumpCondition::Unconditional)),
            0xc2 => Some(Instruction::JP(JumpCondition::NotZero)),
            0xd2 => Some(Instruction::JP(JumpCondition::NotCarry)),
            0xca => Some(Instruction::JP(JumpCondition::Zero)),
            0xda => Some(Instruction::JP(JumpCondition::Carry)),

            // Jump to the memory address relative to the current one.
            0x18 => Some(Instruction::JR(JumpCondition::Unconditional)),
            0x28 => Some(Instruction::JR(JumpCondition::Zero)),
            0x38 => Some(Instruction::JR(JumpCondition::Carry)),
            0x20 => Some(Instruction::JR(JumpCondition::NotZero)),
            0x30 => Some(Instruction::JR(JumpCondition::NotCarry)),

            // Jump to the memory address relative to the current one.
            0xe9 => Some(Instruction::JpHli),

            // 0xf2 => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::LastByteIndirect,
            // ))),
            // 0x0a => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::BCIndirect,
            // ))),
            // 0x1a => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::DEIndirect,
            // ))),
            // 0x2a => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::HLIndirectPlus,
            // ))),
            // 0x3a => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::HLIndirectMinus,
            // ))),
            // 0xfa => Some(Instruction::LD(LoadType::AFromIndirect(
            //     Indirect::WordIndirect,
            // ))),
            //
            // 0xe2 => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::LastByteIndirect,
            // ))),
            // 0x02 => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::BCIndirect,
            // ))),
            // 0x12 => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::DEIndirect,
            // ))),
            // 0x22 => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::HLIndirectPlus,
            // ))),
            // 0x32 => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::HLIndirectMinus,
            // ))),
            // 0xea => Some(Instruction::LD(LoadType::IndirectFromA(
            //     Indirect::WordIndirect,
            // ))),
            //
            // 0x01 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::BC))),
            // 0x11 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::DE))),
            // 0x21 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::HL))),
            // 0x31 => Some(Instruction::LD(LoadType::Word(LoadWordTarget::SP))),
            //
            // 0x40 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::B,
            // ))),
            // 0x41 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::C,
            // ))),
            // 0x42 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::D,
            // ))),
            // 0x43 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::E,
            // ))),
            // 0x44 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::H,
            // ))),
            // 0x45 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::L,
            // ))),
            // 0x46 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::HLI,
            // ))),
            // 0x47 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x48 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::B,
            // ))),
            // 0x49 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::C,
            // ))),
            // 0x4a => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::D,
            // ))),
            // 0x4b => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::E,
            // ))),
            // 0x4c => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::H,
            // ))),
            // 0x4d => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::L,
            // ))),
            // 0x4e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::HLI,
            // ))),
            // 0x4f => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x50 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::B,
            // ))),
            // 0x51 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::C,
            // ))),
            // 0x52 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::D,
            // ))),
            // 0x53 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::E,
            // ))),
            // 0x54 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::H,
            // ))),
            // 0x55 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::L,
            // ))),
            // 0x56 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::HLI,
            // ))),
            // 0x57 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x58 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::B,
            // ))),
            // 0x59 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::C,
            // ))),
            // 0x5a => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::D,
            // ))),
            // 0x5b => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::E,
            // ))),
            // 0x5c => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::H,
            // ))),
            // 0x5d => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::L,
            // ))),
            // 0x5e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::HLI,
            // ))),
            // 0x5f => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x60 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::B,
            // ))),
            // 0x61 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::C,
            // ))),
            // 0x62 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::D,
            // ))),
            // 0x63 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::E,
            // ))),
            // 0x64 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::H,
            // ))),
            // 0x65 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::L,
            // ))),
            // 0x66 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::HLI,
            // ))),
            // 0x67 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x68 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::B,
            // ))),
            // 0x69 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::C,
            // ))),
            // 0x6a => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::D,
            // ))),
            // 0x6b => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::E,
            // ))),
            // 0x6c => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::H,
            // ))),
            // 0x6d => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::L,
            // ))),
            // 0x6e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::HLI,
            // ))),
            // 0x6f => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x70 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::B,
            // ))),
            // 0x71 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::C,
            // ))),
            // 0x72 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::D,
            // ))),
            // 0x73 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::E,
            // ))),
            // 0x74 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::H,
            // ))),
            // 0x75 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::L,
            // ))),
            // 0x77 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x78 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::B,
            // ))),
            // 0x79 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::C,
            // ))),
            // 0x7a => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::D,
            // ))),
            // 0x7b => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::E,
            // ))),
            // 0x7c => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::H,
            // ))),
            // 0x7d => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::L,
            // ))),
            // 0x7e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::HLI,
            // ))),
            // 0x7f => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::A,
            // ))),
            //
            // 0x3e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::A,
            //     LoadByteSource::D8,
            // ))),
            // 0x06 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::B,
            //     LoadByteSource::D8,
            // ))),
            // 0x0e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::C,
            //     LoadByteSource::D8,
            // ))),
            // 0x16 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::D,
            //     LoadByteSource::D8,
            // ))),
            // 0x1e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::E,
            //     LoadByteSource::D8,
            // ))),
            // 0x26 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::H,
            //     LoadByteSource::D8,
            // ))),
            // 0x2e => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::L,
            //     LoadByteSource::D8,
            // ))),
            // 0x36 => Some(Instruction::LD(LoadType::Byte(
            //     LoadByteTarget::HLI,
            //     LoadByteSource::D8,
            // ))),

            // 0xe0 => Some(Instruction::LD(LoadType::ByteAddressFromA)),
            // 0xf0 => Some(Instruction::LD(LoadType::AFromByteAddress)),

            // 0x08 => Some(Instruction::LD(LoadType::IndirectFromSP)),
            // 0xf9 => Some(Instruction::LD(LoadType::SPFromHL)),
            // 0xf8 => Some(Instruction::LD(LoadType::HLFromSPN)),

            // Push one of the 16 bit registers onto the stack.
            0xc5 => Some(Instruction::PUSH(StackTarget::BC)),
            0xd5 => Some(Instruction::PUSH(StackTarget::DE)),
            0xe5 => Some(Instruction::PUSH(StackTarget::HL)),
            0xf5 => Some(Instruction::PUSH(StackTarget::AF)),

            // Pop one of the 16 bit registers onto the stack.
            0xc1 => Some(Instruction::POP(StackTarget::BC)),
            0xd1 => Some(Instruction::POP(StackTarget::DE)),
            0xe1 => Some(Instruction::POP(StackTarget::HL)),
            0xf1 => Some(Instruction::POP(StackTarget::AF)),

            // Push but with conditions.
            0xc4 => Some(Instruction::CALL(JumpCondition::NotZero)),
            0xd4 => Some(Instruction::CALL(JumpCondition::NotCarry)),
            0xcc => Some(Instruction::CALL(JumpCondition::Zero)),
            0xdc => Some(Instruction::CALL(JumpCondition::Carry)),
            0xcd => Some(Instruction::CALL(JumpCondition::Unconditional)),

            // Pop the top of the stack into the program counter (Also known as returning)
            0xc0 => Some(Instruction::RET(JumpCondition::NotZero)),
            0xd0 => Some(Instruction::RET(JumpCondition::NotCarry)),
            0xc8 => Some(Instruction::RET(JumpCondition::Zero)),
            0xd8 => Some(Instruction::RET(JumpCondition::Carry)),
            0xc9 => Some(Instruction::RET(JumpCondition::Unconditional)),

            // Return from interrupt and re-enable interrupts
            0xd9 => Some(Instruction::RETI),

            // Call the fixed address indicated by the RST location
            0xc7 => Some(Instruction::RST(RSTLocation::X00)),
            0xd7 => Some(Instruction::RST(RSTLocation::X10)),
            0xe7 => Some(Instruction::RST(RSTLocation::X20)),
            0xf7 => Some(Instruction::RST(RSTLocation::X30)),
            0xcf => Some(Instruction::RST(RSTLocation::X08)),
            0xdf => Some(Instruction::RST(RSTLocation::X18)),
            0xef => Some(Instruction::RST(RSTLocation::X28)),
            0xff => Some(Instruction::RST(RSTLocation::X38)),

            // (No Operation): A placeholder instruction that does nothing and is typically used for timing adjustments or as a placeholder for future code.
            0x00 => Some(Instruction::NOP),

            // Halts the CPU until an interrupt occurs. It's used to reduce power consumption and CPU activity until it's necessary to respond to an interrupt.
            0x76 => Some(Instruction::HALT),

            // (Disable Interrupts): Disables all interrupts, preventing the CPU from handling interrupt requests until they are re-enabled.
            0xf3 => Some(Instruction::DI),

            // (Enable Interrupts): Enables interrupts, allowing the CPU to respond to interrupt requests.
            0xfb => Some(Instruction::EI),
            _ => None,
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Instruction::RLC(PrefixTarget::B)),
            0x01 => Some(Instruction::RLC(PrefixTarget::C)),
            0x02 => Some(Instruction::RLC(PrefixTarget::D)),
            0x03 => Some(Instruction::RLC(PrefixTarget::E)),
            0x04 => Some(Instruction::RLC(PrefixTarget::H)),
            0x05 => Some(Instruction::RLC(PrefixTarget::L)),
            0x06 => Some(Instruction::RLC(PrefixTarget::HLI)),
            0x07 => Some(Instruction::RLC(PrefixTarget::A)),

            0x08 => Some(Instruction::RRC(PrefixTarget::B)),
            0x09 => Some(Instruction::RRC(PrefixTarget::C)),
            0x0a => Some(Instruction::RRC(PrefixTarget::D)),
            0x0b => Some(Instruction::RRC(PrefixTarget::E)),
            0x0c => Some(Instruction::RRC(PrefixTarget::H)),
            0x0d => Some(Instruction::RRC(PrefixTarget::L)),
            0x0e => Some(Instruction::RRC(PrefixTarget::HLI)),
            0x0f => Some(Instruction::RRC(PrefixTarget::A)),

            0x10 => Some(Instruction::RL(PrefixTarget::B)),
            0x11 => Some(Instruction::RL(PrefixTarget::C)),
            0x12 => Some(Instruction::RL(PrefixTarget::D)),
            0x13 => Some(Instruction::RL(PrefixTarget::E)),
            0x14 => Some(Instruction::RL(PrefixTarget::H)),
            0x15 => Some(Instruction::RL(PrefixTarget::L)),
            0x16 => Some(Instruction::RL(PrefixTarget::HLI)),
            0x17 => Some(Instruction::RL(PrefixTarget::A)),

            0x18 => Some(Instruction::RR(PrefixTarget::B)),
            0x19 => Some(Instruction::RR(PrefixTarget::C)),
            0x1a => Some(Instruction::RR(PrefixTarget::D)),
            0x1b => Some(Instruction::RR(PrefixTarget::E)),
            0x1c => Some(Instruction::RR(PrefixTarget::H)),
            0x1d => Some(Instruction::RR(PrefixTarget::L)),
            0x1e => Some(Instruction::RR(PrefixTarget::HLI)),
            0x1f => Some(Instruction::RR(PrefixTarget::A)),

            0x20 => Some(Instruction::SLA(PrefixTarget::B)),
            0x21 => Some(Instruction::SLA(PrefixTarget::C)),
            0x22 => Some(Instruction::SLA(PrefixTarget::D)),
            0x23 => Some(Instruction::SLA(PrefixTarget::E)),
            0x24 => Some(Instruction::SLA(PrefixTarget::H)),
            0x25 => Some(Instruction::SLA(PrefixTarget::L)),
            0x26 => Some(Instruction::SLA(PrefixTarget::HLI)),
            0x27 => Some(Instruction::SLA(PrefixTarget::A)),

            0x28 => Some(Instruction::SRA(PrefixTarget::B)),
            0x29 => Some(Instruction::SRA(PrefixTarget::C)),
            0x2a => Some(Instruction::SRA(PrefixTarget::D)),
            0x2b => Some(Instruction::SRA(PrefixTarget::E)),
            0x2c => Some(Instruction::SRA(PrefixTarget::H)),
            0x2d => Some(Instruction::SRA(PrefixTarget::L)),
            0x2e => Some(Instruction::SRA(PrefixTarget::HLI)),
            0x2f => Some(Instruction::SRA(PrefixTarget::A)),

            0x30 => Some(Instruction::SWAP(PrefixTarget::B)),
            0x31 => Some(Instruction::SWAP(PrefixTarget::C)),
            0x32 => Some(Instruction::SWAP(PrefixTarget::D)),
            0x33 => Some(Instruction::SWAP(PrefixTarget::E)),
            0x34 => Some(Instruction::SWAP(PrefixTarget::H)),
            0x35 => Some(Instruction::SWAP(PrefixTarget::L)),
            0x36 => Some(Instruction::SWAP(PrefixTarget::HLI)),
            0x37 => Some(Instruction::SWAP(PrefixTarget::A)),

            0x38 => Some(Instruction::SRL(PrefixTarget::B)),
            0x39 => Some(Instruction::SRL(PrefixTarget::C)),
            0x3a => Some(Instruction::SRL(PrefixTarget::D)),
            0x3b => Some(Instruction::SRL(PrefixTarget::E)),
            0x3c => Some(Instruction::SRL(PrefixTarget::H)),
            0x3d => Some(Instruction::SRL(PrefixTarget::L)),
            0x3e => Some(Instruction::SRL(PrefixTarget::HLI)),
            0x3f => Some(Instruction::SRL(PrefixTarget::A)),

            0x40 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B0)),
            0x41 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B0)),
            0x42 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B0)),
            0x43 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B0)),
            0x44 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B0)),
            0x45 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B0)),
            0x46 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B0)),
            0x47 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B0)),

            0x48 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B1)),
            0x49 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B1)),
            0x4a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B1)),
            0x4b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B1)),
            0x4c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B1)),
            0x4d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B1)),
            0x4e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B1)),
            0x4f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B1)),

            0x50 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B2)),
            0x51 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B2)),
            0x52 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B2)),
            0x53 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B2)),
            0x54 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B2)),
            0x55 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B2)),
            0x56 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B2)),
            0x57 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B2)),

            0x58 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B3)),
            0x59 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B3)),
            0x5a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B3)),
            0x5b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B3)),
            0x5c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B3)),
            0x5d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B3)),
            0x5e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B3)),
            0x5f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B3)),

            0x60 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B4)),
            0x61 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B4)),
            0x62 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B4)),
            0x63 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B4)),
            0x64 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B4)),
            0x65 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B4)),
            0x66 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B4)),
            0x67 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B4)),

            0x68 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B5)),
            0x69 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B5)),
            0x6a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B5)),
            0x6b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B5)),
            0x6c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B5)),
            0x6d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B5)),
            0x6e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B5)),
            0x6f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B5)),

            0x70 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B6)),
            0x71 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B6)),
            0x72 => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B6)),
            0x73 => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B6)),
            0x74 => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B6)),
            0x75 => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B6)),
            0x76 => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B6)),
            0x77 => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B6)),

            0x78 => Some(Instruction::BIT(PrefixTarget::B, BitPosition::B7)),
            0x79 => Some(Instruction::BIT(PrefixTarget::C, BitPosition::B7)),
            0x7a => Some(Instruction::BIT(PrefixTarget::D, BitPosition::B7)),
            0x7b => Some(Instruction::BIT(PrefixTarget::E, BitPosition::B7)),
            0x7c => Some(Instruction::BIT(PrefixTarget::H, BitPosition::B7)),
            0x7d => Some(Instruction::BIT(PrefixTarget::L, BitPosition::B7)),
            0x7e => Some(Instruction::BIT(PrefixTarget::HLI, BitPosition::B7)),
            0x7f => Some(Instruction::BIT(PrefixTarget::A, BitPosition::B7)),

            0x80 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B0)),
            0x81 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B0)),
            0x82 => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B0)),
            0x83 => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B0)),
            0x84 => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B0)),
            0x85 => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B0)),
            0x86 => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B0)),
            0x87 => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B0)),

            0x88 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B1)),
            0x89 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B1)),
            0x8a => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B1)),
            0x8b => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B1)),
            0x8c => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B1)),
            0x8d => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B1)),
            0x8e => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B1)),
            0x8f => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B1)),

            0x90 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B2)),
            0x91 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B2)),
            0x92 => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B2)),
            0x93 => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B2)),
            0x94 => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B2)),
            0x95 => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B2)),
            0x96 => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B2)),
            0x97 => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B2)),

            0x98 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B3)),
            0x99 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B3)),
            0x9a => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B3)),
            0x9b => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B3)),
            0x9c => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B3)),
            0x9d => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B3)),
            0x9e => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B3)),
            0x9f => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B3)),

            0xa0 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B4)),
            0xa1 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B4)),
            0xa2 => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B4)),
            0xa3 => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B4)),
            0xa4 => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B4)),
            0xa5 => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B4)),
            0xa6 => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B4)),
            0xa7 => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B4)),

            0xa8 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B5)),
            0xa9 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B5)),
            0xaa => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B5)),
            0xab => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B5)),
            0xac => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B5)),
            0xad => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B5)),
            0xae => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B5)),
            0xaf => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B5)),

            0xb0 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B6)),
            0xb1 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B6)),
            0xb2 => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B6)),
            0xb3 => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B6)),
            0xb4 => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B6)),
            0xb5 => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B6)),
            0xb6 => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B6)),
            0xb7 => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B6)),

            0xb8 => Some(Instruction::RESET(PrefixTarget::B, BitPosition::B7)),
            0xb9 => Some(Instruction::RESET(PrefixTarget::C, BitPosition::B7)),
            0xba => Some(Instruction::RESET(PrefixTarget::D, BitPosition::B7)),
            0xbb => Some(Instruction::RESET(PrefixTarget::E, BitPosition::B7)),
            0xbc => Some(Instruction::RESET(PrefixTarget::H, BitPosition::B7)),
            0xbd => Some(Instruction::RESET(PrefixTarget::L, BitPosition::B7)),
            0xbe => Some(Instruction::RESET(PrefixTarget::HLI, BitPosition::B7)),
            0xbf => Some(Instruction::RESET(PrefixTarget::A, BitPosition::B7)),

            0xc0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B0)),
            0xc1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B0)),
            0xc2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B0)),
            0xc3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B0)),
            0xc4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B0)),
            0xc5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B0)),
            0xc6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B0)),
            0xc7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B0)),

            0xc8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B1)),
            0xc9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B1)),
            0xca => Some(Instruction::SET(PrefixTarget::D, BitPosition::B1)),
            0xcb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B1)),
            0xcc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B1)),
            0xcd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B1)),
            0xce => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B1)),
            0xcf => Some(Instruction::SET(PrefixTarget::A, BitPosition::B1)),

            0xd0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B2)),
            0xd1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B2)),
            0xd2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B2)),
            0xd3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B2)),
            0xd4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B2)),
            0xd5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B2)),
            0xd6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B2)),
            0xd7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B2)),

            0xd8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B3)),
            0xd9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B3)),
            0xda => Some(Instruction::SET(PrefixTarget::D, BitPosition::B3)),
            0xdb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B3)),
            0xdc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B3)),
            0xdd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B3)),
            0xde => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B3)),
            0xdf => Some(Instruction::SET(PrefixTarget::A, BitPosition::B3)),

            0xe0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B4)),
            0xe1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B4)),
            0xe2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B4)),
            0xe3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B4)),
            0xe4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B4)),
            0xe5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B4)),
            0xe6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B4)),
            0xe7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B4)),

            0xe8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B5)),
            0xe9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B5)),
            0xea => Some(Instruction::SET(PrefixTarget::D, BitPosition::B5)),
            0xeb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B5)),
            0xec => Some(Instruction::SET(PrefixTarget::H, BitPosition::B5)),
            0xed => Some(Instruction::SET(PrefixTarget::L, BitPosition::B5)),
            0xee => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B5)),
            0xef => Some(Instruction::SET(PrefixTarget::A, BitPosition::B5)),

            0xf0 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B6)),
            0xf1 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B6)),
            0xf2 => Some(Instruction::SET(PrefixTarget::D, BitPosition::B6)),
            0xf3 => Some(Instruction::SET(PrefixTarget::E, BitPosition::B6)),
            0xf4 => Some(Instruction::SET(PrefixTarget::H, BitPosition::B6)),
            0xf5 => Some(Instruction::SET(PrefixTarget::L, BitPosition::B6)),
            0xf6 => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B6)),
            0xf7 => Some(Instruction::SET(PrefixTarget::A, BitPosition::B6)),

            0xf8 => Some(Instruction::SET(PrefixTarget::B, BitPosition::B7)),
            0xf9 => Some(Instruction::SET(PrefixTarget::C, BitPosition::B7)),
            0xfa => Some(Instruction::SET(PrefixTarget::D, BitPosition::B7)),
            0xfb => Some(Instruction::SET(PrefixTarget::E, BitPosition::B7)),
            0xfc => Some(Instruction::SET(PrefixTarget::H, BitPosition::B7)),
            0xfd => Some(Instruction::SET(PrefixTarget::L, BitPosition::B7)),
            0xfe => Some(Instruction::SET(PrefixTarget::HLI, BitPosition::B7)),
            0xff => Some(Instruction::SET(PrefixTarget::A, BitPosition::B7)),
        }
    }
}
