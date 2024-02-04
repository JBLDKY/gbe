#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum Arithmetic16BitTarget {
    HL,
    BC,
    SP,
    DE,
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
    BIT(Arithmetic8BitTarget, u8),
    RESET(Arithmetic8BitTarget, u8),
    SET(Arithmetic8BitTarget, u8),
    SRL(Arithmetic8BitTarget),
    RR(Arithmetic8BitTarget),
    RL(Arithmetic8BitTarget),
    RRC(Arithmetic8BitTarget),
    RLC(Arithmetic8BitTarget),
    SRA(Arithmetic8BitTarget),
    SLA(Arithmetic8BitTarget),
    SWAP(Arithmetic8BitTarget),
    DAA,
    JP(JumpCondition),
    JR(JumpCondition),
    JpHli,
    PUSH(StackTarget),
    POP(StackTarget),
    CALL(JumpCondition),
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

            0xc5 => Some(Instruction::CALL(JumpCondition::NotZero)),
            0xd4 => Some(Instruction::CALL(JumpCondition::NotCarry)),
            0xcc => Some(Instruction::CALL(JumpCondition::Zero)),
            0xdc => Some(Instruction::CALL(JumpCondition::Carry)),
            0xcd => Some(Instruction::CALL(JumpCondition::Unconditional)),

            // 0xc0 => Some(Instruction::RET(JumpTest::NotZero)),
            // 0xd0 => Some(Instruction::RET(JumpTest::NotCarry)),
            // 0xc8 => Some(Instruction::RET(JumpTest::Zero)),
            // 0xd8 => Some(Instruction::RET(JumpTest::Carry)),
            // 0xc9 => Some(Instruction::RET(JumpTest::Always)),
            // 0xd9 => Some(Instruction::RETI),

            // 0xc7 => Some(Instruction::RST(RSTLocation::X00)),
            // 0xd7 => Some(Instruction::RST(RSTLocation::X10)),
            // 0xe7 => Some(Instruction::RST(RSTLocation::X20)),
            // 0xf7 => Some(Instruction::RST(RSTLocation::X30)),
            // 0xcf => Some(Instruction::RST(RSTLocation::X08)),
            // 0xdf => Some(Instruction::RST(RSTLocation::X18)),
            // 0xef => Some(Instruction::RST(RSTLocation::X28)),
            // 0xff => Some(Instruction::RST(RSTLocation::X38)),

            // 0x00 => Some(Instruction::NOP),
            // 0x76 => Some(Instruction::HALT),
            // 0xf3 => Some(Instruction::DI),
            //
            // 0xfb => Some(Instruction::EI),
            _ => None,
        }
    }
}
