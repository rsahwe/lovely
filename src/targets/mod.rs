pub struct Target {
    os: OSType,
    assembler: Assembler,
    arch: Architecture,
}

enum OSType {
    Linux,
}

enum Assembler {
    Nasm,
}

enum Architecture {
    X86_64,
}

pub static SUPPORTED_TARGETS: [Target; 1] = [Target {
    os: OSType::Linux,
    assembler: Assembler::Nasm,
    arch: Architecture::X86_64,
}];
