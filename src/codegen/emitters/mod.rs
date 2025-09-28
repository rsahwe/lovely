use crate::ir::tac::BasicBlock;

pub mod aarch64_apple_gas;
pub mod x86_64_linux_nasm;

pub trait Emitter {
    fn gen_asm(&mut self, ir: &[BasicBlock]) -> String;
}
