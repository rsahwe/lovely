use super::Emitter;
use crate::ir::tac::{BasicBlock, Entity, Instruction, Value};

pub struct CodeGenerator {
    text_section: String,
    rodata_section: String,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            text_section: String::new(),
            rodata_section: String::new(),
        }
    }

    fn basic_block_codegen(&mut self, block: &BasicBlock) {
        if block.label.as_str() == "ENTRY" {
            self.text_section.push_str("_start:\n");

            for instr in &block.instructions {
                if let Instruction::Goto(_) = instr {
                    self.instruction_codegen(instr);
                    self.text_section.push('\n');
                } else {
                    self.rodata_instruction_codegen(instr);
                    self.rodata_section.push('\n');
                }
            }
            self.text_section.push('\n');
        } else {
            self.text_section += &block.label;
            self.text_section.push_str(":\n");

            for instr in &block.instructions {
                self.instruction_codegen(instr);
                self.text_section.push('\n');
            }
            self.text_section.push('\n');
        }
    }

    fn instruction_codegen(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Add { .. } => {
                self.text_section.push_str("  TODO: add");
            }
            Instruction::Sub { .. } => {
                self.text_section.push_str("  TODO: sub");
            }
            Instruction::Mul { .. } => {
                self.text_section.push_str("  TODO: mul");
            }
            Instruction::Div { .. } => {
                self.text_section.push_str("  TODO: div");
            }
            Instruction::Not { .. } => {
                self.text_section.push_str("  TODO: not");
            }
            Instruction::Assign { .. } => {
                self.text_section.push_str("  TODO: assign");
            }
            Instruction::Conditional { .. } => {
                self.text_section.push_str("  TODO: conditional");
            }
            Instruction::Call { .. } => {
                self.text_section.push_str("  TODO: call");
            }
            Instruction::Goto(label) => {
                self.text_section.push_str(&format!("  jmp {label}"));
            }
            Instruction::Ret(_) => {
                self.text_section.push_str("  TODO: ret");
            }
            Instruction::Exit(entity) => {
                self.text_section.push_str("  mov rax, 60\n");
                let entity_asm = self.entity_to_asm(entity);
                self.text_section
                    .push_str(&format!("  mov rdi, {}\n", entity_asm));
                self.text_section.push_str("  syscall\n");
            }
        }
    }

    fn rodata_instruction_codegen(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Assign { dest, src } => match (dest, src) {
                (
                    Entity {
                        value: Value::Variable(name),
                        ..
                    },
                    ..,
                ) => {
                    let entity_asm = self.entity_to_asm(src);
                    self.rodata_section
                        .push_str(&format!("  {name}: dq {entity_asm}"));
                }
                _ => unreachable!("only variable declarations are allowed in the global scope"),
            },
            _ => unreachable!("only assigns are allowed in the global scope"),
        }
    }

    fn entity_to_asm(&mut self, entity: &Entity) -> String {
        match &entity.value {
            Value::Temp(_) => todo!("temp"),
            Value::Unit => todo!("unit"),
            Value::Int(num) => num.to_string(),
            Value::Bool(_) => todo!("bool"),
            Value::Variable(name) => name.to_string(),
            Value::FunctionPointer(label) => label.to_string(),
        }
    }
}

impl Emitter for CodeGenerator {
    fn gen_asm(&mut self, ir: &[BasicBlock]) -> String {
        for block in ir {
            self.basic_block_codegen(block);
        }

        format!(
            "global _start\n\nsection .rodata\n\n{}\nsection .text\n\n{}",
            self.rodata_section, self.text_section
        )
        .trim()
        .to_string()
    }
}
