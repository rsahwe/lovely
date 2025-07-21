use std::collections::{HashMap, HashSet};

use super::Emitter;
use crate::ir::tac::{BasicBlock, Entity, Instruction, TempId, Value};

pub struct CodeGenerator {
    text_section: String,
    rodata_section: String,
    global_vars: HashSet<String>,
    local_vars: HashMap<String, usize>,
    current_stack_offset: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            text_section: String::new(),
            rodata_section: String::new(),
            global_vars: HashSet::new(),
            local_vars: HashMap::new(),
            current_stack_offset: 0,
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
            self.text_section.push_str("  enter 0, 0\n");

            for instr in &block.instructions {
                self.instruction_codegen(instr);
                self.text_section.push('\n');
            }
            self.text_section.push('\n');
        }
    }

    fn instruction_codegen(&mut self, instr: &Instruction) {
        match instr {
            Instruction::Add { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    let name = format!("t{temp_id}");
                    self.local_vars
                        .insert(name.to_string(), self.current_stack_offset);
                    self.current_stack_offset += dest.ty.size_in_bytes();

                    let lhs_asm = self.entity_to_asm(lhs);
                    let rhs_asm = self.entity_to_asm(rhs);
                    let dest_asm = self.entity_to_asm(dest);

                    self.text_section
                        .push_str(&format!("  push qword {lhs_asm}\n"));
                    self.text_section
                        .push_str(&format!("  add qword {}, {}", dest_asm, rhs_asm));
                } else {
                    unreachable!()
                }
            }
            Instruction::Sub { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    let name = format!("t{temp_id}");
                    self.local_vars
                        .insert(name.to_string(), self.current_stack_offset);
                    self.current_stack_offset += dest.ty.size_in_bytes();

                    let lhs_asm = self.entity_to_asm(lhs);
                    let rhs_asm = self.entity_to_asm(rhs);
                    let dest_asm = self.entity_to_asm(dest);

                    self.text_section
                        .push_str(&format!("  push qword {lhs_asm}\n"));
                    self.text_section
                        .push_str(&format!("  sub qword {}, {}", dest_asm, rhs_asm));
                } else {
                    unreachable!()
                }
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
            Instruction::Assign { dest, src } => match (&dest.value, &src.ty) {
                (Value::Variable(name), ty) => {
                    self.local_vars
                        .insert(name.to_string(), self.current_stack_offset);
                    self.current_stack_offset += ty.size_in_bytes();

                    let src_asm = self.entity_to_asm(src);
                    self.text_section
                        .push_str(&format!("  push qword {}", src_asm));
                }
                _ => unreachable!(),
            },
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
                    self.global_vars.insert(name.to_string());
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
            Value::Temp(temp_id) => format!(
                "[rbp - {}]",
                self.local_vars
                    .get(&format!("t{temp_id}"))
                    .expect(&format!("local var not found: t{temp_id}"))
                    + 8
            ),
            Value::Unit => todo!("unit"),
            Value::Int(num) => num.to_string(),
            Value::Bool(_) => todo!("bool"),
            Value::Variable(name) => {
                if let Some(var_name) = self.global_vars.get(name) {
                    return format!("[{var_name}]");
                }
                format!(
                    "[rbp - {}]",
                    self.local_vars
                        .get(name)
                        .expect(&format!("local var not found: {name}"))
                        + 8
                )
            }
            Value::FunctionPointer(label) => label.to_string(),
        }
    }

    fn if_temp_push(&mut self, entity: &Entity) {
        if let Value::Temp(temp_id) = entity.value {
            let name = format!("t{temp_id}");
            self.local_vars
                .insert(name.to_string(), self.current_stack_offset);
            self.current_stack_offset += entity.ty.size_in_bytes();
            let entity_asm = self.entity_to_asm(entity);
            self.text_section
                .push_str(&format!("  push qword {entity_asm}"));
        }
    }
}

impl Emitter for CodeGenerator {
    fn gen_asm(&mut self, ir: &[BasicBlock]) -> String {
        for block in ir {
            self.basic_block_codegen(block);
        }

        format!(
            "bits 64\nglobal _start\n\nsection .rodata\n\n{}\nsection .text\n\n{}",
            self.rodata_section, self.text_section
        )
        .trim()
        .to_string()
    }
}
