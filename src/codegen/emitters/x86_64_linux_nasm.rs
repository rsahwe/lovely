use super::Emitter;
use crate::ir::tac::{BasicBlock, Entity, Instruction, TempId, Value};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

pub struct CodeGenerator {
    text_section: String,
    rodata_section: String,
    global_vars: HashSet<String>,
    local_vars: HashMap<String, HashMap<String, usize>>, // (function label -> (variable name -> stack offset))
    current_stack_offset: HashMap<String, usize>,        // (function label -> stack offset)
    current_function: Option<String>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            text_section: String::new(),
            rodata_section: String::new(),
            global_vars: HashSet::new(),
            local_vars: HashMap::new(),
            current_stack_offset: HashMap::new(),
            current_function: None,
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
        } else {
            self.text_section += &block.label;
            self.current_function = Some(block.label.clone());
            self.text_section.push_str(":\n");
            self.text_section.push_str("  enter 0, 0\n\n");

            let mut this_function_locals = HashMap::new();
            let mut this_function_stack_offset = 0;

            if !block.parameters.is_empty() {
                self.text_section
                    .push_str("  ; register parameters as local variables\n");
                for (index, param) in block.parameters.iter().enumerate() {
                    this_function_locals.insert(param.name.clone(), this_function_stack_offset);
                    this_function_stack_offset += param.ty.size_in_bytes();
                    let _ = writeln!(
                        self.text_section,
                        "  push qword [rbp + {}]",
                        (index + 1) * 8 + 8
                    );
                }
                self.text_section.push('\n');
            }

            self.local_vars
                .insert(block.label.clone(), this_function_locals);
            self.current_stack_offset
                .insert(block.label.clone(), this_function_stack_offset);

            for instr in &block.instructions {
                self.instruction_codegen(instr);
                self.text_section.push('\n');
            }
        }
        self.text_section.push('\n');
    }

    #[allow(clippy::too_many_lines)]
    fn instruction_codegen(&mut self, instr: &Instruction) {
        let _ = writeln!(self.text_section, "  ; {instr}");
        match instr {
            Instruction::Add { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    self.allocate_temp_value(temp_id, dest);

                    let lhs_asm = self.entity_to_asm(lhs);
                    let rhs_asm = self.entity_to_asm(rhs);

                    let _ = writeln!(self.text_section, "  mov rax, {lhs_asm}");
                    let _ = writeln!(self.text_section, "  add rax, {rhs_asm}");
                    let _ = writeln!(self.text_section, "  push qword rax");
                } else {
                    unreachable!()
                }
            }
            Instruction::Sub { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    self.allocate_temp_value(temp_id, dest);

                    let lhs_asm = self.entity_to_asm(lhs);
                    let rhs_asm = self.entity_to_asm(rhs);

                    let _ = writeln!(self.text_section, "  mov rax, {lhs_asm}");
                    let _ = writeln!(self.text_section, "  sub rax, {rhs_asm}");
                    let _ = writeln!(self.text_section, "  push qword rax");
                } else {
                    unreachable!()
                }
            }
            Instruction::Mul { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    self.allocate_temp_value(temp_id, dest);

                    let lhs_asm = self.entity_to_asm(lhs);
                    let rhs_asm = self.entity_to_asm(rhs);

                    let _ = writeln!(self.text_section, "  mov rax, {lhs_asm}");
                    let _ = writeln!(self.text_section, "  imul rax, {rhs_asm}");
                    let _ = writeln!(self.text_section, "  push qword rax");
                } else {
                    unreachable!()
                }
            }
            Instruction::Div { dest, lhs, rhs } => {
                if let Value::Temp(temp_id) = dest.value {
                    self.allocate_temp_value(temp_id, dest);

                    let dividend_asm = self.entity_to_asm(lhs);
                    let divisor_asm = self.entity_to_asm(rhs);

                    let _ = writeln!(self.text_section, "  mov rax, {dividend_asm}");
                    let _ = writeln!(self.text_section, "  cqo");
                    let _ = writeln!(self.text_section, "  idiv qword {divisor_asm}");
                    let _ = writeln!(self.text_section, "  push qword rax");
                } else {
                    unreachable!()
                }
            }
            Instruction::Not { .. } => {
                self.text_section.push_str("  TODO: not\n");
            }
            Instruction::Assign { dest, src } => match (&dest.value, &src.ty) {
                (Value::Variable(name), ty) => {
                    let (this_function_locals, this_function_stack_offset) =
                        self.function_locals_and_offset();

                    this_function_locals.insert(name.to_string(), *this_function_stack_offset);
                    *this_function_stack_offset += ty.size_in_bytes();

                    let src_asm = self.entity_to_asm(src);
                    let _ = writeln!(self.text_section, "  push qword {src_asm}");
                }
                _ => unreachable!(),
            },
            Instruction::Conditional { .. } => {
                self.text_section.push_str("  TODO: conditional\n");
            }
            Instruction::Call { dest, callee, args } => {
                if let Value::Temp(temp_id) = dest.value {
                    self.allocate_temp_value(temp_id, dest);

                    let callee_asm = self.entity_to_asm(callee);

                    for arg in args.iter().rev() {
                        let arg_asm = self.entity_to_asm(arg);
                        let _ = writeln!(self.text_section, "  push qword {arg_asm}");
                    }

                    let _ = writeln!(self.text_section, "  call {callee_asm}");
                    let _ = writeln!(self.text_section, "  add rsp, {}", args.len() * 8);
                    let _ = writeln!(self.text_section, "  push qword rax");
                } else {
                    unreachable!()
                }
            }
            Instruction::Goto(label) => {
                let _ = writeln!(self.text_section, "  jmp {label}");
            }
            Instruction::Ret(entity) => {
                let entity_asm = self.entity_to_asm(entity);
                let _ = writeln!(self.text_section, "  mov rax, {entity_asm}");
                let _ = writeln!(self.text_section, "  leave");
                let _ = writeln!(self.text_section, "  ret");
            }
            Instruction::Exit(entity) => {
                self.text_section.push_str("  mov rax, 60\n");
                let entity_asm = self.entity_to_asm(entity);
                let _ = writeln!(self.text_section, "  mov rdi, {entity_asm}");
                let _ = writeln!(self.text_section, "  syscall");
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
                    let _ = writeln!(self.rodata_section, "  {name}: dq {entity_asm}");
                }
                _ => unreachable!("only variable declarations are allowed in the global scope"),
            },
            _ => unreachable!("only assigns are allowed in the global scope"),
        }
    }

    fn entity_to_asm(&mut self, entity: &Entity) -> String {
        match &entity.value {
            Value::Temp(temp_id) => {
                let (this_function_locals, _) = self.function_locals_and_offset();
                format!(
                    "[rbp - {}]",
                    this_function_locals
                        .get(&format!("t{temp_id}"))
                        .unwrap_or_else(|| panic!("local var not found: t{temp_id}"))
                        + 8
                )
            }
            Value::Unit => todo!("unit"),
            Value::Int(num) => num.to_string(),
            Value::Bool(_) => todo!("bool"),
            Value::Variable(name) => {
                if let Some(var_name) = self.global_vars.get(name) {
                    return format!("[{var_name}]");
                }
                let (this_function_locals, _) = self.function_locals_and_offset();
                format!(
                    "[rbp - {}]",
                    this_function_locals
                        .get(name)
                        .unwrap_or_else(|| panic!("local var not found: {name}"))
                        + 8
                )
            }
            Value::FunctionPointer(label) => label.to_string(),
        }
    }

    fn allocate_temp_value(&mut self, temp_id: TempId, dest_entity: &Entity) {
        let name = format!("t{temp_id}");
        let (this_function_locals, this_function_stack_offset) = self.function_locals_and_offset();

        this_function_locals.insert(name, *this_function_stack_offset);
        *this_function_stack_offset += dest_entity.ty.size_in_bytes();
    }

    fn function_locals_and_offset(&mut self) -> (&mut HashMap<String, usize>, &mut usize) {
        (
            self.local_vars
                .get_mut(self.current_function.as_ref().unwrap())
                .unwrap(),
            self.current_stack_offset
                .get_mut(self.current_function.as_ref().unwrap())
                .unwrap(),
        )
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
