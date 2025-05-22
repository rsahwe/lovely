use std::ops::{Deref, DerefMut};

pub struct Assembly(pub Vec<Instruction>);

impl Assembly {
    pub fn to_x86_64_linux(&self) -> String {
        self.0
            .iter()
            .map(|instruction| instruction.to_x86_64_linux())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl Deref for Assembly {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Assembly {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub enum Instruction {
    Mov { dest: String, src: String },
    GlobalDeclaration(String),
    Label(String),
    Section(String),
    Comment(String),
    Newline,
}

impl Instruction {
    pub fn to_x86_64_linux(&self) -> String {
        match self {
            Instruction::Mov { dest, src } => format!("mov {dest}, {src}"),
            Instruction::GlobalDeclaration(name) => format!("global {name}"),
            Instruction::Label(name) => format!("{name}:"),
            Instruction::Section(name) => format!(".section {name}"),
            Instruction::Comment(text) => format!("; {text}"),
            Instruction::Newline => String::new(),
        }
    }
}
