use std::fmt::Display;

pub type Label = String;

pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

pub struct BasicBlock {
    pub label: Label,
    pub parameters: Vec<Parameter>,
    pub instructions: Vec<Instruction>,
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}{}:",
            self.label,
            if self.parameters.is_empty() {
                String::new()
            } else {
                format!(
                    "({})",
                    self.parameters
                        .iter()
                        .map(|p| format!("{}: {}", p.name, p.ty))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        )?;
        for instruction in &self.instructions {
            writeln!(f, "  {instruction}")?;
        }
        Ok(())
    }
}

pub enum Instruction {
    Add {
        dest: Entity,
        lhs: Entity,
        rhs: Entity,
    },
    Sub {
        dest: Entity,
        lhs: Entity,
        rhs: Entity,
    },
    Mul {
        dest: Entity,
        lhs: Entity,
        rhs: Entity,
    },
    Div {
        dest: Entity,
        lhs: Entity,
        rhs: Entity,
    },
    Not {
        dest: Entity,
        src: Entity,
    },
    Assign {
        dest: Entity,
        src: Entity,
    },
    Conditional {
        lhs: Entity,
        rhs: Entity,
        operator: Operator,
        true_label: Label,
        false_label: Label,
    },
    Call {
        dest: Entity,
        callee: Entity,
        args: Vec<Entity>,
    },
    Goto(Label),
    Ret(Entity),
    Exit(Entity),
}

impl Instruction {
    pub const fn assign(dest: Entity, src: Entity) -> Self {
        Self::Assign { dest, src }
    }
    pub const fn add(dest: Entity, lhs: Entity, rhs: Entity) -> Self {
        Self::Add { dest, lhs, rhs }
    }
    pub const fn sub(dest: Entity, lhs: Entity, rhs: Entity) -> Self {
        Self::Sub { dest, lhs, rhs }
    }
    pub const fn mul(dest: Entity, lhs: Entity, rhs: Entity) -> Self {
        Self::Mul { dest, lhs, rhs }
    }
    pub const fn div(dest: Entity, lhs: Entity, rhs: Entity) -> Self {
        Self::Div { dest, lhs, rhs }
    }
    pub const fn not(dest: Entity, src: Entity) -> Self {
        Self::Not { dest, src }
    }
    pub const fn call(dest: Entity, callee: Entity, args: Vec<Entity>) -> Self {
        Self::Call { dest, callee, args }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add { dest, lhs, rhs } => write!(f, "{dest} := {lhs} + {rhs}"),
            Self::Sub { dest, lhs, rhs } => write!(f, "{dest} := {lhs} - {rhs}"),
            Self::Mul { dest, lhs, rhs } => write!(f, "{dest} := {lhs} * {rhs}"),
            Self::Div { dest, lhs, rhs } => write!(f, "{dest} := {lhs} / {rhs}"),
            Self::Not { dest, src } => write!(f, "{dest} := !{src}"),
            Self::Assign { dest, src } => write!(f, "{dest} := {src}"),
            Self::Conditional {
                lhs,
                rhs,
                operator,
                true_label,
                false_label,
            } => write!(
                f,
                "if {lhs} {operator} {rhs} then goto {true_label} else goto {false_label}"
            ),
            Self::Call { dest, callee, args } => write!(
                f,
                "{dest} := {callee}({})",
                args.iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Goto(label) => write!(f, "goto {label}"),
            Self::Ret(entity) => write!(f, "ret {entity}"),
            Self::Exit(code) => write!(f, "exit {code}"),
        }
    }
}

pub type TempId = usize;

#[derive(Clone, Debug)]
pub struct Entity {
    pub value: Value,
    pub ty: Type,
}

impl Entity {
    pub const fn new(value: Value, ty: Type) -> Self {
        Self { value, ty }
    }
    pub fn variable(name: &str, ty: Type) -> Self {
        Self {
            value: Value::Variable(name.to_string()),
            ty,
        }
    }
    pub const fn temp_val(id: usize, ty: Type) -> Self {
        Self {
            value: Value::Temp(id),
            ty,
        }
    }
    pub const fn int_literal(num: isize) -> Self {
        Self {
            value: Value::Int(num),
            ty: Type::Int,
        }
    }
    pub const fn unit() -> Self {
        Self {
            value: Value::Unit,
            ty: Type::Unit,
        }
    }
    pub const fn bool(val: bool) -> Self {
        Self {
            value: Value::Bool(val),
            ty: Type::Bool,
        }
    }
    pub fn function(label: &str, return_type: Type) -> Self {
        Self {
            value: Value::FunctionPointer(label.to_string()),
            ty: Type::Function {
                return_type: Box::new(return_type),
            },
        }
    }
}

impl Display for Entity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Value::Temp(id) => write!(f, "t{id}"),
            Value::Unit => write!(f, "unit"),
            Value::Int(val) => write!(f, "{val}"),
            Value::Bool(val) => write!(f, "{val}"),
            Value::Variable(name) => write!(f, "{name}"),
            Value::FunctionPointer(label) => write!(f, "{label}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Function { return_type: Box<Type> },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "Unit"),
            Self::Function { .. } => write!(f, "TODO(function types)"),
        }
    }
}

impl Type {
    pub const fn size_in_bytes(&self) -> usize {
        match &self {
            Self::Bool => 1,
            Self::Unit => 0,
            Self::Int | Self::Function { .. } => 8,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Temp(TempId),
    Unit,
    Int(isize),
    Bool(bool),
    Variable(String),
    FunctionPointer(Label),
}

pub enum Operator {
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Less => write!(f, "<"),
            Self::Greater => write!(f, ">"),
            Self::LessEqual => write!(f, "<="),
            Self::GreaterEqual => write!(f, ">="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
        }
    }
}
