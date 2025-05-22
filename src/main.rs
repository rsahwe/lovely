#![allow(dead_code)]
use checker::Checker;
use clap::{Parser, Subcommand};
use ir::IRGenerator;
use parser::Parser as LovelyParser;

mod checker;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod span;

#[cfg(test)]
mod tests;

pub static VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(version, name = "Lovely")]
#[command(about = "Does awesome things", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg()]
        path: String,

        #[arg(short, long)]
        output: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { path, output } => compile(path, output),
    }
}

fn compile(path: String, output_path: Option<String>) {
    println!("Compiling {path}...");
    let file_source = std::fs::read_to_string(path).expect("Unable to read file");
    let mut parser = LovelyParser::new(&file_source);
    let ast = parser.parse().expect("Failed to parse");
    let mut checker = Checker::new();
    let checked_program = checker.check(&ast).expect("Failed to check");

    let ir_generator = IRGenerator::new(checker.types);
    let ir = ir_generator.program_ir(&checked_program);

    let ir_string = ir
        .iter()
        .map(|block| block.to_string())
        .collect::<Vec<_>>()
        .join("\n");

    if let Some(output_path) = output_path {
        std::fs::write(output_path, ir_string).expect("Unable to write file");
    } else {
        println!("{}", ir_string);
    }
}
