#![allow(dead_code)]

use blush::Blush;
use clap::{Parser, Subcommand};

mod blush;
mod checker;
mod codegen;
mod ir;
mod lexer;
mod parser;
mod span;
mod targets;

#[cfg(test)]
mod tests;

pub static VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(version, name = "Lovely")]
#[command(
    about = "A lovely, compiled, type-driven systems programming language.",
    long_about = None
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        #[arg()]
        path: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { path } => {
            println!();
            let res = Blush::build(path);
            match res {
                Ok(()) => {
                    blush::printer::success("Build complete!");
                }
                Err(err) => {
                    blush::printer::error(&format!("Error: {err}"));
                }
            }
            println!("\n");
        }
    }
}
