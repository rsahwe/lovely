#![allow(dead_code)]

use blossom::Blossom;
use clap::{Parser, Subcommand};

mod blossom;
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
            println!("");
            let Ok(_) = Blossom::build(path) else {
                blossom::printer::error("That's not a directory you silly!\n");
                return;
            };
            println!("\n");
        }
    }
}
