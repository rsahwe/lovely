use colored::Colorize;

pub fn error(text: &str) {
    eprintln!("{}", text.red());
}
