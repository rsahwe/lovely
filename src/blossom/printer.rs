use colored::Colorize;

pub fn error(text: &str) {
    eprintln!("{}", text.red());
}

pub fn success(text: &str) {
    println!("{}", text.green());
}
