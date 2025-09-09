use colored::Colorize;

pub fn error(text: &str) {
    eprintln!("{}", text.red());
}

pub fn success(text: &str) {
    println!("{}", text.green());
}

pub fn info(text: &str) {
    println!("{text}");
}
