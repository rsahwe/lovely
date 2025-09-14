// lovingly named by japi (July 24, 2025)

use crate::codegen::emitters::Emitter;
use crate::{
    blush::combiner::Combiner, checker::Checker,
    codegen::emitters::x86_64_linux_nasm::CodeGenerator, ir::IRGenerator, parser::Parser,
};
use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
};
use toml::Table;

pub mod combiner;
pub mod printer;

pub struct Blush {}

impl Blush {
    pub fn build(path: String) -> Result<(), Box<dyn Error>> {
        let path_buf = PathBuf::from(path);
        let file_tree = collect_files(&path_buf)?;

        printer::info("Reading config file...");
        let Some(config_file) = file_tree.find(&|name, _| {
            name.split('/')
                .next_back()
                .expect("No file name")
                .eq_ignore_ascii_case("config.toml")
        }) else {
            return Err("No config file found".into());
        };
        let Ok(config) = config_file.parse::<Table>() else {
            return Err("Failed to parse config file".into());
        };
        let Some(Some(package_name)) = config.get("package").map(|s| s.as_str()) else {
            return Err("No package name found in config file".into());
        };

        let lovely_files = file_tree
            .filter(&|name, _| {
                std::path::Path::new(name)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("lv"))
            })
            .expect("Expected a directory");

        printer::info(&format!("Parsing {} files...", lovely_files.len()));
        let parsed_lovely_files =
            lovely_files.try_map(&|s| Parser::new(s).parse().map_err(std::convert::Into::into))?;
        let flattened_files: Vec<(String, &_)> = parsed_lovely_files
            .flatten()
            .iter()
            .map(|(name, program)| (trim_path(name, package_name), *program))
            .collect();

        let combined_program = Combiner::new(
            flattened_files
                .iter()
                .map(|(name, program)| (name.as_str(), *program))
                .collect(),
        )
        .combine()?;

        let mut checker = Checker::new();
        let Ok(checked_program) = checker.check(&combined_program) else {
            return Err("Failed to typecheck program".into());
        };

        let ir = IRGenerator::new(checker.types).program_ir(&checked_program);
        let asm = CodeGenerator::new().gen_asm(&ir);

        println!("\n---\n{asm}\n---\n");

        Ok(())
    }
}

fn collect_files(path: &Path) -> Result<FileTreeNode<String>, Box<dyn Error>> {
    if !path.is_dir() {
        printer::error("That's not a directory you silly!");
        return Err("Not a directory".into());
    }
    let Ok(dir) = path.read_dir() else {
        printer::error("Uh for some reason we couldn't read the directory. Not really sure why.");
        return Err("Failed to read directory".into());
    };
    let Ok(dir) = dir.collect::<Result<Vec<_>, _>>() else {
        printer::error(
            "You know, today just really isn't your day is it. Something awful happened, and I have no idea what it was.",
        );
        return Err("Failed to read directory".into());
    };

    let mut children = vec![];

    for dir_entry in dir {
        if dir_entry.path().is_dir() {
            children.push(collect_files(&dir_entry.path())?);
        } else {
            let contents = fs::read_to_string(dir_entry.path())?;
            let name = dir_entry.path().to_str().unwrap().to_string();
            children.push(FileTreeNode::File { name, contents });
        }
    }

    Ok(FileTreeNode::Directory {
        children,
        name: path
            .to_str()
            .expect("Weird file name, it made me uncomfortable.")
            .into(),
    })
}

enum FileTreeNode<T> {
    Directory {
        name: String,
        children: Vec<FileTreeNode<T>>,
    },
    File {
        name: String,
        contents: T,
    },
}

impl<T> FileTreeNode<T> {
    fn filter<F>(&self, f: &F) -> Option<Self>
    where
        F: Fn(&String, &T) -> bool,
        T: Clone,
    {
        match self {
            Self::Directory { children, name } => Some(Self::Directory {
                name: name.clone(),
                children: children.iter().filter_map(|c| c.filter(f)).collect(),
            }),
            Self::File { name, contents } => {
                if f(name, contents) {
                    Some(Self::File {
                        name: name.clone(),
                        contents: contents.clone(),
                    })
                } else {
                    None
                }
            }
        }
    }

    fn map<U>(&self, f: &impl Fn(&T) -> U) -> FileTreeNode<U> {
        match self {
            Self::Directory { name, children } => FileTreeNode::Directory {
                name: name.clone(),
                children: children.iter().map(|c| Self::map(c, f)).collect(),
            },
            Self::File { name, contents } => FileTreeNode::File {
                name: name.clone(),
                contents: f(contents),
            },
        }
    }

    fn try_map<U>(
        &self,
        f: &impl Fn(&T) -> Result<U, Box<dyn Error>>,
    ) -> Result<FileTreeNode<U>, Box<dyn Error>> {
        match self {
            Self::Directory { name, children } => Ok(FileTreeNode::Directory {
                name: name.clone(),
                children: children
                    .iter()
                    .map(|c| c.try_map(f))
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Self::File { name, contents } => Ok(FileTreeNode::File {
                name: name.clone(),
                contents: f(contents)?,
            }),
        }
    }

    fn find(&self, f: &impl Fn(&String, &T) -> bool) -> Option<&T> {
        match self {
            Self::Directory { children, .. } => children.iter().find_map(|c| c.find(f)),
            Self::File { contents, name } => {
                if f(name, contents) {
                    Some(contents)
                } else {
                    None
                }
            }
        }
    }

    fn flatten(&self) -> Vec<(&str, &T)> {
        match self {
            Self::Directory { children, .. } => children.iter().flat_map(|c| c.flatten()).collect(),
            Self::File { contents, name } => vec![(name, contents)],
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::Directory { children, .. } => children.iter().map(Self::len).sum(),
            Self::File { .. } => 1,
        }
    }
}

fn trim_path(file_path: &str, package_name: &str) -> String {
    let segments = file_path.split('/').collect::<Vec<_>>();
    let mut new_segments = Vec::new();

    let inside_dir = !segments.contains(&package_name);

    let mut has_hit_package_name = false;
    for segment in segments {
        if segment.eq_ignore_ascii_case(".") {
            continue;
        }
        if segment.eq_ignore_ascii_case(package_name) {
            has_hit_package_name = true;
            continue;
        }
        if has_hit_package_name || inside_dir {
            new_segments.push(segment);
        }
    }

    new_segments.join("/")
}

#[cfg(test)]
mod tests {
    use super::trim_path;

    #[test]
    fn trim_path_test() {
        assert_eq!(
            trim_path("my_package/foo/bar.lv", "my_package"),
            "foo/bar.lv"
        );
        assert_eq!(
            trim_path("./my_package/foo/bar.lv", "my_package"),
            "foo/bar.lv"
        );
        assert_eq!(
            trim_path(
                "/Users/sam/whatever/my_package/foo/bar/baz.lv",
                "my_package"
            ),
            "foo/bar/baz.lv"
        );

        // edge case for if you're in the directory...
        assert_eq!(
            trim_path("./foo/bar/baz.lv", "my_package"),
            "foo/bar/baz.lv"
        );
    }
}
