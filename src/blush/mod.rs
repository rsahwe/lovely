// lovingly named by japi (July 24, 2025)

use crate::parser;
use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
};

pub mod printer;

pub struct Blush {}

impl Blush {
    pub fn build(path: String) -> Result<(), Box<dyn Error>> {
        let path_buf = PathBuf::from(path);
        let file_tree = collect_files(&path_buf)?;
        let lovely_files = file_tree
            .filter(&|name, _| {
                std::path::Path::new(name)
                    .extension()
                    .is_some_and(|ext| ext.eq_ignore_ascii_case("lv"))
            })
            .expect("Expected a directory");
        let _parsed_lovely_files = lovely_files.try_map(&|s| {
            parser::Parser::new(s)
                .parse()
                .map_err(std::convert::Into::into)
        })?;

        println!("Building your project...");

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
}
