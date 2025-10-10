use crate::blush::Blush;
use std::fs;
use std::path::PathBuf;

fn get_test_case_dir_names() -> Vec<String> {
    let cases_dir = PathBuf::from("src/tests/cases");
    fs::read_dir(&cases_dir)
        .expect("Failed to read test cases directory")
        .filter_map(std::result::Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .map(|path| path.to_str().unwrap().to_string())
        .collect()
}

#[test]
fn snapshot_tests() {
    let test_case_dirs = get_test_case_dir_names();
    for dir_name in test_case_dirs {
        let asm = Blush::get_asm_and_package_name(&dir_name);
        insta::with_settings!({
            description => dir_name.clone(),
        }, {
            match asm {
                Ok((asm, _)) => {
                    insta::assert_snapshot!(asm);
                }
                Err(err) => {
                    insta::assert_snapshot!(err);
                }
            }
        });
    }
}
