use std::fs;

use insta::glob;

use crate::{checker::Checker, ir::IRGenerator, lexer::Lexer, parser::Parser};

#[test]
fn compiler_tests() {
    glob!("source_files/*.lv", |path| {
        let input = fs::read_to_string(path).unwrap();

        // test the lexer
        let lexer = Lexer::new(&input);
        let tokens = lexer.collect::<Vec<_>>();
        insta::assert_debug_snapshot!(tokens);

        // test the parser
        let mut parser = Parser::new(&input);
        let ast = parser.parse().unwrap();
        insta::assert_debug_snapshot!(ast);

        // test the checker
        let mut checker = Checker::new();
        let checked_program = checker.check(&ast).unwrap();
        insta::assert_debug_snapshot!(checked_program);

        // test the IR
        let ir_generator = IRGenerator::new(checker.types);
        let ir = ir_generator.program_ir(&checked_program);
        let ir_string = ir
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        insta::assert_snapshot!(ir_string);
    });
}
