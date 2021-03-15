struct Deleter {
    file: &'static str,
}

impl Drop for Deleter {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(self.file);
    }
}

#[cfg(debug_assertions)]
const COMPILER_PATH: &'static str = "target/debug/hagane";
#[cfg(not(debug_assertions))]
const COMPILER_PATH: &'static str = "target/release/hagane";

macro_rules! test {
    ( $name:ident , $output:expr ) => {
        #[test]
        fn $name() {
            let _deleter = Deleter { file: concat!("tests/", stringify!($name), ".hgn.out") };
            let compiler_out = std::process::Command::new(COMPILER_PATH)
                .arg(concat!("tests/", stringify!($name), ".hgn"))
                .stderr(std::process::Stdio::inherit())
                .output()
                .unwrap();
            assert!(compiler_out.status.success());
            let program_out = std::process::Command::new(concat!("tests/", stringify!($name), ".hgn.out"))
                .stderr(std::process::Stdio::inherit())
                .output()
                .unwrap();
            assert!(program_out.status.success());
            assert_eq!(program_out.stdout, concat!($output, "\n").as_bytes());
            std::fs::remove_file(concat!("tests/", stringify!($name), ".hgn.out")).unwrap();
        }
    };
}

test!(print, "1");
test!(variable, "11");
test!(function, "4");
test!(global_function, "9");
test!(local_function, "20");
test!(capture_mut, "1\n2");
test!(if_, "1\n0");
test!(if_1, "1");
test!(if_2, "1");
test!(if_3, "1\n0");
test!(if_nested, "1");
test!(if_nested_1, "1");
test!(if_nested_2, "1");
test!(if_nested_3, "1");
test!(if_nested_4, "1");
test!(if_nested_5, "1");
test!(if_nested_6, "1");
test!(if_nested_7, "1");
test!(if_nested_8, "1");
test!(while_, "0\n1200");
test!(fib, "7540113804746346429");
test!(while_nested, "3025");
test!(block_expr_in_braces, "1");
test!(first_indent, "1");
test!(comment, "1\n1");
test!(tab_indent, "1");
test!(function_type, "1");
test!(array, "3\n9\n4\n16\n2\n2");
