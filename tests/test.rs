#[cfg(debug_assertions)]
const COMPILER_PATH: &'static str = "target/debug/hagane";
#[cfg(not(debug_assertions))]
const COMPILER_PATH: &'static str = "target/release/hagane";

macro_rules! test {
    ( $name:ident , $output:expr ) => {
        #[test]
        fn $name() {
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