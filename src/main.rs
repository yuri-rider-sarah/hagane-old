mod codegen;
mod error;
mod lexer;
mod parser;

use std::ffi::CStr;
use std::ffi::CString;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_manager_builder::*;

fn main() {
    let mut main_args = Vec::new();
    let mut print_ir_unopt = false;
    let mut print_ir = false;
    let mut opt_level = 0;
    let inline_threshold = 225;
    for arg in std::env::args().skip(1) {
        if arg.starts_with("-") {
            match &arg[1..] {
                "O0" => opt_level = 0,
                "O1" => opt_level = 1,
                "O2" => opt_level = 2,
                "O3" => opt_level = 3,
                "print-ir-unopt" => print_ir_unopt = true,
                "print-ir" => print_ir = true,
                _ => {
                    eprintln!("Invalid argument: {}", arg);
                    std::process::exit(1);
                },
            }
        } else {
            main_args.push(arg);
        }
    }
    let (src_file, dest_file) = match &main_args[..] {
        [src] => (src.clone(), src.clone() + ".out"),
        [src, dest] => (src.clone(), dest.clone()),
        _ => {
            eprintln!("Invalid arguments");
            std::process::exit(1);
        },
    };
    let chars: Vec<_> = std::fs::read_to_string(&src_file).unwrap().chars().collect();
    let mut tokens = lexer::Tokens::new(chars).unwrap();
    let mut exprs = Vec::new();
    loop {
        let state = lexer::get_lexer_state(&tokens);
        if lexer::read_token(&mut tokens).unwrap() == lexer::Token::Eof {
            break;
        }
        lexer::restore_lexer_state(&mut tokens, state);
        exprs.push(parser::read_block_expr(&mut tokens).unwrap());
    }
    unsafe {
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
        let target_triple = LLVMGetDefaultTargetTriple();
        let mut target = std::ptr::null_mut();
        let mut error = std::ptr::null_mut();
        if LLVMGetTargetFromTriple(target_triple, &mut target, &mut error) != 0 {
            eprint!("Error getting target from triple");
            if !error.is_null() {
                eprintln!(": {}", CStr::from_ptr(error).to_string_lossy());
            } else {
                eprintln!("");
            }
            std::process::exit(1);
        }
        let cpu = CString::new("generic").unwrap();
        let features = CString::new("").unwrap();
        let target_machine = LLVMCreateTargetMachine(
            target,
            target_triple,
            cpu.as_ptr(),
            features.as_ptr(),
            LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            LLVMRelocMode::LLVMRelocDefault,
            LLVMCodeModel::LLVMCodeModelDefault,
        );
        let module = codegen::codegen_top(&exprs).unwrap();
        LLVMSetModuleDataLayout(module, LLVMCreateTargetDataLayout(target_machine));
        LLVMSetTarget(module, target_triple);
        if LLVMVerifyModule(module, LLVMVerifierFailureAction::LLVMPrintMessageAction, std::ptr::null_mut()) != 0 {
            eprintln!("=== Unoptimized LLVM IR ===");
            eprintln!("{}", std::ffi::CStr::from_ptr(LLVMPrintModuleToString(module)).to_string_lossy());
            std::process::exit(1);
        }
        if opt_level > 0 {
            if print_ir_unopt {
                eprintln!("=== Unoptimized LLVM IR ===");
                eprintln!("{}", std::ffi::CStr::from_ptr(LLVMPrintModuleToString(module)).to_string_lossy());
            }
            let pass_manager = LLVMCreatePassManager();
            let pass_manager_builder = LLVMPassManagerBuilderCreate();
            LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, opt_level);
            LLVMPassManagerBuilderUseInlinerWithThreshold(pass_manager_builder, inline_threshold);
            LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
            LLVMRunPassManager(pass_manager, module);
        }
        if print_ir {
            eprintln!("=== Optimized LLVM IR ===");
            eprintln!("{}", std::ffi::CStr::from_ptr(LLVMPrintModuleToString(module)).to_string_lossy());
        }
        let temp_dir = tempfile::tempdir().unwrap();
        let obj_path = temp_dir.path().clone().join(std::path::Path::new("object.o")).to_str().unwrap().to_string();
        let mut c_obj_path = CString::new(&obj_path[..]).unwrap().into_bytes_with_nul();
        if LLVMTargetMachineEmitToFile(target_machine, module, c_obj_path.as_mut_ptr() as *mut i8, LLVMCodeGenFileType::LLVMObjectFile, &mut error) != 0 {
            eprint!("Error emitting code to file");
            if !error.is_null() {
                eprintln!(": {}", CStr::from_ptr(error).to_string_lossy());
            } else {
                eprintln!("");
            }
            std::process::exit(1);
        }
        let program_main: &[u8] = std::include_bytes!("program_main.c");
        let main_path = temp_dir.path().clone().join(std::path::Path::new("main.c")).to_str().unwrap().to_string();
        std::fs::write(&main_path, program_main).unwrap();
        let clang_exit_code = std::process::Command::new("clang")
            .args(&[&obj_path[..], &main_path[..], "-static", "-o", &dest_file[..]])
            .status()
            .unwrap();
        if !clang_exit_code.success() {
            std::process::exit(1);
        }
    };
}
