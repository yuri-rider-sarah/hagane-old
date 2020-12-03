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

fn main() {
    let src_file = std::env::args().nth(1).unwrap();
    let dest_file = std::env::args().nth(2).unwrap_or(src_file.clone() + ".out");
    let mut chars: Vec<_> = std::fs::read_to_string(&src_file).unwrap().chars().collect();
    chars.reverse();
    let mut tokens = lexer::read_file_tokens(chars).unwrap();
    let mut exprs = Vec::new();
    while tokens.len() != 0 {
        exprs.push(parser::read_expr(&mut tokens).unwrap());
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
            eprintln!("Module:");
            eprintln!("{}", std::ffi::CStr::from_ptr(LLVMPrintModuleToString(module)).to_string_lossy());
            std::process::exit(1);
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
