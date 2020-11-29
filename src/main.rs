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
    let mut chars: Vec<_> = std::fs::read_to_string("/dev/stdin").unwrap().chars().collect();
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
            print!("Error getting target from triple");
            if !error.is_null() {
                println!(": {}", CStr::from_ptr(error).to_string_lossy());
            } else {
                println!("");
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
        let result = LLVMPrintModuleToString(module);
        println!("{}", CStr::from_ptr(result).to_string_lossy());
        LLVMVerifyModule(module, LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
        let mut filename = CString::new("output.o").unwrap().into_bytes_with_nul();
        if LLVMTargetMachineEmitToFile(target_machine, module, filename.as_mut_ptr() as *mut i8, LLVMCodeGenFileType::LLVMObjectFile, &mut error) != 0 {
            print!("Error emitting code to file");
            if !error.is_null() {
                println!(": {}", CStr::from_ptr(error).to_string_lossy());
            } else {
                println!("");
            }
            std::process::exit(1);
        }
    };
}
