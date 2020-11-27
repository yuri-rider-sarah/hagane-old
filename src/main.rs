mod codegen;
mod error;
mod lexer;
mod parser;

use llvm_sys::analysis::*;
use llvm_sys::core::*;

fn main() {
    let mut chars: Vec<_> = std::fs::read_to_string("/dev/stdin").unwrap().chars().collect();
    chars.reverse();
    let mut tokens = lexer::read_file_tokens(chars).unwrap();
    let mut exprs = Vec::new();
    while tokens.len() != 0 {
        exprs.push(parser::read_expr(&mut tokens).unwrap());
    }
    unsafe {
        let module = codegen::codegen_top(&exprs).unwrap();
        let result = LLVMPrintModuleToString(module);
        println!("{}", std::ffi::CStr::from_ptr(result).to_string_lossy());
        let _ = LLVMVerifyModule(module, LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
    }
}
