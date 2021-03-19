use crate::error::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

pub const ENTRY_C_NAME: *const i8 = b"entry\0" as *const u8 as *const i8;
pub const BB_C_NAME: *const i8 = b"bb\0" as *const u8 as *const i8;

#[derive(Clone, Copy)]
pub enum Var {
    Const(LLVMValueRef),
    Mut(LLVMValueRef),
}

pub struct Context {
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub names: Vec<(String, Var, Type)>
}

pub unsafe fn create_function(context: &mut Context, name: &str, params_num: usize) -> Result<LLVMValueRef> {
    let func_c_name = CString::new(name).unwrap();
    let func = LLVMAddFunction(context.module, func_c_name.as_ptr(), get_llvm_function_type(params_num));
    LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetFunctionCallConv(func, LLVMCallConv::LLVMFastCallConv as u32);
    let entry = LLVMAppendBasicBlock(func, ENTRY_C_NAME);
    LLVMPositionBuilderAtEnd(context.builder, entry);
    Ok(func)
}

pub unsafe fn codegen_to_void_ptr(val: LLVMValueRef, context: &mut Context) -> LLVMValueRef {
    LLVMBuildBitCast(context.builder, val, LLVMPointerType(LLVMInt8Type(), 0), &0)
}

pub unsafe fn codegen_unit(context: &mut Context) -> (LLVMValueRef, Type) {
    (codegen_to_void_ptr(LLVMBuildMalloc(context.builder, unit_type(), &0), context), Type::Tuple(Vec::new()))
}

pub unsafe fn codegen_load(ptr: LLVMValueRef, llvm_type: LLVMTypeRef, context: &mut Context) -> LLVMValueRef {
    LLVMBuildLoad(context.builder, LLVMBuildBitCast(context.builder, ptr, LLVMPointerType(llvm_type, 0), &0), &0)
}

pub fn check_type_compat(stated_type: &Option<Type>, inferred_type: &Type) -> Result<()> {
    match stated_type {
        Some(stated_type) if stated_type != inferred_type => Err(Error::ConflictingType(stated_type.clone(), inferred_type.clone())),
        _ => Ok(()),
    }
}

pub unsafe fn unit_type() -> LLVMTypeRef {
    LLVMStructType(std::ptr::null_mut(), 0, 0)
}

pub unsafe fn void_ptr_type() -> LLVMTypeRef {
    LLVMPointerType(LLVMInt8Type(), 0)
}

pub unsafe fn list_type() -> LLVMTypeRef {
    let mut contents = [
        LLVMInt64Type(),
        LLVMPointerType(void_ptr_type(), 0),
    ];
    LLVMStructType(
        contents.as_mut_ptr(),
        contents.len() as u32,
        0,
    )
}

pub unsafe fn get_unboxed_llvm_type(type_: &Type) -> Result<LLVMTypeRef> {
    Ok(match type_ {
        Type::Named(name) => match &name[..] {
            "Int" => LLVMInt64Type(),
            "Bool" => LLVMInt1Type(),
            _ => return Err(Error::UnboundType(name.clone())),
        }
        Type::Tuple(types) => {
            let mut llvm_types = Vec::new();
            for _ in types {
                llvm_types.push(void_ptr_type());
            }
            LLVMStructType(
                llvm_types.as_mut_ptr(),
                types.len() as u32,
                0,
            )
        },
        Type::Function(params, _) => {
            let mut contents = [
                LLVMPointerType(get_llvm_function_type(params.len()), 0),
                void_ptr_type(),
            ];
            LLVMStructType(
                contents.as_mut_ptr(),
                contents.len() as u32,
                0,
            )
        },
        Type::Applied(base, args) => match &**base {
            Type::Named(name) => match &name[..] {
                "List" => match &args[..] {
                    [_] => list_type(),
                    _ => return Err(Error::MismatchedTypeArgsNum(1, args.len())),
                }
                _ => return Err(Error::UnboundType(name.clone())),
            }
            _ => return Err(Error::NonParametrizableType(type_.clone())),
        },
        Type::Forall(_, _) => return Err(Error::AmbiguousType),
    })
}

pub unsafe fn get_llvm_function_type(params: usize) -> LLVMTypeRef {
    let mut llvm_types = vec![void_ptr_type(); params + 1];
    LLVMFunctionType(
        void_ptr_type(),
        llvm_types.as_mut_ptr(),
        llvm_types.len() as u32,
        0,
    )
}

pub fn resolve_name(names: &Vec<(String, Var, Type)>, name: &str) -> Result<(Var, Type)> {
    names
        .iter()
        .rev()
        .find_map(|(key, value, type_)| if *key == *name { Some((*value, type_.clone())) } else { None })
        .ok_or(Error::UnboundVariable(name.to_string()))
}

pub fn substitute_in_type(type_: &Type, substitutions: &Vec<(String, Type)>) -> Type {
    match type_ {
        Type::Named(name) => {
            for (param, arg) in substitutions {
                if name == param {
                    return arg.clone();
                }
            }
            Type::Named(name.clone())
        },
        Type::Applied(base, args) => Type::Applied(
            Box::new(substitute_in_type(base, substitutions)),
            args.into_iter().map(|type_| substitute_in_type(type_, substitutions)).collect(),
        ),
        Type::Tuple(types) => Type::Tuple(types.into_iter().map(|type_| substitute_in_type(type_, substitutions)).collect()),
        Type::Function(param_types, ret_type) => Type::Function(
            param_types.into_iter().map(|type_| substitute_in_type(type_, substitutions)).collect(),
            Box::new(substitute_in_type(ret_type, substitutions))
        ),
        Type::Forall(params, base) => {
            let substitutions_ = substitutions.clone().into_iter().filter(|(param, _)| params.contains(param)).collect();
            Type::Forall(params.clone(), Box::new(substitute_in_type(base, &substitutions_)))
        },
    }
}
