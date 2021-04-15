use crate::error::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

pub const ENTRY_C_NAME: *const i8 = b"entry\0" as *const u8 as *const i8;
pub const BB_C_NAME: *const i8 = b"bb\0" as *const u8 as *const i8;

#[derive(Clone)]
pub enum Var {
    Const(LLVMValueRef, Type),
    Mut(LLVMValueRef, Type),
    Ctor(LLVMValueRef, u64, Option<Vec<String>>, Vec<Type>, Type),
}

pub unsafe fn var_value(var: &Var, context: &Context) -> LLVMValueRef {
    match var {
        Var::Const(val, _) => *val,
        Var::Mut(ptr, _) => LLVMBuildLoad(context.builder, *ptr, &0),
        Var::Ctor(ctor, _, _, _, _) => *ctor,
    }
}

pub fn var_type(var: Var) -> Type {
    match var {
        Var::Const(_, type_) => type_,
        Var::Mut(_, type_) => type_,
        Var::Ctor(_, _, type_params, param_types, ret_type) => match type_params {
            Some(type_params) => Type::Forall(
                type_params.clone(),
                Box::new(Type::Function(
                    param_types,
                    Box::new(Type::Applied(
                        Box::new(ret_type),
                        type_params.iter().map(|type_param| Type::Named(type_param.clone())).collect(),
                    )),
                )),
            ),
            None => Type::Function(param_types, Box::new(ret_type)),
        },
    }
}

pub unsafe fn get_llvm_variant_type(num_params: usize) -> LLVMTypeRef {
    let mut llvm_types = vec![LLVMInt64Type()];
    for _ in 0..num_params {
        llvm_types.push(void_ptr_type());
    }
    LLVMStructType(llvm_types.as_mut_ptr(), llvm_types.len() as u32, 0)
}

pub unsafe fn codegen_var_ctor(
    tag: u64,
    type_params: Option<Vec<String>>,
    param_types: Vec<Type>,
    ret_type: Type,
    name: &str,
    context: &mut Context
) -> Result<()> {
    let parent = LLVMGetInsertBlock(context.builder);
    let func = create_function(context, name, param_types.len())?;
    let llvm_type = get_llvm_variant_type(param_types.len());
    let ret_val = LLVMBuildMalloc(context.builder, llvm_type, &0);
    let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), 0, 0)];
    let ptr = LLVMBuildGEP(context.builder, ret_val, indices.as_mut_ptr(), indices.len() as u32, &0);
    LLVMBuildStore(context.builder, LLVMConstInt(LLVMInt64Type(), tag as u64, 0), ptr);
    for i in 0 .. param_types.len() {
        let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), (i + 1) as u64, 0)];
        let ptr = LLVMBuildGEP(context.builder, ret_val, indices.as_mut_ptr(), indices.len() as u32, &0);
        LLVMBuildStore(context.builder, LLVMGetParam(func, i as u32), ptr);
    }
    LLVMBuildRet(context.builder, codegen_to_void_ptr(ret_val, context));
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let val = function_value(func, &Type::Function(param_types.clone(), Box::new(ret_type.clone())), context)?;
    context.names.push((name.to_string(), Var::Ctor(val, tag, type_params, param_types, ret_type)));
    Ok(())
}

pub struct Context {
    pub module: LLVMModuleRef,
    pub builder: LLVMBuilderRef,
    pub names: Vec<(String, Var)>
}

pub unsafe fn codegen_box(val: LLVMValueRef, llvm_type: LLVMTypeRef, context: &mut Context) -> LLVMValueRef {
    let ptr = LLVMBuildMalloc(context.builder, llvm_type, &0);
    LLVMBuildStore(context.builder, val, ptr);
    codegen_to_void_ptr(ptr, context)
}

pub unsafe fn function_value(func: LLVMValueRef, func_type: &Type, context: &mut Context) -> Result<LLVMValueRef> {
    let llvm_type = get_unboxed_llvm_type(func_type)?;
    let val = LLVMBuildMalloc(context.builder, llvm_type, &0);
    LLVMBuildStore(context.builder,
        LLVMBuildInsertValue(context.builder,
            LLVMBuildInsertValue(context.builder,
                LLVMGetUndef(llvm_type),
                func,
                0,
                &0,
            ),
            LLVMConstNull(void_ptr_type()),
            1,
            &0,
        ),
        val,
    );
    Ok(codegen_to_void_ptr(val, context))
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

pub unsafe fn codegen_decompose_list(list: LLVMValueRef, context: &mut Context) -> (LLVMValueRef, LLVMValueRef) {
    (LLVMBuildExtractValue(context.builder, list, 0, &0), LLVMBuildExtractValue(context.builder, list, 1, &0))
}

pub unsafe fn codegen_compose_list(
    length: LLVMValueRef,
    contents: LLVMValueRef,
    context: &mut Context
) -> LLVMValueRef {
    LLVMBuildInsertValue(context.builder,
        LLVMBuildInsertValue(context.builder,
            LLVMGetUndef(list_type()),
            length,
            0,
            &0,
        ),
        contents,
        1,
        &0,
    )
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

pub fn resolve_name(names: &Vec<(String, Var)>, name: &str) -> Result<Var> {
    names
        .iter()
        .rev()
        .find_map(|(key, value)| if *key == *name { Some(value.clone()) } else { None })
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
