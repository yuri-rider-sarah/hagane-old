use crate::codegen::util::*;
use crate::error::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

unsafe fn function_value(func: LLVMValueRef, func_type: &Type, context: &mut Context) -> Result<LLVMValueRef> {
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

unsafe fn codegen_box(val: LLVMValueRef, llvm_type: LLVMTypeRef, context: &mut Context) -> LLVMValueRef {
    let ptr = LLVMBuildMalloc(context.builder, llvm_type, &0);
    LLVMBuildStore(context.builder, val, ptr);
    codegen_to_void_ptr(ptr, context)
}

unsafe fn codegen_check(
    cond: LLVMValueRef,
    error_c_func: LLVMValueRef,
    func: LLVMValueRef,
    context: &mut Context,
) {
    let then_block = LLVMAppendBasicBlock(func, BB_C_NAME);
    let else_block = LLVMAppendBasicBlock(func, BB_C_NAME);
    LLVMBuildCondBr(context.builder, cond, then_block, else_block);
    LLVMPositionBuilderAtEnd(context.builder, then_block);
    LLVMBuildCall(context.builder, error_c_func, std::ptr::null_mut(), 0, &0);
    LLVMBuildUnreachable(context.builder);
    LLVMPositionBuilderAtEnd(context.builder, else_block);
}

unsafe fn codegen_copy_loop(
    src: LLVMValueRef,
    dest: LLVMValueRef,
    length: LLVMValueRef,
    func: LLVMValueRef,
    context: &mut Context
) {
    let mut entry_block = LLVMGetInsertBlock(context.builder);
    let test_block = LLVMAppendBasicBlock(func, BB_C_NAME);
    let mut loop_block = LLVMAppendBasicBlock(func, BB_C_NAME);
    let after_block = LLVMAppendBasicBlock(func, BB_C_NAME);
    LLVMBuildBr(context.builder, test_block);
    LLVMPositionBuilderAtEnd(context.builder, test_block);
    let mut i = LLVMBuildPhi(context.builder, LLVMInt64Type(), &0);
    LLVMBuildCondBr(context.builder,
        LLVMBuildICmp(context.builder, LLVMIntULT, i, length, &0),
        loop_block,
        after_block,
    );
    LLVMPositionBuilderAtEnd(context.builder, loop_block);
    LLVMBuildStore(context.builder,
        LLVMBuildLoad(context.builder, LLVMBuildGEP(context.builder, src, &mut i, 1, &0), &0),
        LLVMBuildGEP(context.builder, dest, &mut i, 1, &0),
    );
    let mut i1 = LLVMBuildAdd(context.builder, i, LLVMConstInt(LLVMInt64Type(), 1, 0), &0);
    LLVMAddIncoming(i, &mut LLVMConstInt(LLVMInt64Type(), 0, 0), &mut entry_block, 1);
    LLVMAddIncoming(i, &mut i1, &mut loop_block, 1);
    LLVMBuildBr(context.builder, test_block);
    LLVMPositionBuilderAtEnd(context.builder, after_block);
}

unsafe fn codegen_decompose_list(list: LLVMValueRef, context: &mut Context) -> (LLVMValueRef, LLVMValueRef) {
    (LLVMBuildExtractValue(context.builder, list, 0, &0), LLVMBuildExtractValue(context.builder, list, 1, &0))
}

unsafe fn codegen_compose_list(
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

unsafe fn codegen_primitive<F>(
    name: &str,
    type_params: Option<Vec<String>>,
    type_: Type,
    build: F,
    context: &mut Context,
) -> Result<()> where F: Fn(&mut Context) -> Result<LLVMValueRef> {
    let val = LLVMBuildMalloc(context.builder, get_unboxed_llvm_type(&type_)?, &0);
    LLVMBuildStore(context.builder, build(context)?, val);
    let type_ = match type_params {
        None => type_,
        Some(type_params) => Type::Forall(type_params, Box::new(type_)),
    };
    let var = Var::Const(codegen_to_void_ptr(val, context));
    context.names.push((name.to_string(), var, type_));
    Ok(())
}

unsafe fn codegen_primitive_function<F>(
    name: &str,
    type_params: Option<Vec<String>>,
    param_types: Vec<Type>,
    ret_type: Type,
    build: F,
    context: &mut Context,
) -> Result<()> where F: Fn(LLVMValueRef, &mut Context) -> Result<LLVMValueRef> {
    let parent = LLVMGetInsertBlock(context.builder);
    let func = create_function(context, &name, param_types.len())?;
    LLVMBuildRet(context.builder, build(func, context)?);
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let func_type = Type::Function(param_types, Box::new(ret_type));
    let val = function_value(func, &func_type, context)?;
    let type_ = match type_params {
        None => func_type,
        Some(type_params) => Type::Forall(type_params, Box::new(func_type)),
    };
    context.names.push((name.to_string(), Var::Const(val), type_));
    Ok(())
}

unsafe fn codegen_bool_constant_primitive(name: &str, n: u64, context: &mut Context) -> Result<()> {
    codegen_primitive(
        name,
        None,
        Type::Named("Bool".to_string()),
        |_| Ok(LLVMConstInt(LLVMInt1Type(), n, 0)),
        context,
    )
}

unsafe fn codegen_binary_arith_primitive(
    name: &str, 
    build: unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const i8) -> LLVMValueRef,
    context: &mut Context
) -> Result<()> {
    codegen_primitive_function(
        name,
        None,
        vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())],
        Type::Named("Int".to_string()),
        |func, context| Ok(codegen_box(
            build(context.builder,
                codegen_load(LLVMGetParam(func, 0), LLVMInt64Type(), context),
                codegen_load(LLVMGetParam(func, 1), LLVMInt64Type(), context),
                &0,
            ),
            LLVMInt64Type(),
            context,
        )),
        context,
    )
}

unsafe fn codegen_binary_cmp_primitive(name: &str, cmp: LLVMIntPredicate, context: &mut Context) -> Result<()> {
    codegen_primitive_function(
        name,
        None,
        vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())],
        Type::Named("Bool".to_string()),
        |func, context| Ok(codegen_box(
            LLVMBuildICmp(context.builder,
                cmp,
                codegen_load(LLVMGetParam(func, 0), LLVMInt64Type(), context),
                codegen_load(LLVMGetParam(func, 1), LLVMInt64Type(), context),
                &0,
            ),
            LLVMInt1Type(),
            context,
        )),
        context,
    )
}

pub unsafe fn codegen_primitives(context: &mut Context) -> Result<()> {
    codegen_bool_constant_primitive("⊥", 0, context)?;
    codegen_bool_constant_primitive("⊤", 1, context)?;
    codegen_binary_arith_primitive("+", LLVMBuildAdd, context)?;
    codegen_binary_arith_primitive("-", LLVMBuildSub, context)?;
    codegen_binary_arith_primitive("*", LLVMBuildMul, context)?;
    codegen_binary_arith_primitive("/", LLVMBuildSDiv, context)?;
    codegen_binary_arith_primitive("%", LLVMBuildSRem, context)?;
    codegen_binary_cmp_primitive("=", LLVMIntEQ, context)?;
    codegen_binary_cmp_primitive("≠", LLVMIntNE, context)?;
    codegen_binary_cmp_primitive("<", LLVMIntSLT, context)?;
    codegen_binary_cmp_primitive("≤", LLVMIntSLE, context)?;
    codegen_binary_cmp_primitive(">", LLVMIntSGT, context)?;
    codegen_binary_cmp_primitive("≥", LLVMIntSGE, context)?;
    let print_c_func = {
        let c_func_name = CString::new("print").unwrap();
        let mut func_param_types = vec![LLVMInt64Type()];
        let llvm_c_func_type = LLVMFunctionType(LLVMVoidType(), func_param_types.as_mut_ptr(), func_param_types.len() as u32, 0);
        LLVMAddFunction(context.module, c_func_name.as_ptr(), llvm_c_func_type)
    };
    codegen_primitive_function(
        "print",
        None,
        vec![Type::Named("Int".to_string())],
        Type::Tuple(Vec::new()),
        |func, context| {
            let mut args = vec![codegen_load(LLVMGetParam(func, 0), LLVMInt64Type(), context)];
            LLVMBuildCall(context.builder, print_c_func, args.as_mut_ptr(), args.len() as u32, &0);
            Ok(codegen_box(LLVMGetUndef(unit_type()), unit_type(), context))
        },
        context,
    )?;
    let read_c_func = {
        let c_func_name = CString::new("read").unwrap();
        let llvm_c_func_type = LLVMFunctionType(LLVMInt64Type(), std::ptr::null_mut(), 0, 0);
        LLVMAddFunction(context.module, c_func_name.as_ptr(), llvm_c_func_type)
    };
    codegen_primitive_function(
        "read",
        None,
        vec![],
        Type::Named("Int".to_string()),
        |_, context| Ok(codegen_box(LLVMBuildCall(context.builder, read_c_func, std::ptr::null_mut(), 0, &0), LLVMInt64Type(), context)),
        context,
    )?;
    codegen_primitive(
        "empty",
        Some(vec!["T".to_string()]),
        Type::Applied(Box::new(Type::Named("List".to_string())), vec![Type::Named("T".to_string())]),
        |context| Ok(LLVMBuildInsertValue(context.builder,
            LLVMBuildInsertValue(context.builder,
                LLVMGetUndef(list_type()),
                LLVMConstInt(LLVMInt64Type(), 0, 0),
                0,
                &0,
            ),
            LLVMConstNull(void_ptr_type()),
            1,
            &0,
        )),
        context,
    )?;
    let list_t_type = Type::Applied(Box::new(Type::Named("List".to_string())), vec![Type::Named("T".to_string())]);
    codegen_primitive_function(
        "len",
        Some(vec!["T".to_string()]),
        vec![list_t_type.clone()],
        Type::Named("Int".to_string()),
        |func, context| {
            let length = LLVMBuildExtractValue(context.builder,
                codegen_load(LLVMGetParam(func, 0), list_type(), context),
                0,
                &0,
            );
            Ok(codegen_box(length, LLVMInt64Type(), context))
        },
        context,
    )?;
    let bounds_error_c_func = {
        let c_func_name = CString::new("bounds_error").unwrap();
        let llvm_c_func_type = LLVMFunctionType(LLVMVoidType(), std::ptr::null_mut(), 0, 0);
        LLVMAddFunction(context.module, c_func_name.as_ptr(), llvm_c_func_type)
    };
    codegen_primitive_function(
        "get",
        Some(vec!["T".to_string()]),
        vec![list_t_type.clone(), Type::Named("Int".to_string())],
        Type::Named("T".to_string()),
        |func, context| {
            let (length, contents) = codegen_decompose_list(
                codegen_load(LLVMGetParam(func, 0), list_type(), context),
                context,
            );
            let mut i = codegen_load(LLVMGetParam(func, 1), LLVMInt64Type(), context);
            codegen_check(
                LLVMBuildICmp(context.builder, LLVMIntUGE, i, length, &0),
                bounds_error_c_func,
                func,
                context,
            );
            Ok(LLVMBuildLoad(context.builder,
                LLVMBuildGEP(context.builder, contents, &mut i, 1, &0),
                &0,
            ))
        },
        context,
    )?;
    codegen_primitive_function(
        "put",
        Some(vec!["T".to_string()]),
        vec![list_t_type.clone(), Type::Named("Int".to_string()), Type::Named("T".to_string())],
        list_t_type.clone(),
        |func, context| {
            let (length, old_contents) = codegen_decompose_list(
                codegen_load(LLVMGetParam(func, 0), list_type(), context),
                context,
            );
            let mut i = codegen_load(LLVMGetParam(func, 1), LLVMInt64Type(), context);
            codegen_check(
                LLVMBuildICmp(context.builder, LLVMIntUGE, i, length, &0),
                bounds_error_c_func,
                func,
                context,
            );
            let contents = LLVMBuildArrayMalloc(context.builder, void_ptr_type(), length, &0);
            codegen_copy_loop(old_contents, contents, length, func, context);
            LLVMBuildStore(context.builder,
                LLVMGetParam(func, 2),
                LLVMBuildGEP(context.builder, contents, &mut i, 1, &0),
            );
            Ok(codegen_box(codegen_compose_list(length, contents, context), list_type(), context))
        },
        context,
    )?;
    codegen_primitive_function(
        "push",
        Some(vec!["T".to_string()]),
        vec![list_t_type.clone(), Type::Named("T".to_string())],
        list_t_type.clone(),
        |func, context| {
            let (mut old_length, old_contents) = codegen_decompose_list(
                codegen_load(LLVMGetParam(func, 0), list_type(), context),
                context,
            );
            let length = LLVMBuildAdd(context.builder, old_length, LLVMConstInt(LLVMInt64Type(), 1, 0), &0);
            let contents = LLVMBuildArrayMalloc(context.builder, void_ptr_type(), length, &0);
            codegen_copy_loop(old_contents, contents, old_length, func, context);
            LLVMBuildStore(context.builder,
                LLVMGetParam(func, 1),
                LLVMBuildGEP(context.builder, contents, &mut old_length, 1, &0),
            );
            Ok(codegen_box(codegen_compose_list(length, contents, context), list_type(), context))
        },
        context,
    )?;
    codegen_primitive_function(
        "pop",
        Some(vec!["T".to_string()]),
        vec![list_t_type.clone()],
        list_t_type.clone(),
        |func, context| {
            let (old_length, old_contents) = codegen_decompose_list(
                codegen_load(LLVMGetParam(func, 0), list_type(), context),
                context,
            );
            codegen_check(
                LLVMBuildICmp(context.builder, LLVMIntEQ, old_length, LLVMConstInt(LLVMInt64Type(), 0, 0), &0),
                bounds_error_c_func,
                func,
                context,
            );
            let length = LLVMBuildSub(context.builder, old_length, LLVMConstInt(LLVMInt64Type(), 1, 0), &0);
            let contents = LLVMBuildArrayMalloc(context.builder, void_ptr_type(), length, &0);
            codegen_copy_loop(old_contents, contents, length, func, context);
            Ok(codegen_box(codegen_compose_list(length, contents, context), list_type(), context))
        },
        context,
    )?;
    Ok(())
}
