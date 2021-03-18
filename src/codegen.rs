use crate::error::*;
use crate::parser::Expr;
use crate::parser::UExpr::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

const ENTRY_C_NAME: *const i8 = b"entry\0" as *const u8 as *const i8;
const BB_C_NAME: *const i8 = b"bb\0" as *const u8 as *const i8;

#[derive(Clone, Copy)]
enum Var {
    Const(LLVMValueRef),
    Mut(LLVMValueRef),
}

struct Context {
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    names: Vec<(String, Var, Type)>
}

unsafe fn create_function(context: &mut Context, name: &str, params_num: usize) -> Result<LLVMValueRef> {
    let func_c_name = CString::new(name).unwrap();
    let func = LLVMAddFunction(context.module, func_c_name.as_ptr(), get_llvm_function_type(params_num));
    LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetFunctionCallConv(func, LLVMCallConv::LLVMFastCallConv as u32);
    let entry = LLVMAppendBasicBlock(func, ENTRY_C_NAME);
    LLVMPositionBuilderAtEnd(context.builder, entry);
    Ok(func)
}

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

unsafe fn codegen_primitives(context: &mut Context) -> Result<()> {
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

pub unsafe fn codegen_top(exprs: &Vec<Expr>) -> Result<LLVMModuleRef> {
    let module_c_name = CString::new("module").unwrap();
    let module = LLVMModuleCreateWithName(module_c_name.as_ptr());
    let main_func_type = LLVMFunctionType(LLVMVoidType(), std::ptr::null_mut(), 0, 0);
    let func_c_name = CString::new("hagane_main").unwrap();
    let func = LLVMAddFunction(module, func_c_name.as_ptr(), main_func_type);
    let entry = LLVMAppendBasicBlock(func, ENTRY_C_NAME);
    let builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);
    let mut context = Context { module, builder, names: Vec::new() };
    codegen_primitives(&mut context)?;
    let (_, ret_type) = codegen_block(exprs, &mut context)?;
    if ret_type != Type::Tuple(Vec::new()) {
        return Err(Error::ConflictingType(ret_type, Type::Tuple(Vec::new())));
    }
    LLVMBuildRetVoid(builder);
    Ok(module)
}

unsafe fn codegen(Expr(uexpr, stated_type): &Expr, context: &mut Context) -> Result<(LLVMValueRef, Type)> {
    let (value, inferred_type) = match uexpr {
        IntLiteral(n) => {
            let type_ = Type::Named("Int".to_string());
            let val = LLVMBuildMalloc(context.builder, get_unboxed_llvm_type(&type_)?, &0);
            LLVMBuildStore(context.builder, LLVMConstInt(LLVMInt64Type(), *n as u64, 0), val);
            (codegen_to_void_ptr(val, context), type_)
        },
        Ident(name) => {
            let (var, type_) = resolve_name(&context.names, name)?;
            (match var {
                Var::Const(val) => val,
                Var::Mut(ptr) => LLVMBuildLoad(context.builder, ptr, &0),
            }, type_)
        },
        Tuple(exprs) => {
            let (values, types) = codegen_group(exprs, context)?;
            let type_ = Type::Tuple(types);
            let tuple = LLVMBuildMalloc(context.builder, get_unboxed_llvm_type(&type_)?, &0);
            for (i, &element) in values.iter().enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, tuple, indices.as_mut_ptr(), indices.len() as u32, &0);
                LLVMBuildStore(context.builder, element, ptr);
            }
            (codegen_to_void_ptr(tuple, context), type_)
        },
        Function(_, _) => return Err(Error::InvalidExpr),
        Call(func, args) => {
            let (func_val_ptr, func_type) = codegen(func, context)?;
            let (mut arg_values, arg_types) = codegen_group(args, context)?;
            let ret_type = match func_type.clone() {
                Type::Function(param_types, ret_type) => if param_types == arg_types {
                    *ret_type
                } else {
                    return Err(Error::MismatchedArgs(param_types, arg_types))
                },
                _ => return Err(Error::NotAFunction(func_type)),
            };
            let func_val = codegen_load(func_val_ptr, get_unboxed_llvm_type(&func_type)?, context);
            let func_ptr = LLVMBuildExtractValue(context.builder, func_val, 0, &0);
            arg_values.push(LLVMBuildExtractValue(context.builder, func_val, 1, &0));
            let call = LLVMBuildCall(context.builder, func_ptr, arg_values.as_mut_ptr(), arg_values.len() as u32, &0);
            LLVMSetInstructionCallConv(call, LLVMCallConv::LLVMFastCallConv as u32);
            (call, ret_type)
        },
        Monomorphic(expr, args) => {
            let Expr(uexpr, stated_type) = &**expr;
            match uexpr {
                Ident(name) => {
                    let (var, name_type) = resolve_name(&context.names, name)?;
                    check_type_compat(stated_type, &name_type)?;
                    let type_ = match name_type {
                        Type::Forall(params, name_type) => {
                            if params.len() != args.len() {
                                return Err(Error::MismatchedTypeArgsNum(params.len(), args.len()));
                            }
                            let substitutions = Iterator::zip(params.clone().into_iter(), args.clone().into_iter()).collect();
                            substitute_in_type(&*name_type, &substitutions)
                        },
                        _ => Type::Applied(Box::new(name_type), args.clone()),
                    };
                    (match var {
                        Var::Const(val) => val,
                        Var::Mut(ptr) => LLVMBuildLoad(context.builder, ptr, &0),
                    }, type_)
                },
                _ => return Err(Error::NotPolymorphic),
            }
        },
        Let(name, exprs, false) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, type_) = codegen_block(exprs, context)?;
                check_type_compat(stated_type, &type_)?;
                context.names.push((name.clone(), Var::Const(value), type_));
                codegen_unit(context)
            },
            _ => return Err(Error::AssignToExpr),
        },
        Let(name, exprs, true) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, type_) = codegen_block(exprs, context)?;
                check_type_compat(stated_type, &type_)?;
                let c_name = CString::new(&name[..]).unwrap();
                let ptr = LLVMBuildMalloc(context.builder, void_ptr_type(), c_name.as_ptr());
                LLVMBuildStore(context.builder, value, ptr);
                let var = Var::Mut(ptr);
                context.names.push((name.clone(), var, type_));
                codegen_unit(context)
            },
            _ => return Err(Error::AssignToExpr),
        },
        Set(name, exprs) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (var, var_type) = match resolve_name(&context.names, name)? {
                    (Var::Mut(var), var_type) => (var, var_type),
                    _ => return Err(Error::AssignToConst),
                };
                let (val, val_type) = codegen_block(exprs, context)?;
                if val_type != var_type {
                    return Err(Error::ConflictingType(var_type, val_type));
                }
                check_type_compat(stated_type, &var_type)?;
                LLVMBuildStore(context.builder, val, var);
                codegen_unit(context)
            },
            _ => return Err(Error::AssignToExpr),
        },
        If(cond, then, else_) => {
            let (cond_val_ptr, cond_type) = codegen_block(cond, context)?;
            if cond_type != Type::Named("Bool".to_string()) {
                return Err(Error::ConflictingType(cond_type, Type::Named("Bool".to_string())));
            }
            let cond_val = codegen_load(cond_val_ptr, LLVMInt1Type(), context);
            let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(context.builder));
            let parent_block = LLVMGetInsertBlock(context.builder);
            let then_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, then_block);
            let (mut then_val, then_type) = codegen_block(then, context)?;
            let mut then_end = LLVMGetInsertBlock(context.builder);
            let else_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, else_block);
            let (mut else_val, else_type) = codegen_block(else_, context)?;
            let mut else_end = LLVMGetInsertBlock(context.builder);
            if then_type != else_type {
                return Err(Error::ConflictingType(then_type, else_type));
            }
            LLVMPositionBuilderAtEnd(context.builder, parent_block);
            LLVMBuildCondBr(context.builder, cond_val, then_block, else_block);
            let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, then_end);
            LLVMBuildBr(context.builder, merge_block);
            LLVMPositionBuilderAtEnd(context.builder, else_end);
            LLVMBuildBr(context.builder, merge_block);
            LLVMPositionBuilderAtEnd(context.builder, merge_block);
            let phi = LLVMBuildPhi(context.builder, void_ptr_type(), &0);
            LLVMAddIncoming(phi, &mut then_val, &mut then_end, 1);
            LLVMAddIncoming(phi, &mut else_val, &mut else_end, 1);
            (phi, then_type)
        },
        While(cond, body) => {
            let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(context.builder));
            let cond_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMBuildBr(context.builder, cond_block);
            LLVMPositionBuilderAtEnd(context.builder, cond_block);
            let (cond_val_ptr, cond_type) = codegen_block(cond, context)?;
            if cond_type != Type::Named("Bool".to_string()) {
                return Err(Error::ConflictingType(cond_type, Type::Named("Bool".to_string())));
            }
            let cond_val = codegen_load(cond_val_ptr, LLVMInt1Type(), context);
            let body_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, body_block);
            let _ = codegen_block(body, context)?;
            LLVMBuildBr(context.builder, cond_block);
            LLVMPositionBuilderAtEnd(context.builder, cond_block);
            let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMBuildCondBr(context.builder, cond_val, body_block, merge_block);
            LLVMPositionBuilderAtEnd(context.builder, merge_block);
            codegen_unit(context)
        },
        Do(body) => codegen_block(body, context)?,
        Lambda(expr_params, body) => {
            let parent = LLVMGetInsertBlock(context.builder);
            let param_types_: Vec<_> = expr_params.iter().map(|Expr(_, t)| t.clone()).collect();
            let ret_type_ = match body.last() {
                Some(Expr(_, t)) => t.clone(),
                None => Some(Type::Tuple(Vec::new())),
            };
            let (param_types, ret_type) = match stated_type {
                Some(type_) => match &type_ {
                    Type::Function(param_types, ret_type) => {
                        if param_types.len() != expr_params.len() {
                            return Err(Error::MismatchedParamsNum(param_types.len(), expr_params.len()));
                        }
                        for (param_type_, param_type) in Iterator::zip(param_types_.iter(), param_types.iter()) {
                            check_type_compat(param_type_, param_type)?;
                        }
                        check_type_compat(&ret_type_, ret_type)?;
                        (param_types.clone(), *ret_type.clone())
                    },
                    _ => return Err(Error::NotAFunction(type_.clone())),
                },
                None => {
                    let mut param_types = Vec::new();
                    for param_type_ in param_types_ {
                        param_types.push(param_type_.ok_or(Error::AmbiguousType)?);
                    }
                    (param_types, ret_type_.ok_or(Error::AmbiguousType)?)
                },
            };
            let mut captures_types = Vec::new();
            for (_, var, _) in &context.names {
                captures_types.push(match var {
                    Var::Const(_) => void_ptr_type(),
                    Var::Mut(_) => LLVMPointerType(void_ptr_type(), 0),
                });
            }
            let func = create_function(context, "lambda", param_types.len())?;
            let captures_type = LLVMStructType(
                captures_types.as_mut_ptr(),
                captures_types.len() as u32,
                0,
            );
            let captures = LLVMBuildBitCast(
                context.builder,
                LLVMGetParam(func, expr_params.len() as u32),
                LLVMPointerType(captures_type, 0),
                &0,
            );
            let mut inner_names: Vec<_> = context.names.iter().enumerate().map(|(i, (name, var, type_))| {
                let c_name = CString::new(&name[..]).unwrap();
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, captures, indices.as_mut_ptr(), indices.len() as u32, c_name.as_ptr());
                let val = LLVMBuildLoad(context.builder, ptr, c_name.as_ptr());
                (name.clone(), match var {
                    Var::Const(_) => Var::Const(val),
                    Var::Mut(_) => Var::Mut(val),
                }, type_.clone())
            }).collect();
            for (i, (Expr(name, _), param_type)) in Iterator::zip(expr_params.iter(), param_types.iter()).enumerate() {
                match name {
                    Ident(name) => {
                        let value = LLVMGetParam(func, i as u32);
                        LLVMSetValueName2(value, name.as_ptr() as *const i8, name.len());
                        inner_names.push((name.clone(), Var::Const(value), param_type.clone()));
                    },
                    _ => return Err(Error::AssignToExpr),
                }
            }
            let (ret_val, inferred_ret_type) = codegen_block(body, &mut Context { names: inner_names, .. *context })?;
            LLVMBuildRet(context.builder, ret_val);
            if ret_type != inferred_ret_type {
                return Err(Error::ConflictingType(inferred_ret_type, ret_type.clone()));
            }
            LLVMPositionBuilderAtEnd(context.builder, parent);
            let captures = LLVMBuildMalloc(context.builder, captures_type, &0);
            for (i, (_, var, _)) in context.names.iter().enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, captures, indices.as_mut_ptr(), indices.len() as u32, &0);
                LLVMBuildStore(context.builder, match var {
                    Var::Const(val) => *val,
                    Var::Mut(val) => *val,
                }, ptr);
            }
            let type_ = Type::Function(param_types, Box::new(ret_type));
            let llvm_type = get_unboxed_llvm_type(&type_)?;
            let func_val = LLVMBuildMalloc(context.builder, llvm_type, &0);
            LLVMBuildStore(context.builder,
                LLVMBuildInsertValue(context.builder,
                    LLVMBuildInsertValue(context.builder,
                        LLVMGetUndef(llvm_type),
                        func,
                        0,
                        &0,
                    ),
                    codegen_to_void_ptr(captures, context),
                    1,
                    &0,
                ),
                func_val,
            );
            (codegen_to_void_ptr(func_val, context), type_)
        },
    };
    check_type_compat(stated_type, &inferred_type)?;
    Ok((value, inferred_type))
}

unsafe fn codegen_group(exprs: &Vec<Expr>, context: &mut Context) -> Result<(Vec<LLVMValueRef>, Vec<Type>)> {
    let mut values = Vec::new();
    let mut types = Vec::new();
    for expr in exprs {
        let (value, type_) = codegen(expr, context)?;
        values.push(value);
        types.push(type_);
    }
    Ok((values, types))
}

unsafe fn codegen_block(exprs: &Vec<Expr>, context: &mut Context) -> Result<(LLVMValueRef, Type)> {
    let old_names_len = context.names.len();
    let mut result = codegen_unit(context);
    for expr in exprs {
        result = codegen(expr, context)?;
    }
    context.names.truncate(old_names_len);
    Ok(result)
}

unsafe fn codegen_to_void_ptr(val: LLVMValueRef, context: &mut Context) -> LLVMValueRef {
    LLVMBuildBitCast(context.builder, val, LLVMPointerType(LLVMInt8Type(), 0), &0)
}

unsafe fn codegen_unit(context: &mut Context) -> (LLVMValueRef, Type) {
    (codegen_to_void_ptr(LLVMBuildMalloc(context.builder, unit_type(), &0), context), Type::Tuple(Vec::new()))
}

unsafe fn codegen_load(ptr: LLVMValueRef, llvm_type: LLVMTypeRef, context: &mut Context) -> LLVMValueRef {
    LLVMBuildLoad(context.builder, LLVMBuildBitCast(context.builder, ptr, LLVMPointerType(llvm_type, 0), &0), &0)
}

fn resolve_name(names: &Vec<(String, Var, Type)>, name: &str) -> Result<(Var, Type)> {
    names
        .iter()
        .rev()
        .find_map(|(key, value, type_)| if *key == *name { Some((*value, type_.clone())) } else { None })
        .ok_or(Error::UnboundVariable(name.to_string()))
}

fn check_type_compat(stated_type: &Option<Type>, inferred_type: &Type) -> Result<()> {
    match stated_type {
        Some(stated_type) if stated_type != inferred_type => Err(Error::ConflictingType(stated_type.clone(), inferred_type.clone())),
        _ => Ok(()),
    }
}

unsafe fn unit_type() -> LLVMTypeRef {
    LLVMStructType(std::ptr::null_mut(), 0, 0)
}

unsafe fn void_ptr_type() -> LLVMTypeRef {
    LLVMPointerType(LLVMInt8Type(), 0)
}

unsafe fn list_type() -> LLVMTypeRef {
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

unsafe fn get_unboxed_llvm_type(type_: &Type) -> Result<LLVMTypeRef> {
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

unsafe fn get_llvm_function_type(params: usize) -> LLVMTypeRef {
    let mut llvm_types = vec![void_ptr_type(); params + 1];
    LLVMFunctionType(
        void_ptr_type(),
        llvm_types.as_mut_ptr(),
        llvm_types.len() as u32,
        0,
    )
}

fn substitute_in_type(type_: &Type, substitutions: &Vec<(String, Type)>) -> Type {
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
