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

unsafe fn create_function(context: &mut Context, name: &str, param_types: &Vec<Type>, ret_type: &Type) -> Result<LLVMValueRef> {
    let func_c_name = CString::new(name).unwrap();
    let func = LLVMAddFunction(context.module, func_c_name.as_ptr(), get_llvm_function_type(param_types, ret_type)?);
    LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetFunctionCallConv(func, LLVMCallConv::LLVMFastCallConv as u32);
    let entry = LLVMAppendBasicBlock(func, ENTRY_C_NAME);
    LLVMPositionBuilderAtEnd(context.builder, entry);
    Ok(func)
}

unsafe fn codegen_bool_constant_primitive(name: &str, n: u64, context: &mut Context) -> Result<()> {
    let type_ = Type::Named("Bool".to_string());
    let val = LLVMConstInt(LLVMInt1Type(), n, 0);
    context.names.push((name.to_string(), Var::Const(val), type_));
    Ok(())
}

unsafe fn function_value(func: LLVMValueRef, llvm_type: LLVMTypeRef, context: &mut Context) -> LLVMValueRef {
    LLVMBuildInsertValue(
        context.builder,
        LLVMBuildInsertValue(
            context.builder,
            LLVMGetUndef(llvm_type),
            func,
            0,
            &0,
        ),
        LLVMConstNull(LLVMPointerType(LLVMInt8Type(), 0)),
        1,
        &0,
    )
}

unsafe fn codegen_binary_arith_primitive(name: &str, build: unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const i8) -> LLVMValueRef, context: &mut Context) -> Result<()> {
    let parent = LLVMGetInsertBlock(context.builder);
    let param_types = vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())];
    let ret_type = Type::Named("Int".to_string());
    let func = create_function(context, &name, &param_types, &ret_type)?;
    LLVMBuildRet(context.builder, build(context.builder, LLVMGetParam(func, 0), LLVMGetParam(func, 1), &0));
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let func_type = Type::Function(param_types, Box::new(ret_type));
    let llvm_type = get_llvm_type(&func_type)?;
    let val = function_value(func, llvm_type, context);
    context.names.push((name.to_string(), Var::Const(val), func_type));
    Ok(())
}

unsafe fn codegen_binary_cmp_primitive(name: &str, cmp: LLVMIntPredicate, context: &mut Context) -> Result<()> {
    let parent = LLVMGetInsertBlock(context.builder);
    let param_types = vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())];
    let ret_type = Type::Named("Bool".to_string());
    let func = create_function(context, &name, &param_types, &ret_type)?;
    LLVMBuildRet(context.builder, LLVMBuildICmp(context.builder, cmp, LLVMGetParam(func, 0), LLVMGetParam(func, 1), &0));
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let func_type = Type::Function(param_types, Box::new(ret_type));
    let llvm_type = get_llvm_type(&func_type)?;
    let val = function_value(func, llvm_type, context);
    context.names.push((name.to_string(), Var::Const(val), func_type));
    Ok(())
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
    {
        let parent = LLVMGetInsertBlock(context.builder);
        let func_c_name = CString::new("print").unwrap();
        let mut func_param_types = vec![LLVMInt64Type()];
        let param_types = vec![Type::Named("Int".to_string())];
        let llvm_c_func_type = LLVMFunctionType(LLVMVoidType(), func_param_types.as_mut_ptr(), func_param_types.len() as u32, 0);
        let c_func = LLVMAddFunction(context.module, func_c_name.as_ptr(), llvm_c_func_type);
        let func = create_function(context, "print", &param_types, &Type::Tuple(Vec::new()))?;
        let mut args = vec![LLVMGetParam(func, 0)];
        LLVMBuildCall(context.builder, c_func, args.as_mut_ptr(), args.len() as u32, &0);
        LLVMBuildRet(context.builder, codegen_unit().0);
        LLVMPositionBuilderAtEnd(context.builder, parent);
        let func_type = Type::Function(param_types, Box::new(Type::Tuple(Vec::new())));
        let llvm_type = get_llvm_type(&func_type)?;
        let val = function_value(func, llvm_type, context);
        context.names.push(("print".to_string(), Var::Const(val), func_type));
    }
    {
        let parent = LLVMGetInsertBlock(context.builder);
        let func_c_name = CString::new("read").unwrap();
        let llvm_c_func_type = LLVMFunctionType(LLVMInt64Type(), std::ptr::null_mut(), 0, 0);
        let c_func = LLVMAddFunction(context.module, func_c_name.as_ptr(), llvm_c_func_type);
        let func = create_function(context, "read", &Vec::new(), &Type::Named("Int".to_string()))?;
        let ret_val = LLVMBuildCall(context.builder, c_func, std::ptr::null_mut(), 0, &0);
        LLVMBuildRet(context.builder, ret_val);
        LLVMPositionBuilderAtEnd(context.builder, parent);
        let func_type = Type::Function(Vec::new(), Box::new(Type::Named("Int".to_string())));
        let llvm_type = get_llvm_type(&func_type)?;
        let val = function_value(func, llvm_type, context);
        context.names.push(("read".to_string(), Var::Const(val), func_type));
    }
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
        IntLiteral(n) => (LLVMConstInt(LLVMInt64Type(), *n as u64, 0), Type::Named("Int".to_string())),
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
            let llvm_type = get_llvm_type(&type_)?;
            let mut tuple = LLVMGetUndef(llvm_type);
            for (i, &element) in values.iter().enumerate() {
                tuple = LLVMBuildInsertValue(context.builder, tuple, element, i as u32, &0);
            }
            (tuple, type_)
        },
        Function(_, _) => return Err(Error::InvalidExpr),
        Call(func, args) => {
            let (func_val, func_type) = codegen(func, context)?;
            let (mut arg_values, arg_types) = codegen_group(args, context)?;
            let ret_type = match func_type {
                Type::Function(param_types, ret_type) => if param_types == arg_types {
                    *ret_type
                } else {
                    return Err(Error::MismatchedArgs(param_types, arg_types))
                },
                _ => return Err(Error::NotAFunction(func_type)),
            };
            let func_ptr = LLVMBuildExtractValue(context.builder, func_val, 0, &0);
            arg_values.push(LLVMBuildExtractValue(context.builder, func_val, 1, &0));
            let call = LLVMBuildCall(context.builder, func_ptr, arg_values.as_mut_ptr(), arg_values.len() as u32, &0);
            LLVMSetInstructionCallConv(call, LLVMCallConv::LLVMFastCallConv as u32);
            (call, ret_type)
        },
        Let(name, exprs, false) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, type_) = codegen_block(exprs, context)?;
                check_type_compat(stated_type, &type_)?;
                context.names.push((name.clone(), Var::Const(value), type_));
                codegen_unit()
            },
            _ => return Err(Error::AssignToExpr),
        },
        Let(name, exprs, true) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, type_) = codegen_block(exprs, context)?;
                check_type_compat(stated_type, &type_)?;
                let c_name = CString::new(&name[..]).unwrap();
                let ptr = LLVMBuildMalloc(context.builder, get_llvm_type(&type_)?, c_name.as_ptr());
                LLVMBuildStore(context.builder, value, ptr);
                context.names.push((name.clone(), Var::Mut(ptr), type_));
                codegen_unit()
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
                codegen_unit()
            },
            _ => return Err(Error::AssignToExpr),
        },
        If(cond, then, else_) => {
            let (cond_val, cond_type) = codegen_block(cond, context)?;
            if cond_type != Type::Named("Bool".to_string()) {
                return Err(Error::ConflictingType(cond_type, Type::Named("Bool".to_string())));
            }
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
            let phi = LLVMBuildPhi(context.builder, get_llvm_type(&then_type)?, &0);
            LLVMAddIncoming(phi, &mut then_val, &mut then_end, 1);
            LLVMAddIncoming(phi, &mut else_val, &mut else_end, 1);
            (phi, then_type)
        },
        While(cond, body) => {
            let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(context.builder));
            let cond_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMBuildBr(context.builder, cond_block);
            LLVMPositionBuilderAtEnd(context.builder, cond_block);
            let (cond_val, cond_type) = codegen_block(cond, context)?;
            if cond_type != Type::Named("Bool".to_string()) {
                return Err(Error::ConflictingType(cond_type, Type::Named("Bool".to_string())));
            }
            let body_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, body_block);
            let _ = codegen_block(body, context)?;
            LLVMBuildBr(context.builder, cond_block);
            LLVMPositionBuilderAtEnd(context.builder, cond_block);
            let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMBuildCondBr(context.builder, cond_val, body_block, merge_block);
            LLVMPositionBuilderAtEnd(context.builder, merge_block);
            codegen_unit()
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
            for (_, var, type_) in &context.names {
                captures_types.push(match var {
                    Var::Const(_) => get_llvm_type(type_)?,
                    Var::Mut(_) => LLVMPointerType(get_llvm_type(type_)?, 0),
                });
            }
            let func = create_function(context, "lambda", &param_types, &ret_type)?;
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
                let mut indices = vec![LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
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
                let mut indices = vec![LLVMConstInt(LLVMInt32Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, captures, indices.as_mut_ptr(), indices.len() as u32, &0);
                LLVMBuildStore(context.builder, match var {
                    Var::Const(val) => *val,
                    Var::Mut(val) => *val,
                }, ptr);
            }
            let type_ = Type::Function(param_types, Box::new(ret_type));
            let func_val = LLVMBuildInsertValue(
                context.builder,
                LLVMBuildInsertValue(
                    context.builder,
                    LLVMGetUndef(get_llvm_type(&type_)?),
                    func,
                    0,
                    &0,
                ),
                LLVMBuildBitCast(
                    context.builder,
                    captures,
                    LLVMPointerType(LLVMInt8Type(), 0),
                    &0,
                ),
                1,
                &0,
            );
            (func_val, type_)
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
    let mut result = codegen_unit();
    for expr in exprs {
        result = codegen(expr, context)?;
    }
    context.names.truncate(old_names_len);
    Ok(result)
}

unsafe fn codegen_unit() -> (LLVMValueRef, Type) {
    (LLVMGetUndef(LLVMStructType(std::ptr::null_mut(), 0, 0)), Type::Tuple(Vec::new()))
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

unsafe fn get_llvm_type(type_: &Type) -> Result<LLVMTypeRef> {
    Ok(match type_ {
        Type::Named(name) => match &name[..] {
            "Int" => LLVMInt64Type(),
            "Bool" => LLVMInt1Type(),
            _ => return Err(Error::UnboundType(name.clone())),
        }
        Type::Tuple(types) => {
            let mut llvm_types = Vec::new();
            for type_ in types {
                llvm_types.push(get_llvm_type(type_)?);
            }
            LLVMStructType(
                llvm_types.as_mut_ptr(),
                types.len() as u32,
                0,
            )
        },
        Type::Function(params, ret) => {
            let mut contents = [
                LLVMPointerType(get_llvm_function_type(params, ret)?, 0),
                LLVMPointerType(LLVMInt8Type(), 0),
            ];
            LLVMStructType(
                contents.as_mut_ptr(),
                contents.len() as u32,
                0,
            )
        },
    })
}

unsafe fn get_llvm_function_type(params: &Vec<Type>, ret: &Type) -> Result<LLVMTypeRef> {
    let mut llvm_types = Vec::new();
    for type_ in params {
        llvm_types.push(get_llvm_type(type_)?);
    }
    llvm_types.push(LLVMPointerType(LLVMInt8Type(), 0));
    Ok(LLVMFunctionType(
        get_llvm_type(ret)?,
        llvm_types.as_mut_ptr(),
        llvm_types.len() as u32,
        0,
    ))
}
