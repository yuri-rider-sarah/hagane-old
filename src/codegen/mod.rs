mod primitives;
mod util;

use crate::codegen::util::*;
use crate::error::*;
use crate::parser::Expr;
use crate::parser::UExpr::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

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
    primitives::codegen_primitives(&mut context)?;
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
        Let(type_params, name, exprs, false) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, monotype) = codegen_block(exprs, context)?;
                let type_ = match type_params {
                    None => monotype,
                    Some(type_params) => Type::Forall(type_params.clone(), Box::new(monotype)),
                };
                check_type_compat(stated_type, &type_)?;
                context.names.push((name.clone(), Var::Const(value), type_));
                codegen_unit(context)
            },
            _ => return Err(Error::AssignToExpr),
        },
        Let(None, name, exprs, true) => match &**name {
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
        Let(Some(_), _, _, true) => return Err(Error::PolymorphicMut),
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
