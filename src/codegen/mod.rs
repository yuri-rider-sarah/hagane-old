mod primitives;
mod util;

use crate::codegen::primitives::*;
use crate::codegen::util::*;
use crate::error::*;
use crate::parser::Expr;
use crate::parser::UExpr::*;
use crate::parser::Pattern;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::LLVMIntPredicate::*;

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

unsafe fn codegen_pattern_test(
    val: LLVMValueRef,
    type_: &Type,
    pattern: &Pattern,
    context: &mut Context,
) -> Result<Vec<(LLVMBasicBlockRef, LLVMValueRef, LLVMBasicBlockRef)>> {
    use crate::parser::Pattern::*;
    let parent_block = LLVMGetInsertBlock(context.builder);
    let function = LLVMGetBasicBlockParent(parent_block);
    Ok(match pattern {
        IntLiteral(n) => {
            if type_ != &Type::Named("Int".to_string()) {
                return Err(Error::ConflictingType(type_.clone(), Type::Named("Int".to_string())));
            }
            let test_val = LLVMBuildICmp(context.builder,
                LLVMIntEQ,
                codegen_load(val, LLVMInt64Type(), context),
                LLVMConstInt(LLVMInt64Type(), *n as u64, 0),
                &0,
            );
            let then_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, then_block);
            vec![(parent_block, test_val, then_block)]
        },
        Ident(name) => {
            context.names.push((name.clone(), Var::Const(val, type_.clone())));
            vec![]
        },
        Wildcard => vec![],
        Tuple(patterns) => {
            let subtypes = match type_ {
                Type::Tuple(subtypes) => subtypes,
                _ => return Err(Error::ConflictingPatternType),
            };
            if patterns.len() != subtypes.len() {
                return Err(Error::ConflictingPatternType);
            }
            let tuple = LLVMBuildBitCast(context.builder, val, LLVMPointerType(get_unboxed_llvm_type(type_)?, 0), &0);
            let mut branches = vec![];
            for (i, (subpattern, subtype)) in Iterator::zip(patterns.iter(), subtypes).enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let subval = LLVMBuildLoad(context.builder,
                    LLVMBuildGEP(context.builder, tuple, indices.as_mut_ptr(), indices.len() as u32, &0),
                    &0,
                );
                branches.append(&mut codegen_pattern_test(subval, subtype, subpattern, context)?);
            }
            branches
        },
        Variant(ctor, patterns) => {
            let (tag, subtypes) = match resolve_name(&context.names, &**ctor)? {
                Var::Ctor(_, tag, type_params, subtypes, ret_type) => {
                    match type_params {
                        Some(type_params) => {
                            match type_ {
                                Type::Applied(base_type, type_args) => {
                                    if **base_type != ret_type || type_params.len() != type_args.len() {
                                        return Err(Error::ConflictingPatternType);
                                    }
                                    let substitutions = Iterator::zip(type_params.into_iter(), type_args.clone().into_iter()).collect();
                                    (tag, subtypes.iter().map(|t| substitute_in_type(t, &substitutions)).collect())
                                },
                                _ => return Err(Error::ConflictingPatternType),
                            }
                        },
                        None => {
                            if type_ != &ret_type {
                                return Err(Error::ConflictingPatternType);
                            }
                            (tag, subtypes)
                        },
                    }
                },
                _ => return Err(Error::NotAConstructor(ctor.to_string())),
            };
            let tag_ptr = LLVMBuildBitCast(context.builder, val, LLVMPointerType(LLVMInt64Type(), 0), &0);
            let tag_test = LLVMBuildICmp(context.builder,
                LLVMIntEQ,
                LLVMBuildLoad(context.builder, tag_ptr, &0),
                LLVMConstInt(LLVMInt64Type(), tag, 0),
                &0,
            );
            let then_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, then_block);
            let mut branches = vec![(parent_block, tag_test, then_block)];
            let variant = LLVMBuildBitCast(context.builder, val, LLVMPointerType(get_llvm_variant_type(subtypes.len()), 0), &0);
            for (i, (subpattern, subtype)) in Iterator::zip(patterns.iter(), subtypes).enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), (i + 1) as u64, 0)];
                let subval = LLVMBuildLoad(context.builder,
                    LLVMBuildGEP(context.builder, variant, indices.as_mut_ptr(), indices.len() as u32, &0),
                    &0,
                );
                branches.append(&mut codegen_pattern_test(subval, &subtype, subpattern, context)?);
            }
            branches
        },
        List(patterns) => {
            let elem_type = match type_ {
                Type::Applied(type_list, elem_type)
                    if **type_list == Type::Named("List".to_string()) && elem_type.len() == 1 => 
                        elem_type[0].clone(),
                _ => return Err(Error::ConflictingPatternType),
            };
            let (length, contents) = codegen_decompose_list(codegen_load(val, list_type(), context), context);
            let length_test = LLVMBuildICmp(context.builder,
                LLVMIntEQ,
                length,
                LLVMConstInt(LLVMInt64Type(), patterns.len() as u64, 0),
                &0
            );
            let then_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, then_block);
            let mut branches = vec![(parent_block, length_test, then_block)];
            for (i, subpattern) in patterns.iter().enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), i as u64, 0)];
                let subval = LLVMBuildLoad(context.builder,
                    LLVMBuildGEP(context.builder, contents, indices.as_mut_ptr(), indices.len() as u32, &0),
                    &0,
                );
                branches.append(&mut codegen_pattern_test(subval, &elem_type, subpattern, context)?);
            }
            branches
        },
    })
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
            let var = resolve_name(&context.names, name)?;
            (var_value(&var, context), var_type(var))
        },
        Wildcard => return Err(Error::InvalidExpr),
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
        List(exprs) => {
            let mut type_ = stated_type.clone();
            let length = LLVMConstInt(LLVMInt64Type(), exprs.len() as u64, 0);
            let contents = LLVMBuildArrayMalloc(context.builder, void_ptr_type(), length, &0);
            for (i, expr) in exprs.iter().enumerate() {
                let (elem, elem_type) = codegen_block(expr, context)?;
                match type_ {
                    Some(type_) if elem_type != type_ => return Err(Error::ConflictingType(elem_type, type_.clone())),
                    Some(_) => (),
                    None => type_ = Some(elem_type),
                }
                LLVMBuildStore(context.builder,
                    elem,
                    LLVMBuildGEP(context.builder, contents, &mut LLVMConstInt(LLVMInt32Type(), i as u64, 0), 1, &0)
                );
            }
            (codegen_box(codegen_compose_list(length, contents, context), list_type(), context), match type_ {
                Some(type_) => Type::Applied(Box::new(Type::Named("List".to_string())), vec![type_]),
                None => return Err(Error::AmbiguousType),
            })
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
                    let var = resolve_name(&context.names, name)?;
                    let name_type = var_type(var.clone());
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
                    (var_value(&var, context), type_)
                },
                _ => return Err(Error::NotPolymorphic),
            }
        },
        Let(type_params, pattern, exprs, false) => match type_params {
            None => {
                let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(context.builder));
                let (value, type_) = codegen_block(exprs, context)?;
                let test_blocks = codegen_pattern_test(value, &type_, pattern, context)?;
                let test_end = LLVMGetInsertBlock(context.builder);
                let else_block = LLVMAppendBasicBlock(function, BB_C_NAME);
                for (test_block, cond_val, then_block) in test_blocks {
                    LLVMPositionBuilderAtEnd(context.builder, test_block);
                    LLVMBuildCondBr(context.builder, cond_val, then_block, else_block);
                }
                LLVMPositionBuilderAtEnd(context.builder, else_block);
                let error_c_func_name = CString::new("case_error").unwrap();
                let error_c_func = LLVMGetNamedFunction(context.module, error_c_func_name.as_ptr());
                LLVMBuildCall(context.builder, error_c_func, std::ptr::null_mut(), 0, &0);
                LLVMBuildUnreachable(context.builder);
                let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
                LLVMPositionBuilderAtEnd(context.builder, test_end);
                LLVMBuildBr(context.builder, merge_block);
                LLVMPositionBuilderAtEnd(context.builder, merge_block);
                codegen_unit(context)
            },
            Some(type_params) => match pattern {
                Pattern::Ident(name) => {
                    let (value, monotype) = codegen_block(exprs, context)?;
                    let type_ = Type::Forall(type_params.clone(), Box::new(monotype));
                    context.names.push((name.clone(), Var::Const(value, type_)));
                    codegen_unit(context)
                },
                _ => return Err(Error::AssignToExpr),
            },
        },
        Let(None, pattern, exprs, true) => match pattern {
            Pattern::Ident(name) => {
                let (value, type_) = codegen_block(exprs, context)?;
                let c_name = CString::new(&name[..]).unwrap();
                let ptr = LLVMBuildMalloc(context.builder, void_ptr_type(), c_name.as_ptr());
                LLVMBuildStore(context.builder, value, ptr);
                let var = Var::Mut(ptr, type_);
                context.names.push((name.clone(), var));
                codegen_unit(context)
            },
            _ => return Err(Error::AssignToExpr),
        },
        Let(Some(_), _, _, true) => return Err(Error::PolymorphicMut),
        Set(name, exprs) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (var, var_type) = match resolve_name(&context.names, name)? {
                    Var::Mut(var, var_type) => (var, var_type),
                    Var::Const(_, _) | Var::Ctor(_, _, _, _, _) => return Err(Error::AssignToConst),
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
        Type(type_name, type_params, variants) => {
            for (i, (variant_name, param_types)) in variants.iter().enumerate() {
                codegen_var_ctor(
                    i as u64,
                    type_params.clone(),
                    param_types.clone(),
                    Type::Named(type_name.clone()),
                    variant_name,
                    context
                )?;
            }
            codegen_unit(context)
        },
        Extern(name, type_) => match type_ {
            Type::Function(param_types, ret_type) => {
                codegen_c_function(name, param_types.clone(), *ret_type.clone(), context)?;
                codegen_unit(context)
            }
            _ => return Err(Error::TypeNotInFFI(type_.clone())),
        }
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
        Cond(cases) => {
            codegen_branching(
                cases,
                |expr, context| {
                    let (cond_val_ptr, cond_type) = codegen(expr, context)?;
                    if cond_type != Type::Named("Bool".to_string()) {
                        return Err(Error::ConflictingType(cond_type, Type::Named("Bool".to_string())));
                    }
                    let cond_val = codegen_load(cond_val_ptr, LLVMInt1Type(), context);
                    let parent_block = LLVMGetInsertBlock(context.builder);
                    let function = LLVMGetBasicBlockParent(parent_block);
                    let then_block = LLVMAppendBasicBlock(function, BB_C_NAME);
                    LLVMPositionBuilderAtEnd(context.builder, then_block);
                    Ok(vec![(parent_block, cond_val, then_block)])
                },
                stated_type.clone(),
                context
            )?
        },
        Match(test_expr, cases) => {
            let (val, type_) = codegen(test_expr, context)?;
            codegen_branching(
                cases,
                |pattern, context| codegen_pattern_test(val, &type_, pattern, context),
                stated_type.clone(),
                context
            )?
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
            let cond_end = LLVMGetInsertBlock(context.builder);
            let body_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, body_block);
            let _ = codegen_block(body, context)?;
            LLVMBuildBr(context.builder, cond_block);
            let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
            LLVMPositionBuilderAtEnd(context.builder, cond_end);
            LLVMBuildCondBr(context.builder, cond_val, body_block, merge_block);
            LLVMPositionBuilderAtEnd(context.builder, merge_block);
            codegen_unit(context)
        },
        Do(body) => codegen_block(body, context)?,
        Lambda(expr_params, body) => {
            let parent = LLVMGetInsertBlock(context.builder);
            let param_types_: Vec<_> = expr_params.iter().map(|Expr(_, t)| t.clone()).collect();
            let param_types = match stated_type {
                Some(type_) => match &type_ {
                    Type::Function(param_types, _) => {
                        if param_types.len() != expr_params.len() {
                            return Err(Error::MismatchedParamsNum(param_types.len(), expr_params.len()));
                        }
                        for (param_type_, param_type) in Iterator::zip(param_types_.iter(), param_types.iter()) {
                            check_type_compat(param_type_, param_type)?;
                        }
                        param_types.clone()
                    },
                    _ => return Err(Error::NotAFunction(type_.clone())),
                },
                None => {
                    let mut param_types = Vec::new();
                    for param_type_ in param_types_ {
                        param_types.push(param_type_.ok_or(Error::AmbiguousType)?);
                    }
                    param_types
                },
            };
            let mut captures_types = Vec::new();
            for (_, var) in &context.names {
                captures_types.push(match var {
                    Var::Const(_, _) => void_ptr_type(),
                    Var::Mut(_, _) => LLVMPointerType(void_ptr_type(), 0),
                    Var::Ctor(_, _, _, _, _) => void_ptr_type(),
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
            let mut inner_names: Vec<_> = context.names.iter().enumerate().map(|(i, (name, var))| {
                let c_name = CString::new(&name[..]).unwrap();
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, captures, indices.as_mut_ptr(), indices.len() as u32, c_name.as_ptr());
                let val = LLVMBuildLoad(context.builder, ptr, c_name.as_ptr());
                (name.clone(), match var {
                    Var::Const(_, type_) => Var::Const(val, type_.clone()),
                    Var::Mut(_, type_) => Var::Mut(val, type_.clone()),
                    Var::Ctor(_, i, type_params, param_types, ret_type) =>
                        Var::Ctor(val, *i, type_params.clone(), param_types.clone(), ret_type.clone()),
                })
            }).collect();
            for (i, (Expr(name, _), param_type)) in Iterator::zip(expr_params.iter(), param_types.iter()).enumerate() {
                match name {
                    Ident(name) => {
                        let value = LLVMGetParam(func, i as u32);
                        LLVMSetValueName2(value, name.as_ptr() as *const i8, name.len());
                        inner_names.push((name.clone(), Var::Const(value, param_type.clone())));
                    },
                    _ => return Err(Error::AssignToExpr),
                }
            }
            let (ret_val, ret_type) = codegen_block(body, &mut Context { names: inner_names, .. *context })?;
            LLVMBuildRet(context.builder, ret_val);
            match stated_type {
                Some(type_) => match &type_ {
                    Type::Function(_, stated_ret_type) => {
                        if **stated_ret_type != ret_type {
                            return Err(Error::ConflictingType(*stated_ret_type.clone(), ret_type));
                        }
                    }
                    _ => (),
                }
                _ => (),
            };
            LLVMPositionBuilderAtEnd(context.builder, parent);
            let captures = LLVMBuildMalloc(context.builder, captures_type, &0);
            for (i, (_, var)) in context.names.iter().enumerate() {
                let mut indices = vec![LLVMConstInt(LLVMInt64Type(), 0, 0), LLVMConstInt(LLVMInt32Type(), i as u64, 0)];
                let ptr = LLVMBuildGEP(context.builder, captures, indices.as_mut_ptr(), indices.len() as u32, &0);
                LLVMBuildStore(context.builder, match var {
                    Var::Const(val, _) => *val,
                    Var::Mut(val, _) => *val,
                    Var::Ctor(val, _, _, _, _) => *val,
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

pub unsafe fn codegen_branching<T, F>(
    cases: &Vec<(T, Vec<Expr>)>,
    codegen_test: F,
    mut type_: Option<Type>,
    context: &mut Context
) -> Result<(LLVMValueRef, Type)>
where F: Fn(&T, &mut Context) -> Result<Vec<(LLVMBasicBlockRef, LLVMValueRef, LLVMBasicBlockRef)>> {
    let function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(context.builder));
    let mut branches = Vec::new();
    for (cond, then) in cases {
        let old_names_len = context.names.len();
        let test_blocks = codegen_test(cond, context)?;
        let (branch_val, branch_type) = codegen_block(then, context)?;
        match type_ {
            Some(type_) if branch_type != type_ => return Err(Error::ConflictingType(branch_type, type_)),
            Some(_) => (),
            None => type_ = Some(branch_type),
        }
        let branch_end = LLVMGetInsertBlock(context.builder);
        branches.push((branch_val, branch_end));
        context.names.truncate(old_names_len);
        let else_block = LLVMAppendBasicBlock(function, BB_C_NAME);
        for (test_block, cond_val, then_block) in test_blocks {
            LLVMPositionBuilderAtEnd(context.builder, test_block);
            LLVMBuildCondBr(context.builder, cond_val, then_block, else_block);
        }
        LLVMPositionBuilderAtEnd(context.builder, else_block);
    }
    let error_c_func_name = CString::new("case_error").unwrap();
    let error_c_func = LLVMGetNamedFunction(context.module, error_c_func_name.as_ptr());
    LLVMBuildCall(context.builder, error_c_func, std::ptr::null_mut(), 0, &0);
    LLVMBuildUnreachable(context.builder);
    let merge_block = LLVMAppendBasicBlock(function, BB_C_NAME);
    LLVMPositionBuilderAtEnd(context.builder, merge_block);
    let phi = LLVMBuildPhi(context.builder, void_ptr_type(), &0);
    for (mut val, mut block) in branches {
        LLVMPositionBuilderAtEnd(context.builder, block);
        LLVMBuildBr(context.builder, merge_block);
        LLVMAddIncoming(phi, &mut val, &mut block, 1);
    }
    LLVMPositionBuilderAtEnd(context.builder, merge_block);
    Ok((phi, match type_ {
        Some(t) => t,
        None => return Err(Error::AmbiguousType),
    }))
}
