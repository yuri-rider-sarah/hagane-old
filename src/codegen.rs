use crate::error::*;
use crate::parser::Expr;
use crate::parser::UExpr::*;
use crate::parser::Type;
use std::ffi::CString;
use llvm_sys::*;
use llvm_sys::LLVMIntPredicate::*;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

struct Context {
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    names: Vec<(String, LLVMValueRef, Type)>
}

unsafe fn create_function(context: &mut Context, name: &str, param_types: &Vec<Type>, ret_type: &Type) -> Result<LLVMValueRef> {
    let func_c_name = CString::new(name).unwrap();
    let func = LLVMAddFunction(context.module, func_c_name.as_ptr(), get_llvm_function_type(param_types, ret_type)?);
    LLVMSetLinkage(func, LLVMLinkage::LLVMPrivateLinkage);
    LLVMSetFunctionCallConv(func, LLVMCallConv::LLVMFastCallConv as u32);
    let entry_c_name = CString::new("entry").unwrap();
    let entry = LLVMAppendBasicBlock(func, entry_c_name.as_ptr());
    LLVMPositionBuilderAtEnd(context.builder, entry);
    Ok(func)
}

unsafe fn codegen_bool_constant_primitive(name: &str, n: u64, context: &mut Context) -> Result<()> {
    let c_name = CString::new(name).unwrap();
    let type_ = Type::Named("Bool".to_string());
    let val = LLVMConstInt(LLVMInt1Type(), n, 0);
    let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&type_)?, c_name.as_ptr());
    LLVMBuildStore(context.builder, val, ptr);
    context.names.push((name.to_string(), ptr, type_));
    Ok(())
}

unsafe fn codegen_binary_arith_primitive(name: &str, build: unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const i8) -> LLVMValueRef, context: &mut Context) -> Result<()> {
    let parent = LLVMGetInsertBlock(context.builder);
    let c_name = CString::new(name).unwrap();
    let param_types = vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())];
    let ret_type = Type::Named("Int".to_string());
    let func = create_function(context, &name, &param_types, &ret_type)?;
    LLVMBuildRet(context.builder, build(context.builder, LLVMGetParam(func, 0), LLVMGetParam(func, 1), &0));
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let func_type = Type::Function(param_types, Box::new(ret_type));
    let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&func_type)?, c_name.as_ptr());
    LLVMBuildStore(context.builder, func, ptr);
    context.names.push((name.to_string(), ptr, func_type));
    Ok(())
}

unsafe fn codegen_binary_cmp_primitive(name: &str, cmp: LLVMIntPredicate, context: &mut Context) -> Result<()> {
    let parent = LLVMGetInsertBlock(context.builder);
    let c_name = CString::new(name).unwrap();
    let param_types = vec![Type::Named("Int".to_string()), Type::Named("Int".to_string())];
    let ret_type = Type::Named("Bool".to_string());
    let func = create_function(context, &name, &param_types, &ret_type)?;
    LLVMBuildRet(context.builder, LLVMBuildICmp(context.builder, cmp, LLVMGetParam(func, 0), LLVMGetParam(func, 1), &0));
    LLVMPositionBuilderAtEnd(context.builder, parent);
    let func_type = Type::Function(param_types, Box::new(ret_type));
    let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&func_type)?, c_name.as_ptr());
    LLVMBuildStore(context.builder, func, ptr);
    context.names.push((name.to_string(), ptr, func_type));
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
        let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&func_type)?, &0);
        LLVMBuildStore(context.builder, func, ptr);
        context.names.push(("print".to_string(), ptr, func_type));
    }
    Ok(())
}

pub unsafe fn codegen_top(exprs: &Vec<Expr>) -> Result<LLVMModuleRef> {
    let c_name = CString::new("module").unwrap();
    let module = LLVMModuleCreateWithName(c_name.as_ptr());
    let main_func_type = LLVMFunctionType(LLVMVoidType(), std::ptr::null_mut(), 0, 0);
    let func_c_name = CString::new("hagane_main").unwrap();
    let func = LLVMAddFunction(module, func_c_name.as_ptr(), main_func_type);
    let entry_c_name = CString::new("entry").unwrap();
    let entry = LLVMAppendBasicBlock(func, entry_c_name.as_ptr());
    let builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, entry);
    let mut context = Context { module, builder, names: Vec::new() };
    codegen_primitives(&mut context)?;
    let (_, ret_type) = codegen_block(exprs, &mut context)?;
    if ret_type != Type::Tuple(Vec::new()) {
        return Err(Error::ConflictingType(ret_type, Type::Tuple(Vec::new())));
    }
    LLVMPositionBuilderAtEnd(builder, entry);
    LLVMBuildRetVoid(builder);
    Ok(module)
}

unsafe fn codegen(Expr(uexpr, stated_type): &Expr, context: &mut Context) -> Result<(LLVMValueRef, Type)> {
    let (value, inferred_type) = match uexpr {
        IntLiteral(n) => (LLVMConstInt(LLVMInt64Type(), *n as u64, 0), Type::Named("Int".to_string())),
        Ident(name) => {
            let (ptr, type_) = resolve_name(&context.names, name)?;
            let c_name = CString::new(&name[..]).unwrap();
            (LLVMBuildLoad(context.builder, ptr, c_name.as_ptr()), type_)
        },
        Tuple(exprs) => {
            let (values, types) = codegen_group(exprs, context)?;
            let type_ = Type::Tuple(types);
            let llvm_type = get_llvm_type(&type_)?;
            let mut tuple = LLVMGetUndef(llvm_type);
            for (i, &element) in values.iter().enumerate() {
                let c_name = CString::new("tuple").unwrap();
                tuple = LLVMBuildInsertValue(context.builder, tuple, element, i as u32, c_name.as_ptr());
            }
            (tuple, type_)
        },
        Call(func, args) => {
            let (func_value, func_type) = codegen(func, context)?;
            let (mut arg_values, arg_types) = codegen_group(args, context)?;
            let ret_type = match func_type {
                Type::Function(param_types, ret_type) => if param_types == arg_types {
                    *ret_type
                } else {
                    return Err(Error::MismatchedArgs(param_types, arg_types))
                },
                _ => return Err(Error::NotAFunction(func_type)),
            };
            let c_name = CString::new("call").unwrap();
            let call = LLVMBuildCall(context.builder, func_value, arg_values.as_mut_ptr(), args.len() as u32, c_name.as_ptr());
            LLVMSetInstructionCallConv(call, LLVMCallConv::LLVMFastCallConv as u32);
            (call, ret_type)
        },
        Let(name, expr) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (value, type_) = codegen(expr, context)?;
                check_type_compat(stated_type, &type_)?;
                let c_name = CString::new(&name[..]).unwrap();
                let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&type_)?, c_name.as_ptr());
                LLVMBuildStore(context.builder, value, ptr);
                context.names.push((name.clone(), ptr, type_));
                codegen_unit()
            },
            _ => return Err(Error::AssignToExpr),
        },
        Set(name, expr) => match &**name {
            Expr(Ident(name), stated_type) => {
                let (var, var_type) = resolve_name(&context.names, name)?;
                let (val, val_type) = codegen(expr, context)?;
                if val_type != var_type {
                    return Err(Error::ConflictingType(var_type, val_type));
                }
                check_type_compat(stated_type, &var_type)?;
                LLVMBuildStore(context.builder, val, var);
                codegen_unit()
            },
            _ => return Err(Error::AssignToExpr),
        },
        Match(_expr, _branches) => return Err(Error::Unimplemented),
        While(_expr, _body) => return Err(Error::Unimplemented),
        Do(body) => codegen_block(body, context)?,
        Lambda(expr_params, body) => {
            let parent = LLVMGetInsertBlock(context.builder);
            let type_ = stated_type.clone().ok_or(Error::AmbiguousType)?;
            let (param_types, ret_type) = match &type_ {
                Type::Function(param_types, ret_type) => {
                    if param_types.len() != expr_params.len() {
                        return Err(Error::MismatchedParamsNum(param_types.len(), expr_params.len()));
                    }
                    (param_types, ret_type)
                },
                _ => return Err(Error::NotAFunction(type_)),
            };
            let func = create_function(context, "lambda", param_types, ret_type)?;
            let old_names_len = context.names.len();
            for (i, (Expr(name, stated_param_type), param_type)) in Iterator::zip(expr_params.iter(), param_types.iter()).enumerate() {
                match name {
                    Ident(name) => {
                        check_type_compat(&stated_param_type, param_type)?;
                        let value = LLVMGetParam(func, i as u32);
                        let c_name = CString::new(&name[..]).unwrap();
                        let ptr = LLVMBuildAlloca(context.builder, get_llvm_type(&param_type)?, c_name.as_ptr());
                        LLVMBuildStore(context.builder, value, ptr);
                        LLVMSetValueName2(value, name.as_ptr() as *const i8, name.len());
                        context.names.push((name.clone(), ptr, param_type.clone()));
                    },
                    _ => return Err(Error::AssignToExpr),
                }
            }
            let (ret_val, inferred_ret_type) = codegen_block(body, context)?;
            LLVMBuildRet(context.builder, ret_val);
            if **ret_type != inferred_ret_type {
                return Err(Error::ConflictingType(inferred_ret_type, *ret_type.clone()));
            }
            context.names.truncate(old_names_len);
            LLVMPositionBuilderAtEnd(context.builder, parent);
            (func, type_)
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

fn resolve_name(names: &Vec<(String, LLVMValueRef, Type)>, name: &str) -> Result<(LLVMValueRef, Type)> {
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
        Type::Function(params, ret) => LLVMPointerType(get_llvm_function_type(params, ret)?, 0),
    })
}

unsafe fn get_llvm_function_type(params: &Vec<Type>, ret: &Type) -> Result<LLVMTypeRef> {
    let mut llvm_types = Vec::new();
    for type_ in params {
        llvm_types.push(get_llvm_type(type_)?);
    }
    Ok(LLVMFunctionType(
        get_llvm_type(ret)?,
        llvm_types.as_mut_ptr(),
        llvm_types.len() as u32,
        0,
    ))
}
