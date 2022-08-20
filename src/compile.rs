#[cfg(test)]
mod test;

use std::mem;

use thiserror::Error;
use tracing::instrument;

use crate::{
    ast::AstCtx,
    jit::{JITError, JIT},
    parse::parser,
    resolve::{ResolutionError, Resolver},
    typeck::TypeCtx,
    validate::AstValidator,
};

#[instrument]
pub fn compile<'a>(input: &'a str) -> Result<(), CompileError> {
    let code = compile_code(input)?;

    unsafe {
        let ret: i64 = run_code(code, ());
        tracing::info!("return = {ret:?}");
    }
    Ok(())
}

fn compile_code<'a>(input: &'a str) -> Result<*const u8, CompileError> {
    let ast = AstCtx::new();
    let program = parser::program(input, &ast).unwrap();
    let resolver = Resolver::new(&program);
    let mut validator = AstValidator::new(&resolver);

    if let Err(errors) = validator.validate(&program) {
        for error in errors {
            tracing::error!("Error: {error}");
        }
        return Err(CompileError::ValidationError);
    }

    let ty = TypeCtx::new(&ast, &resolver);
    for stmt in &program.stmts {
        let t = ty
            .type_of_stmt(stmt)
            .map_err(|e| CompileError::TypeError(e.to_string()))?;
        tracing::debug!("{t:#?}");
    }

    let mut jit = JIT::new(&ty)?;

    Ok(jit.compile(&program)?)
}

#[cfg(test)]
fn compile_standalone_fun<'a>(source: &'a str) -> Result<*const u8, CompileError> {
    let ast = AstCtx::new();
    let fun = parser::standalone_fun(source, &ast).unwrap();
    let resolver = Resolver::new(&fun);
    let mut validator = AstValidator::new(&resolver);

    if let Err(errors) = validator.validate(&fun) {
        for error in errors {
            tracing::error!("Error: {error}");
        }
        return Err(CompileError::ValidationError);
    }

    let ty = TypeCtx::new(&ast, &resolver);

    let mut jit = JIT::new(&ty)?;

    let code = jit.compile_standalone_fun(&fun)?;

    Ok(code)
}

unsafe fn run_code<I, O>(code: *const u8, input: I) -> O {
    let code_fn = mem::transmute::<_, fn(I) -> O>(code);
    code_fn(input)
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("TypeError occurred: {0}")]
    TypeError(String),

    #[error("ResolutionError occurred")]
    ResolutionError(#[from] ResolutionError),

    #[error("Validation error")]
    ValidationError,

    #[error("JIT error occurred: {0}")]
    JITError(#[from] JITError),
}
