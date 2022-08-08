use thiserror::Error;
use tracing::instrument;

use crate::{
    ast::AstCtx,
    parse::parser,
    resolve::{ResolutionError, Resolver},
    typeck::TypeCtx,
    validate::AstValidator,
};

#[instrument]
pub fn compile<'a>(input: &'a str) -> Result<(), CompileError> {
    let ast = AstCtx::new();
    let program = parser::program(input, &ast).unwrap();
    let resolver = Resolver::new(&program);
    let mut validator = AstValidator::new(&resolver);

    if let Err(errors) = validator.validate(&program) {
        for error in errors {
            eprintln!("Error: {error}");
        }
        return Err(CompileError::ValidationError);
    }

    let ty = TypeCtx::new(&ast, &resolver);
    for stmt in &program.stmts {
        let t = ty
            .type_of_stmt(stmt)
            .map_err(|e| CompileError::TypeError(e.to_string()))?;
        println!("{t:#?}");
    }
    Ok(())
}

#[derive(Error, Clone, Debug, PartialEq)]
pub enum CompileError {
    #[error("TypeError occurred: {0}")]
    TypeError(String),

    #[error("ResolutionError occurred")]
    ResolutionError(#[from] ResolutionError),

    #[error("Validation error")]
    ValidationError,
}
