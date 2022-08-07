use thiserror::Error;
use tracing::instrument;

use crate::{
    ast::AstCtx,
    parse::parser,
    resolve::{ResolutionError, Resolver},
    typeck::TypeCtx,
};

#[instrument]
pub fn compile<'a>(input: &'a str) -> Result<(), CompileError> {
    let ast = AstCtx::new();
    let program = parser::program(input, &ast).unwrap();
    let resolver = Resolver::new(&program);
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
}
