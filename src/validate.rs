use std::{error::Error, mem};

use crate::{
    ast::visit::{Visit, Visitor},
    resolve::Resolver,
};

pub struct AstValidator<'ast> {
    resolver: &'ast Resolver,
    errors: Vec<Box<dyn Error>>,
}

impl<'ast> AstValidator<'ast> {
    pub fn new(resolver: &'ast Resolver) -> Self {
        Self {
            resolver,
            errors: Vec::new(),
        }
    }

    pub fn validate<T: Visit<'ast> + 'ast>(
        &mut self,
        ast: &'ast T,
    ) -> Result<(), Vec<Box<dyn Error>>> {
        ast.visit(self);
        let errors = mem::take(&mut self.errors);
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

impl<'ast> Visitor<'ast> for AstValidator<'ast> {
    fn visit_ident(&mut self, name: &'ast crate::ast::Name) {
        if let Err(e) = self.resolver.resolve(name) {
            self.errors.push(Box::new(e));
        }
    }
}
