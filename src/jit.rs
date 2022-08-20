use std::{collections::HashMap, mem};

use cranelift::{codegen::ir::FuncRef, prelude::*};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, FuncId, Linkage, Module, ModuleError};
use thiserror::Error;
use tracing::instrument;

use crate::{
    ast::{
        context::{ExprRef, StmtRef, TypeRef},
        Call, Fun, Program,
    },
    resolve::Binding,
    typeck::{TypeCtx, TypeError},
};

pub struct JIT<'ty, 'ast> {
    fn_ctx: FunctionBuilderContext,

    ctx: codegen::Context,

    _data_ctx: DataContext,

    module: JITModule,

    ty_ctx: &'ty TypeCtx<'ast>,
}

impl<'ty, 'ast> std::fmt::Debug for JIT<'ty, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JIT").finish()
    }
}

impl<'ty, 'ast> JIT<'ty, 'ast> {
    pub fn new(ty_ctx: &'ty TypeCtx<'ast>) -> JITResult<Self> {
        let mut flag_builder = settings::builder();
        // On at least AArch64, "colocated" calls use shorter-range relocations,
        // which might not reach all definitions; we can't handle that here, so
        // we require long-range relocation types.
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);

        Ok(Self {
            fn_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            _data_ctx: DataContext::new(),
            module,
            ty_ctx,
        })
    }
}

const INT: Type = types::I64;

impl<'ty, 'ast> JIT<'ty, 'ast> {
    #[instrument]
    pub fn compile(&mut self, program: &'ast Program<'ast>) -> JITResult<*const u8> {
        self.ctx.func.signature.returns.push(AbiParam::new(INT));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fn_ctx);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);

        builder.seal_block(entry_block);

        let zero = builder.ins().iconst(INT, 0);

        let mut translator = Translator::new(builder, &mut self.module, &self.ty_ctx);

        let mut last = Unit;
        for &stmt in &program.stmts {
            last = translator.translate_stmt(stmt)?;
        }

        match last {
            JITValue::Val(val) => translator.builder.ins().return_(&[val]),
            JITValue::Fun(_) => todo!(),
            Unit => translator.builder.ins().return_(&[zero]),
        };

        tracing::info!("{:?}", translator.builder.func);

        let (to_build, state) = translator.finish();

        let id = self
            .module
            .declare_function("main", Linkage::Export, &self.ctx.func.signature)?;

        self.module.define_function(id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);

        let mut state = state;
        for (id, fun) in to_build {
            state = self.compile_fun(id, fun, state)?;
        }

        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    pub fn compile_standalone_fun(&mut self, fun: &'ast Fun<'ast>) -> JITResult<*const u8> {
        let Fun { args, ret, .. } = fun;
        let state = TranslationState::new();

        for arg in args {
            let arg_ty = ty_to_clif(arg.ty)?;
            self.ctx.func.signature.params.push(AbiParam::new(arg_ty));
        }
        let ret_ty = ty_to_clif(ret)?;
        self.ctx.func.signature.returns.push(AbiParam::new(ret_ty));
        let func =
            self.module
                .declare_function("fun", Linkage::Export, &self.ctx.func.signature)?;
        self.module.clear_signature(&mut self.ctx.func.signature);
        let _ = self.compile_fun(func, fun, state)?;
        self.module.finalize_definitions();

        Ok(self.module.get_finalized_function(func))
    }

    #[instrument]
    fn compile_fun(
        &mut self,
        id: FuncId,
        fun: &'ast Fun<'ast>,
        translation_state: TranslationState<'ast>,
    ) -> JITResult<TranslationState<'ast>> {
        let Fun { args, ret, body } = fun;

        for arg in args {
            let arg_ty = ty_to_clif(arg.ty)?;
            self.ctx.func.signature.params.push(AbiParam::new(arg_ty));
        }
        let ret_ty = ty_to_clif(ret)?;
        self.ctx.func.signature.returns.push(AbiParam::new(ret_ty));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.fn_ctx);

        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);

        builder.append_block_params_for_function_params(entry_block);

        builder.seal_block(entry_block);

        let params = builder.block_params(entry_block).to_vec();
        let mut translator =
            Translator::with_state(builder, &mut self.module, &self.ty_ctx, translation_state);
        for (idx, arg) in args.iter().enumerate() {
            let binding = self.ty_ctx.resolve(&arg.name)?;
            let arg_var = translator.declare_variable(binding, arg.ty);
            translator.builder.def_var(arg_var, params[idx]);
        }

        let ret = translator.translate_expr(body)?;

        match ret {
            JITValue::Val(val) => translator.builder.ins().return_(&[val]),
            JITValue::Fun(_) => todo!(),
            Unit => translator.builder.ins().return_(&[]),
        };

        println!("Fun = {:?}", translator.builder.func);

        let (to_build, state) = translator.finish();

        let mut state = state;
        for (id, fun) in to_build {
            state = self.compile_fun(id, fun, state)?;
        }

        self.module.define_function(id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);

        Ok(state)
    }
    // fn translate_expr(&mut self, builder: &mut FunctionBuilder, )
}

pub struct Translator<'m, 'ty, 'ast> {
    module: &'m mut JITModule,
    ty_ctx: &'ty TypeCtx<'ast>,
    builder: FunctionBuilder<'m>,
    variables: HashMap<Binding, Variable>,
    var_idx: usize,
    funcs: HashMap<Binding, FuncRef>,
    all_funcs: HashMap<Binding, FuncId>,
    to_build: Vec<(FuncId, &'ast Fun<'ast>)>,
}

impl<'m, 'ty, 'ast> std::fmt::Debug for Translator<'m, 'ty, 'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Translator")
            .field("variables", &self.variables)
            .field("var_idx", &self.var_idx)
            .field("funcs", &self.funcs)
            .field("to_build", &self.to_build)
            .finish()
    }
}

#[derive(Debug)]
pub struct TranslationState<'ast> {
    variables: HashMap<Binding, Variable>,
    var_idx: usize,
    to_build: Vec<(FuncId, &'ast Fun<'ast>)>,
    all_funcs: HashMap<Binding, FuncId>,
}

impl<'ast> TranslationState<'ast> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            var_idx: 0,
            to_build: Vec::new(),
            all_funcs: HashMap::new(),
        }
    }
}

impl<'m, 'ty, 'ast> Translator<'m, 'ty, 'ast> {
    pub fn new(
        builder: FunctionBuilder<'m>,
        module: &'m mut JITModule,
        ty_ctx: &'ty TypeCtx<'ast>,
    ) -> Self {
        Self {
            module,
            ty_ctx,
            variables: HashMap::new(),
            var_idx: 0,
            funcs: HashMap::new(),
            to_build: Vec::new(),
            all_funcs: HashMap::new(),
            builder,
        }
    }

    pub fn with_state(
        builder: FunctionBuilder<'m>,
        module: &'m mut JITModule,
        ty_ctx: &'ty TypeCtx<'ast>,
        state: TranslationState<'ast>,
    ) -> Self {
        Self {
            module,
            ty_ctx,
            builder,
            variables: state.variables,
            var_idx: state.var_idx,
            all_funcs: state.all_funcs,
            to_build: state.to_build,
            funcs: HashMap::new(),
        }
    }

    pub fn finish(mut self) -> (Vec<(FuncId, &'ast Fun<'ast>)>, TranslationState<'ast>) {
        self.builder.finalize();
        (
            mem::take(&mut self.to_build),
            TranslationState {
                variables: self.variables,
                var_idx: self.var_idx,
                to_build: self.to_build,
                all_funcs: self.all_funcs,
            },
        )
    }
}

#[derive(derive_more::From, Debug)]
pub enum JITValue {
    Val(Value),
    Fun(FuncId),
    Unit,
}

impl JITValue {
    pub fn as_value(self) -> Value {
        if let JITValue::Val(v) = self {
            v
        } else {
            panic!("Expected a cranelift value, found {self:?}")
        }
    }
}

use JITValue::Unit;

#[instrument]
fn ty_to_clif<'a>(ty: TypeRef<'a>) -> JITResult<Type> {
    Ok(match ty {
        crate::ast::Type::Int => INT,
        crate::ast::Type::Fun { .. } => {
            todo!()
        }
        crate::ast::Type::Unit => types::B1,
    })
}

impl<'m, 'ty, 'ast> Translator<'m, 'ty, 'ast> {
    #[instrument]
    fn translate_stmt(&mut self, stmt: StmtRef<'ast>) -> JITResult<JITValue> {
        match stmt {
            crate::ast::Stmt::Bind { name, rhs } => {
                let binding = self.ty_ctx.resolve(name)?;
                let rhs = self.translate_expr(rhs)?;
                match rhs {
                    JITValue::Val(value) => {
                        let var = self.declare_variable(
                            binding,
                            self.ty_ctx.type_of_binding(binding).unwrap(),
                        );
                        self.builder.def_var(var, value)
                    }
                    JITValue::Fun(func) => {
                        let func_ref = self.module.declare_func_in_func(func, self.builder.func);
                        self.funcs.insert(binding, func_ref);
                        self.all_funcs.insert(binding, func);
                    }
                    Unit => {
                        let var = self.declare_variable(
                            binding,
                            self.ty_ctx.type_of_binding(binding).unwrap(),
                        );
                        // FIXME: figure out how to properly model unit values
                        let unit = self.builder.ins().bconst(types::B1, false);
                        self.builder.def_var(var, unit);
                    }
                }
            }
            crate::ast::Stmt::Assign { lhs, rhs } => {
                let name = match lhs {
                    crate::ast::Expr::Ident(name) => name,
                    lhs => panic!("expected identifier on lhs of assignment, found {lhs:?}"),
                };
                let binding = self.ty_ctx.resolve(name)?;
                let &var = self
                    .variables
                    .get(&binding)
                    .expect("variable should be declared");
                let rhs = self.translate_expr(rhs)?;
                match rhs {
                    JITValue::Val(value) => self.builder.def_var(var, value),
                    JITValue::Fun(func) => {
                        let func_ref = self.module.declare_func_in_func(func, self.builder.func);
                        self.funcs.insert(binding, func_ref);
                        self.all_funcs.insert(binding, func);
                    }
                    Unit => {
                        // FIXME: figure out how to properly model unit values
                        let unit = self.builder.ins().bconst(types::B1, false);
                        self.builder.def_var(var, unit);
                    }
                }
            }
            crate::ast::Stmt::Expr(expr) => {
                return self.translate_expr(expr);
            }
            crate::ast::Stmt::Semi(expr) => {
                self.translate_expr(expr)?;
            }
        }
        Ok(Unit)
    }

    #[instrument]
    fn translate_expr(&mut self, expr: ExprRef<'ast>) -> JITResult<JITValue> {
        Ok(match expr {
            crate::ast::Expr::Block(stmts) => {
                let mut last = JITValue::Unit;
                for stmt in stmts {
                    last = self.translate_stmt(stmt)?;
                }
                return Ok(last);
            }
            crate::ast::Expr::Fun(fun @ Fun { args, ret, body: _ }) => {
                let arg_tys: Vec<_> = args
                    .iter()
                    .map(|arg| ty_to_clif(arg.ty))
                    .collect::<JITResult<_>>()?;
                let ret_ty = ty_to_clif(ret)?;
                let mut signature = self.module.make_signature();
                for arg_ty in arg_tys {
                    signature.params.push(AbiParam::new(arg_ty));
                }
                signature.returns.push(AbiParam::new(ret_ty));
                let func_id = self.module.declare_anonymous_function(&signature)?;
                self.to_build.push((func_id, fun));
                return Ok(JITValue::Fun(func_id));
            }
            crate::ast::Expr::Call(Call { fun, args }) => {
                let binding = self.ty_ctx.resolve(fun)?;
                // let func_ref = self.module.declare_func_in_func(func, self.builder.func);
                let func = match self.funcs.get(&binding) {
                    None => {
                        let &func_id = self.all_funcs.get(&binding).expect("Unknown function");
                        let func = self.module.declare_func_in_func(func_id, self.builder.func);
                        self.funcs.insert(binding, func);
                        func
                    }
                    Some(func) => *func,
                };
                let args: Vec<_> = args
                    .iter()
                    .map(|e| -> JITResult<Value> {
                        let value = self.translate_expr(e)?;

                        Ok(match value {
                            JITValue::Val(val) => val,
                            JITValue::Fun(_) => panic!("functions as args aren't supported yet"),
                            Unit => self.builder.ins().bconst(types::B1, false),
                        })
                    })
                    .collect::<JITResult<_>>()?;
                let inst = self.builder.ins().call(func, &args);
                self.builder.inst_results(inst)[0]
            }
            crate::ast::Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(imm) => {
                    let imm = *imm;
                    self.builder.ins().iconst(INT, imm)
                }
            },
            crate::ast::Expr::Ident(name) => {
                let binding = self.ty_ctx.resolve(name)?;
                let &var = self
                    .variables
                    .get(&binding)
                    .expect("variable should be declared by now");
                self.builder.use_var(var)
            }
            crate::ast::Expr::BinOp(lhs, op, rhs) => {
                let lhs = self.translate_expr(lhs)?.as_value();
                let rhs = self.translate_expr(rhs)?.as_value();
                match op {
                    crate::ast::Op::Add => self.builder.ins().iadd(lhs, rhs),
                    crate::ast::Op::Sub => self.builder.ins().isub(lhs, rhs),
                    crate::ast::Op::Mul => self.builder.ins().imul(lhs, rhs),
                    crate::ast::Op::Div => self.builder.ins().udiv(lhs, rhs),
                }
            }
        }
        .into())
    }

    #[instrument]
    fn declare_variable(&mut self, binding: Binding, ty: TypeRef<'ast>) -> Variable {
        let var = Variable::new(self.var_idx);
        if let Some(_) = self.variables.insert(binding, var) {
            panic!("BAD: already declared {binding}");
        }
        let ty = match ty {
            crate::ast::Type::Int => INT,
            crate::ast::Type::Fun { .. } => {
                todo!()
            }
            crate::ast::Type::Unit => types::B1,
        };
        self.builder.declare_var(var, ty);
        self.var_idx += 1;
        var
    }
}

pub type JITResult<T, E = JITError> = Result<T, E>;

#[derive(Debug, Error)]
pub enum JITError {
    #[error("Cranelift module error: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("TypeError: {0}")]
    TypeError(String),

    #[error("Expression yielded no value when we expected one: {0}")]
    ExprYieldedNone(String),
}

impl<'a> From<TypeError<'a>> for JITError {
    fn from(err: TypeError<'a>) -> Self {
        Self::TypeError(err.to_string())
    }
}
