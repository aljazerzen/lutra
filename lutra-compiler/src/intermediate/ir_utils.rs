use lutra_bin::ir::*;

pub fn new_call_bin(id: &str, left: Expr, right: Expr, ty: Ty) -> Expr {
    let function = Expr::new(
        ExprKind::Pointer(Pointer::External(ExternalPtr { id: id.to_string() })),
        Ty::new(TyFunction {
            params: vec![left.ty.clone(), right.ty.clone()],
            body: ty.clone(),
        }),
    );
    let args = vec![left, right];
    Expr::new(Call { function, args }, ty)
}

pub fn new_call_bin_bool(func_id: &str, left: Expr, right: Expr) -> Expr {
    new_call_bin(func_id, left, right, Ty::bool())
}

pub fn new_call_un(id: &str, arg: Expr, ty: Ty) -> Expr {
    let function = Expr::new(
        ExprKind::Pointer(Pointer::External(ExternalPtr { id: id.into() })),
        Ty::new(TyFunction {
            params: vec![arg.ty.clone()],
            body: ty.clone(),
        }),
    );
    let args = vec![arg];
    Expr::new(Call { function, args }, ty)
}

pub fn new_call_tri(id: &str, a: Expr, b: Expr, c: Expr, ty: Ty) -> Expr {
    let function = Expr::new(
        ExprKind::Pointer(Pointer::External(ExternalPtr { id: id.to_string() })),
        Ty::new(TyFunction {
            params: vec![a.ty.clone(), b.ty.clone(), c.ty.clone()],
            body: ty.clone(),
        }),
    );
    let args = vec![a, b, c];
    Expr::new(Call { function, args }, ty)
}

pub fn new_param(function_id: u32, param_position: u8, ty: Ty) -> Expr {
    Expr::new(
        ParameterPtr {
            function_id,
            param_position,
        },
        ty,
    )
}
