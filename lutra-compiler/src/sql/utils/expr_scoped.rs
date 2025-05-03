use std::rc::Rc;

use crate::sql::cr;

use super::ExprOrSource;

/// An SQL expression that might require a relational variable (rvar) to be in scope.
/// For example, `invoice.id` and requirement that `invoice` rvar is in scope.
pub struct ExprScoped {
    pub inner: ExprOrSource,
    pub rvar: Option<Rc<cr::RelVar>>,
}

impl ExprScoped {
    pub fn unwrap_inner(self) -> ExprOrSource {
        if let Some(rvar) = self.rvar {
            panic!("expected ExprScoped not to contain an rvar, but it does: {rvar:?}")
        }
        self.inner
    }
}

impl From<ExprOrSource> for ExprScoped {
    fn from(inner: ExprOrSource) -> Self {
        Self { inner, rvar: None }
    }
}
