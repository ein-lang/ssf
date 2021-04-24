use super::type_::Type;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Variant {
    tag: String,
    payload: Arc<Type>,
}

impl Variant {
    pub fn new(tag: impl Into<String>, payload: impl Into<Type>) -> Self {
        Self {
            tag: tag.into(),
            payload: payload.into().into(),
        }
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }

    pub fn payload(&self) -> &Type {
        &self.payload
    }
}
