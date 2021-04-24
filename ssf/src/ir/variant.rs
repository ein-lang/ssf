use super::expression::Expression;
use crate::types::Type;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Variant {
    tag: String,
    payload: Arc<Expression>,
}

impl Variant {
    pub fn new(tag: impl Into<String>, payload: impl Into<Expression>) -> Self {
        Self {
            tag: tag.into(),
            payload: payload.into().into(),
        }
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }

    pub fn payload(&self) -> &Expression {
        &self.payload
    }

    pub(crate) fn find_variables(&self) -> HashSet<String> {
        self.payload.find_variables()
    }

    pub(crate) fn infer_environment(&self, variables: &HashMap<String, Type>) -> Self {
        Self::new(self.tag.clone(), self.payload.infer_environment(variables))
    }

    pub(crate) fn convert_types(&self, convert: &impl Fn(&Type) -> Type) -> Self {
        Self::new(self.tag.clone(), self.payload.convert_types(convert))
    }
}
