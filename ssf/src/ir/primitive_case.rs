use super::default_alternative::DefaultAlternative;
use super::expression::Expression;
use super::primitive_alternative::PrimitiveAlternative;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq)]
pub struct PrimitiveCase {
    argument: Box<Expression>,
    alternatives: Vec<PrimitiveAlternative>,
    default_alternative: Option<DefaultAlternative>,
}

impl PrimitiveCase {
    pub fn new(
        argument: impl Into<Expression>,
        alternatives: Vec<PrimitiveAlternative>,
        default_alternative: Option<DefaultAlternative>,
    ) -> Self {
        Self {
            argument: Box::new(argument.into()),
            alternatives,
            default_alternative,
        }
    }

    pub fn argument(&self) -> &Expression {
        &self.argument
    }

    pub fn alternatives(&self) -> &[PrimitiveAlternative] {
        &self.alternatives
    }

    pub fn default_alternative(&self) -> Option<&DefaultAlternative> {
        self.default_alternative.as_ref()
    }

    pub(crate) fn rename_variables(&self, names: &HashMap<String, String>) -> Self {
        Self {
            argument: self.argument.rename_variables(names).into(),
            alternatives: self
                .alternatives
                .iter()
                .map(|alternative| alternative.rename_variables(names))
                .collect(),
            default_alternative: self
                .default_alternative
                .as_ref()
                .map(|default_alternative| default_alternative.rename_variables(names)),
        }
    }

    pub(crate) fn find_free_variables(&self) -> HashSet<String> {
        let mut variables = self.argument.find_free_variables();

        for alternative in &self.alternatives {
            variables.extend(alternative.find_free_variables());
        }

        if let Some(default_alternative) = &self.default_alternative {
            variables.extend(default_alternative.find_free_variables());
        }

        variables
    }

    pub(crate) fn infer_environment(&self, variables: &HashMap<String, Type>) -> Self {
        Self {
            argument: self.argument.infer_environment(variables).into(),
            alternatives: self
                .alternatives
                .iter()
                .map(|alternative| alternative.infer_environment(variables))
                .collect(),
            default_alternative: self
                .default_alternative
                .as_ref()
                .map(|default_alternative| default_alternative.infer_environment(variables)),
        }
    }

    pub(crate) fn convert_types(&self, convert: &impl Fn(&Type) -> Type) -> Self {
        Self {
            argument: self.argument.convert_types(convert).into(),
            alternatives: self
                .alternatives
                .iter()
                .map(|alternative| alternative.convert_types(convert))
                .collect(),
            default_alternative: self
                .default_alternative
                .as_ref()
                .map(|default_alternative| default_alternative.convert_types(convert)),
        }
    }
}
