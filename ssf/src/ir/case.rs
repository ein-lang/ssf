use super::primitive_case::PrimitiveCase;
use super::variant_case::VariantCase;
use crate::types::Type;
use std::collections::{HashMap, HashSet};

/// Case expressions match values of algebraic data types with their
/// constructors deconstructing them.
///
/// Their alternatives do not have to be exhaustive. See also options of each
/// compiler for behavior on match failures.
#[derive(Clone, Debug, PartialEq)]
pub enum Case {
    Primitive(PrimitiveCase),
    Variant(VariantCase),
}

impl Case {
    pub(crate) fn find_variables(&self) -> HashSet<String> {
        match self {
            Self::Primitive(primitive_case) => primitive_case.find_variables(),
            Self::Variant(variant_case) => variant_case.find_variables(),
        }
    }

    pub(crate) fn infer_environment(&self, variables: &HashMap<String, Type>) -> Self {
        match self {
            Self::Primitive(primitive_case) => primitive_case.infer_environment(variables).into(),
            Self::Variant(variant_case) => variant_case.infer_environment(variables).into(),
        }
    }

    pub(crate) fn convert_types(&self, convert: &impl Fn(&Type) -> Type) -> Self {
        match self {
            Self::Primitive(primitive_case) => primitive_case.convert_types(convert).into(),
            Self::Variant(variant_case) => variant_case.convert_types(convert).into(),
        }
    }
}

impl From<PrimitiveCase> for Case {
    fn from(primitive_case: PrimitiveCase) -> Self {
        Self::Primitive(primitive_case)
    }
}

impl From<VariantCase> for Case {
    fn from(variant_case: VariantCase) -> Self {
        Self::Variant(variant_case)
    }
}
