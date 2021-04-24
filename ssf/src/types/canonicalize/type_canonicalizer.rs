use super::type_equality_checker::TypeEqualityChecker;
use crate::types::*;

pub struct TypeCanonicalizer<'a> {
    types: Vec<&'a Record>,
}

impl<'a> TypeCanonicalizer<'a> {
    pub fn new() -> Self {
        Self { types: vec![] }
    }

    pub fn canonicalize(&self, type_: &Type) -> Type {
        match type_ {
            Type::Function(function) => Function::new(
                self.canonicalize(function.argument()),
                self.canonicalize(function.result()),
            )
            .into(),
            Type::Record(record) => {
                for (index, parent_type) in self.types.iter().enumerate() {
                    if TypeEqualityChecker::new(&self.types).equal_records(record, parent_type) {
                        return Type::Index(index);
                    }
                }

                let other = self.push_type(record);

                Record::new(
                    record
                        .elements()
                        .iter()
                        .map(|element| other.canonicalize(element))
                        .collect(),
                    record.is_boxed(),
                )
                .into()
            }
            _ => type_.clone(),
        }
    }

    fn push_type(&'a self, type_: &'a Record) -> Self {
        Self {
            types: [type_].iter().chain(&self.types).cloned().collect(),
        }
    }
}
