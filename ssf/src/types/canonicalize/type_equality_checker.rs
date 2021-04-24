use crate::types::*;

pub struct TypeEqualityChecker<'a> {
    pairs: Vec<(&'a Record, &'a Record)>,
}

impl<'a> TypeEqualityChecker<'a> {
    pub fn new(types: &'a [&'a Record]) -> Self {
        Self {
            pairs: types.iter().cloned().zip(types.iter().cloned()).collect(),
        }
    }

    pub fn equal_records(&self, one: &Record, other: &Record) -> bool {
        self.pairs.contains(&(one, other)) || {
            let checker = self.push_pair(one, other);

            one.elements().len() == other.elements().len()
                && one.is_boxed() == other.is_boxed()
                && one
                    .elements()
                    .iter()
                    .zip(other.elements())
                    .all(|(one, other)| checker.equal(one, other))
        }
    }

    fn equal(&self, one: &Type, other: &Type) -> bool {
        match (one, other) {
            (Type::Function(one), Type::Function(other)) => {
                self.equal(one.argument(), other.argument())
                    && self.equal(one.result(), other.result())
            }
            (Type::Primitive(one), Type::Primitive(other)) => one == other,
            (Type::Record(one), Type::Record(other)) => self.equal_records(one, other),
            (Type::Index(index), Type::Record(other)) => {
                self.equal_records(self.pairs[*index].0, other)
            }
            (Type::Record(other), Type::Index(index)) => {
                self.equal_records(other, self.pairs[*index].1)
            }
            (Type::Index(one), Type::Index(other)) => {
                self.equal_records(self.pairs[*one].0, self.pairs[*other].1)
            }
            (Type::Variant, Type::Variant) => true,
            (_, _) => false,
        }
    }

    fn push_pair(&'a self, one: &'a Record, other: &'a Record) -> Self {
        Self {
            pairs: [(one, other)].iter().chain(&self.pairs).copied().collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equal() {
        for (one, other) in &[
            (Primitive::Float64.into(), Primitive::Float64.into()),
            (
                Function::new(Primitive::Float64, Primitive::Float64).into(),
                Function::new(Primitive::Float64, Primitive::Float64).into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Primitive::Float64.into()])]).into(),
                Record::new(vec![Constructor::boxed(vec![Primitive::Float64.into()])]).into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Type::Index(0)])]).into(),
                Record::new(vec![Constructor::boxed(vec![Record::new(vec![
                    Constructor::boxed(vec![Type::Index(0)]),
                ])
                .into()])])
                .into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Type::Index(0)])]).into(),
                Record::new(vec![Constructor::boxed(vec![Record::new(vec![
                    Constructor::boxed(vec![Type::Index(1)]),
                ])
                .into()])])
                .into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Record::new(vec![
                    Constructor::boxed(vec![Type::Index(0)]),
                ])
                .into()])])
                .into(),
                Record::new(vec![Constructor::boxed(vec![Record::new(vec![
                    Constructor::boxed(vec![Type::Index(1)]),
                ])
                .into()])])
                .into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Function::new(
                    Primitive::Float64,
                    Type::Index(0),
                )
                .into()])])
                .into(),
                Record::new(vec![Constructor::boxed(vec![Function::new(
                    Primitive::Float64,
                    Record::new(vec![Constructor::boxed(vec![Function::new(
                        Primitive::Float64,
                        Type::Index(0),
                    )
                    .into()])]),
                )
                .into()])])
                .into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Function::new(
                    Primitive::Float64,
                    Type::Index(0),
                )
                .into()])])
                .into(),
                Record::new(vec![Constructor::boxed(vec![Function::new(
                    Primitive::Float64,
                    Record::new(vec![Constructor::boxed(vec![Function::new(
                        Primitive::Float64,
                        Type::Index(1),
                    )
                    .into()])]),
                )
                .into()])])
                .into(),
            ),
        ] {
            assert!(TypeEqualityChecker::new(&[]).equal(one, other));
        }
    }

    #[test]
    fn not_equal() {
        for (one, other) in &[
            (
                Primitive::Float64.into(),
                Function::new(Primitive::Float64, Primitive::Float64).into(),
            ),
            (
                Function::new(
                    Primitive::Float64,
                    Function::new(Primitive::Float64, Primitive::Float64),
                )
                .into(),
                Function::new(Primitive::Float64, Primitive::Float64).into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Primitive::Float64.into()])]).into(),
                Record::new(vec![Constructor::boxed(vec![
                    Primitive::Float64.into(),
                    Primitive::Float64.into(),
                ])])
                .into(),
            ),
            (
                Record::new(vec![Constructor::boxed(vec![Primitive::Float64.into()])]).into(),
                Record::new(vec![Constructor::unboxed(vec![Primitive::Float64.into()])]).into(),
            ),
        ] {
            assert!(!TypeEqualityChecker::new(&[]).equal(one, other));
        }
    }
}
