use crate::types::*;

pub struct TypeUnfolder {
    record: Record,
    index: usize,
}

impl TypeUnfolder {
    pub fn new(record: &Record) -> Self {
        Self {
            record: record.clone(),
            index: 0,
        }
    }

    pub fn unfold(&self, type_: &Type) -> Type {
        match type_ {
            Type::Function(function) => Function::new(
                self.unfold(function.argument()),
                self.unfold(function.result()),
            )
            .into(),
            Type::Index(index) => {
                if *index == self.index {
                    self.record.clone().into()
                } else {
                    Type::Index(*index)
                }
            }
            Type::Record(record) => self.unfold_record(record).into(),
            Type::Primitive(_) | Type::Variant => type_.clone(),
        }
    }

    fn unfold_record(&self, record: &Record) -> Record {
        let other = self.increment_index();

        Record::new(
            record
                .elements()
                .iter()
                .map(|type_| other.unfold(type_))
                .collect(),
            record.is_boxed(),
        )
    }

    fn increment_index(&self) -> Self {
        Self {
            record: self.record.clone(),
            index: self.index + 1,
        }
    }
}
