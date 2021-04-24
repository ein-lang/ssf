use super::algebraic::Algebraic;
use super::function::Function;
use super::primitive::Primitive;
use super::record::Record;
use super::variant::Variant;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Algebraic(Algebraic),
    Function(Function),
    Index(usize),
    Primitive(Primitive),
    Record(Record),
    Variant(Variant),
}

impl Type {
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }

    pub fn into_algebraic(self) -> Option<Algebraic> {
        match self {
            Self::Algebraic(algebraic) => Some(algebraic),
            _ => None,
        }
    }

    pub fn into_function(self) -> Option<Function> {
        match self {
            Self::Function(function) => Some(function),
            _ => None,
        }
    }

    pub fn into_primitive(self) -> Option<Primitive> {
        match self {
            Self::Primitive(primitive) => Some(primitive),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<Record> {
        match self {
            Self::Record(record) => Some(record),
            _ => None,
        }
    }

    pub fn into_variant(self) -> Option<Variant> {
        match self {
            Self::Variant(variant) => Some(variant),
            _ => None,
        }
    }
}

impl From<Algebraic> for Type {
    fn from(algebraic: Algebraic) -> Self {
        Self::Algebraic(algebraic)
    }
}

impl From<Function> for Type {
    fn from(function: Function) -> Self {
        Self::Function(function)
    }
}

impl From<Primitive> for Type {
    fn from(primitive: Primitive) -> Self {
        Self::Primitive(primitive)
    }
}

impl From<Record> for Type {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl From<Variant> for Type {
    fn from(variant: Variant) -> Self {
        Self::Variant(variant)
    }
}
