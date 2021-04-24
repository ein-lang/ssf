use super::{type_::Type, unfold::unfold};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Record {
    elements: Vec<Type>,
    boxed: bool,
}

impl Record {
    pub const fn new(elements: Vec<Type>, boxed: bool) -> Self {
        Self { elements, boxed }
    }

    pub fn elements(&self) -> &[Type] {
        &self.elements
    }

    pub fn is_boxed(&self) -> bool {
        self.boxed
    }

    pub fn unfold(&self) -> Self {
        unfold(self)
    }
}
