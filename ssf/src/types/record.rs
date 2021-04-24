use super::type_::Type;

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

    // TODO
    // pub fn unfold(&self) -> Self {
    //     unfold(self)
    // }
}
