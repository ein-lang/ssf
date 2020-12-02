use super::ffi_function_type::FfiFunctionType;
use std::collections::HashMap;
use std::sync::Arc;

pub struct CompileConfiguration {
    malloc_function_name: Option<String>,
    panic_function_name: Option<String>,
    ffi_functions: HashMap<String, FfiFunctionType>,
}

impl CompileConfiguration {
    pub fn new(
        malloc_function_name: Option<String>,
        panic_function_name: Option<String>,
        ffi_functions: HashMap<String, FfiFunctionType>,
    ) -> Arc<Self> {
        Self {
            malloc_function_name,
            panic_function_name,
            ffi_functions,
        }
        .into()
    }

    pub fn malloc_function_name(&self) -> &str {
        self.malloc_function_name
            .as_ref()
            .map(|string| string.as_ref())
            .unwrap_or("malloc")
    }

    pub fn panic_function_name(&self) -> Option<&str> {
        self.panic_function_name
            .as_ref()
            .map(|string| string.as_ref())
    }

    pub fn ffi_functions(&self) -> &HashMap<String, FfiFunctionType> {
        &self.ffi_functions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        CompileConfiguration::new(Some("".into()), None);
    }
}
