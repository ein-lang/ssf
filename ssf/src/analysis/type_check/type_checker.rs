use super::error::TypeCheckError;
use crate::ir::*;
use crate::types::{self, Type};
use std::collections::*;

#[derive(Clone, Debug)]
pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn check(&self, module: &Module) -> Result<(), TypeCheckError> {
        let mut variables = HashMap::<&str, Type>::new();

        for declaration in module.declarations() {
            variables.insert(declaration.name(), declaration.type_().clone());
        }

        for definition in module.definitions() {
            variables.insert(definition.name(), definition.type_().clone().into());
        }

        for definition in module.definitions() {
            self.check_definition(definition, &variables)?;
        }

        Ok(())
    }

    fn check_definition(
        &self,
        definition: &Definition,
        variables: &HashMap<&str, Type>,
    ) -> Result<(), TypeCheckError> {
        self.check_equality(
            &self.check_expression(definition.body(), &variables)?,
            &definition.type_().clone().into(),
        )
    }

    fn check_expression(
        &self,
        expression: &Expression,
        variables: &HashMap<&str, Type>,
    ) -> Result<Type, TypeCheckError> {
        match expression {
            Expression::Bitcast(bitcast) => {
                self.check_expression(bitcast.expression(), variables)?;
                Ok(bitcast.type_().clone().into())
            }
            Expression::Case(case) => self.check_case(case, variables),
            Expression::ConstructorApplication(constructor_application) => {
                let constructor = constructor_application.constructor();

                if constructor_application.arguments().len()
                    != constructor.constructor_type().elements().len()
                {
                    return Err(TypeCheckError::WrongArgumentsLength(expression.clone()));
                }

                for (argument, element_type) in constructor_application
                    .arguments()
                    .iter()
                    .zip(constructor.constructor_type().elements())
                {
                    self.check_equality(
                        &self.check_expression(argument, variables)?,
                        &element_type,
                    )?;
                }

                Ok(constructor_application
                    .constructor()
                    .algebraic_type()
                    .clone()
                    .into())
            }
            Expression::FunctionApplication(function_application) => {
                match self.check_expression(function_application.function(), variables)? {
                    Type::Function(function_type) => {
                        self.check_equality(
                            &self.check_expression(function_application.argument(), variables)?,
                            function_type.argument(),
                        )?;

                        Ok(function_type.result().clone().into())
                    }
                    _ => Err(TypeCheckError::FunctionExpected(
                        function_application.function().clone(),
                    )),
                }
            }
            Expression::Let(let_) => {
                let mut variables = variables.clone();

                self.check_definition(let_.definition(), &variables)?;
                variables.insert(
                    let_.definition().name(),
                    let_.definition().type_().clone().into(),
                );

                self.check_expression(let_.expression(), &variables)
            }
            Expression::LetFunctions(let_) => {
                let mut variables = variables.clone();

                for definition in let_.definitions() {
                    variables.insert(definition.name(), definition.type_().clone().into());
                }

                for definition in let_.definitions() {
                    let mut variables = variables.clone();

                    for argument in definition.arguments() {
                        variables.insert(argument.name().into(), argument.type_().clone());
                    }

                    self.check_equality(
                        &self.check_expression(definition.body(), &variables)?,
                        definition.result_type(),
                    )?;
                }

                self.check_expression(let_.expression(), &variables)
            }
            Expression::Primitive(primitive) => Ok(self.check_primitive(primitive).into()),
            Expression::Operation(operation) => {
                let lhs_type = self.check_expression(operation.lhs(), variables)?;
                let rhs_type = self.check_expression(operation.rhs(), variables)?;

                if lhs_type.is_primitive() && rhs_type.is_primitive() && lhs_type == rhs_type {
                    Ok(match operation.operator() {
                        Operator::Equal
                        | Operator::NotEqual
                        | Operator::GreaterThan
                        | Operator::GreaterThanOrEqual
                        | Operator::LessThan
                        | Operator::LessThanOrEqual => types::Primitive::Integer8.into(),
                        Operator::Add
                        | Operator::Subtract
                        | Operator::Multiply
                        | Operator::Divide => lhs_type,
                    })
                } else {
                    Err(TypeCheckError::TypesNotMatched(lhs_type, rhs_type))
                }
            }
            Expression::Variable(variable) => self.check_variable(variable, variables),
        }
    }

    fn check_case(
        &self,
        case: &Case,
        variables: &HashMap<&str, Type>,
    ) -> Result<Type, TypeCheckError> {
        match case {
            Case::Algebraic(algebraic_case) => {
                let argument_type = self
                    .check_expression(algebraic_case.argument(), variables)?
                    .into_algebraic()
                    .ok_or_else(|| {
                        TypeCheckError::AlgebraicExpected(algebraic_case.argument().clone())
                    })?;
                let mut expression_type = None;

                for alternative in algebraic_case.alternatives() {
                    let constructor = alternative.constructor();

                    self.check_equality(
                        &constructor.algebraic_type().clone().into(),
                        &argument_type.clone().into(),
                    )?;

                    let mut variables = variables.clone();

                    for (name, type_) in alternative
                        .element_names()
                        .iter()
                        .zip(constructor.constructor_type().elements())
                    {
                        variables.insert(name, type_.clone());
                    }

                    let alternative_type =
                        self.check_expression(alternative.expression(), &variables)?;

                    match &expression_type {
                        Some(expression_type) => {
                            self.check_equality(&alternative_type, expression_type)?;
                        }
                        None => expression_type = Some(alternative_type),
                    }
                }

                if let Some(default_alternative) = algebraic_case.default_alternative() {
                    let mut variables = variables.clone();

                    variables.insert(default_alternative.variable(), argument_type.into());

                    let alternative_type =
                        self.check_expression(default_alternative.expression(), &variables)?;

                    match &expression_type {
                        Some(expression_type) => {
                            self.check_equality(&alternative_type, expression_type)?;
                        }
                        None => expression_type = Some(alternative_type),
                    }
                }

                expression_type.ok_or_else(|| TypeCheckError::NoAlternativeFound(case.clone()))
            }
            Case::Primitive(primitive_case) => {
                let argument_type = self
                    .check_expression(primitive_case.argument(), variables)?
                    .into_primitive()
                    .ok_or_else(|| {
                        TypeCheckError::PrimitiveExpected(primitive_case.argument().clone())
                    })?;
                let mut expression_type = None;

                for alternative in primitive_case.alternatives() {
                    self.check_equality(
                        &self.check_primitive(alternative.primitive()).into(),
                        &argument_type.clone().into(),
                    )?;

                    let alternative_type =
                        self.check_expression(alternative.expression(), variables)?;

                    match &expression_type {
                        Some(expression_type) => {
                            self.check_equality(&alternative_type, expression_type)?;
                        }
                        None => expression_type = Some(alternative_type),
                    }
                }

                if let Some(default_alternative) = primitive_case.default_alternative() {
                    let mut variables = variables.clone();

                    variables.insert(default_alternative.variable(), argument_type.into());

                    let alternative_type =
                        self.check_expression(default_alternative.expression(), &variables)?;

                    match &expression_type {
                        Some(expression_type) => {
                            self.check_equality(&alternative_type, expression_type)?;
                        }
                        None => expression_type = Some(alternative_type),
                    }
                }

                expression_type.ok_or_else(|| TypeCheckError::NoAlternativeFound(case.clone()))
            }
        }
    }

    fn check_primitive(&self, primitive: &Primitive) -> types::Primitive {
        match primitive {
            Primitive::Float64(_) => types::Primitive::Float64,
            Primitive::Integer8(_) => types::Primitive::Integer8,
            Primitive::Integer64(_) => types::Primitive::Integer64,
        }
    }

    fn check_variable(
        &self,
        variable: &Variable,
        variables: &HashMap<&str, Type>,
    ) -> Result<Type, TypeCheckError> {
        variables
            .get(variable.name())
            .cloned()
            .ok_or_else(|| TypeCheckError::VariableNotFound(variable.clone()))
    }

    fn check_equality(&self, one: &Type, other: &Type) -> Result<(), TypeCheckError> {
        if one == other {
            Ok(())
        } else {
            Err(TypeCheckError::TypesNotMatched(one.clone(), other.clone()))
        }
    }
}
