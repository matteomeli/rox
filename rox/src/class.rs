use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    callable::{Callable, Function},
    interpreter::{InterpretResult, Interpreter},
    token::Token,
    types::Type,
};

#[derive(Debug, Clone)]
pub struct Class {
    class_data: Rc<ClassData>,
}

impl Class {
    pub fn new(class_data: Rc<ClassData>) -> Self {
        Class { class_data }
    }
}

#[derive(Debug, Clone)]
pub struct ClassData {
    name: String,
    methods: HashMap<String, Function>,
}

impl ClassData {
    pub fn new(name: String, methods: HashMap<String, Function>) -> Self {
        ClassData { name, methods }
    }

    pub fn find_method(&self, name: &str) -> Option<Function> {
        self.methods.get(name).cloned()
    }
}

impl Callable for Class {
    fn arity(&self) -> usize {
        if let Some(initializer) = self.class_data.find_method("init") {
            initializer.arity()
        } else {
            0
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Type>) -> InterpretResult<Type> {
        let instance = Instance::new(self.class_data.clone());
        if let Some(initializer) = self.class_data.find_method("init") {
            initializer.bind(&instance).call(interpreter, arguments)?;
        }
        Ok(Type::Instance(instance))
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class: {}>", self.class_data.name)
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    class_data: Rc<ClassData>,
    instance_data: Rc<RefCell<InstanceData>>,
}

impl Instance {
    pub fn new(class_data: Rc<ClassData>) -> Self {
        Instance {
            class_data,
            instance_data: Rc::new(RefCell::new(InstanceData::default())),
        }
    }

    pub fn get(&self, name: &Token) -> Option<Type> {
        self.instance_data
            .borrow()
            .fields
            .get(&name.lexeme)
            .cloned()
            .or_else(|| {
                let method = self.class_data.find_method(&name.lexeme);
                method.map(|method| Type::Callable(Box::new(method.bind(self))))
            })
    }

    pub fn set(&mut self, name: &Token, value: Type) {
        self.instance_data
            .borrow_mut()
            .fields
            .insert(name.lexeme.clone(), value);
    }
}

#[derive(Debug, Default)]
pub struct InstanceData {
    fields: HashMap<String, Type>,
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class instance: {}>", self.class_data.name)
    }
}
