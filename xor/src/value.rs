use std::{
    borrow::Borrow,
    fmt::Display,
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

use crate::{
    chunk::Chunk,
    vm::{Object, RuntimeError, VMError, VM},
};

pub type ObjectRoot<T> = Rc<T>;
pub type ObjectRef<T> = Weak<T>;

impl Object for ObjectRoot<String> {}

impl Object for ObjectRoot<Function> {}

impl Object for ObjectRoot<NativeFunction> {}

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Undefined,
    Number(f64),
    String(ObjectRef<String>),
    Function(ObjectRef<Function>),
    NativeFunction(ObjectRef<NativeFunction>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => Weak::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(b) => !b,
            Value::Nil => true,
            Value::Number(n) if *n == 0.0 => true,
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Number(n)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<ObjectRef<String>> for Value {
    fn from(o: ObjectRef<String>) -> Self {
        Value::String(o)
    }
}

impl From<ObjectRef<Function>> for Value {
    fn from(o: ObjectRef<Function>) -> Self {
        Value::Function(o)
    }
}

impl TryFrom<Value> for f64 {
    type Error = VMError;

    #[allow(unreachable_patterns)]
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(n) => Ok(n),
            _ => Err(VMError::RuntimeError(RuntimeError::TypeError(
                "number",
                value.to_string(),
                true,
            ))),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = VMError;

    #[allow(unreachable_patterns)]
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(obj_ref) => {
                let s = obj_ref.upgrade().unwrap();
                Ok((*s).clone())
            }
            _ => Err(VMError::RuntimeError(RuntimeError::TypeError(
                "string",
                value.to_string(),
                true,
            ))),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
            Self::Number(n) => {
                if *n == 0.0 && n.is_sign_negative() {
                    write!(f, "-0")
                } else {
                    write!(f, "{}", n)
                }
            }
            Self::String(s) => write!(f, "{}", format_string(s)),
            Self::Undefined => write!(f, "undefined"),
            Self::Function(fun) => {
                write!(f, "{}", format_funcion(fun))
            }
            Self::NativeFunction(_) => {
                write!(f, "<native fn>")
            }
        }
    }
}

pub struct InternedString(pub ObjectRoot<String>);

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for InternedString {}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        (*self.0).borrow()
    }
}

impl TryFrom<Value> for InternedString {
    type Error = VMError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(obj_ref) => Ok(Self(obj_ref.upgrade().unwrap())),
            _ => Err(VMError::RuntimeError(RuntimeError::TypeError(
                "string",
                value.to_string(),
                false,
            ))),
        }
    }
}

fn format_string(string_ref: &ObjectRef<String>) -> String {
    let s = &string_ref.upgrade().unwrap();
    format!("\"{}\"", s)
}

pub fn create_string(vm: &mut VM, content: &str) -> ObjectRef<String> {
    match vm.strings.get(content) {
        Some(InternedString(obj_root)) => Rc::downgrade(obj_root),
        None => {
            let string = content.to_owned();
            let string_root = Rc::new(string);
            let string_ref = Rc::downgrade(&string_root);
            let string_interned = InternedString(Rc::clone(&string_root));
            vm.strings.insert(string_interned);
            vm.objects.push(Box::new(string_root));
            string_ref
        }
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum FunctionType {
    Function,
    Script,
}

pub struct Function {
    pub name: Option<ObjectRef<String>>,
    pub arity: usize,
    pub chunk: Chunk,
}

impl Function {
    pub fn new(vm: &mut VM, name: Option<&str>, arity: usize) -> Self {
        let name = name.map(|s| create_string(vm, s));
        Function {
            name,
            arity,
            chunk: Chunk::default(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_function_name(self))
    }
}

pub fn format_funcion(function_ref: &ObjectRef<Function>) -> String {
    let function = &function_ref.upgrade().unwrap();
    format_function_name(function)
}

pub fn format_function_name(function: &Function) -> String {
    function
        .name
        .as_ref()
        .map(|s| format!("<fn {}>", s.upgrade().unwrap()))
        .unwrap_or_else(|| "<script>".to_owned())
}

pub type NativeFn = fn(arg_count: usize, args: &[Value]) -> Value;

pub struct NativeFunction {
    pub function: NativeFn,
}

impl NativeFunction {
    pub fn new(function: NativeFn) -> Self {
        NativeFunction { function }
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
