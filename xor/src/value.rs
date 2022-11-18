use std::{
    borrow::Borrow,
    fmt::Display,
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

use crate::vm::{RuntimeError, VMError, VM};

pub type ObjRoot<T> = Rc<Obj<T>>;
pub type ObjRef<T> = Weak<Obj<T>>;

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(ObjRef<String>),
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

impl From<ObjRef<String>> for Value {
    fn from(o: ObjRef<String>) -> Self {
        Value::String(o)
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
                let s = &obj_ref.upgrade().unwrap().content;
                Ok(s.clone())
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
            Self::String(s) => write!(f, "{}", to_string(s)),
        }
    }
}

/// A struct that wraps heap stored Ts
pub struct Obj<T> {
    pub content: T,
}

fn to_string(obj: &ObjRef<String>) -> String {
    let s = &obj.upgrade().unwrap().content;
    format!("\"{}\"", s)
}

pub struct InternedString(pub ObjRoot<String>);

impl Hash for InternedString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.content.hash(state);
    }
}

impl PartialEq for InternedString {
    fn eq(&self, other: &Self) -> bool {
        self.0.content == other.0.content
    }
}

impl Eq for InternedString {}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        self.0.content.borrow()
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

pub fn create_string(vm: &mut VM, s: &str) -> ObjRef<String> {
    match vm.strings.get(s) {
        Some(InternedString(obj_root)) => Rc::downgrade(obj_root),
        None => {
            let obj_string = Obj::<String> {
                content: s.to_owned(),
            };
            let obj_root = Rc::new(obj_string);
            let obj_ref = Rc::downgrade(&obj_root);
            let interned_string = InternedString(obj_root);
            vm.strings.insert(interned_string);
            obj_ref
        }
    }
}
