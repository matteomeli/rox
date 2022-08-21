use crate::{token::Token, types::Type};

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct Environment {
    cactus: Cactus<RefCell<EnvironmentNode>>,
}

impl Environment {
    pub fn global() -> Self {
        let mut env = Environment {
            cactus: Cactus::default(),
        };
        env.push();
        env
    }

    pub fn push(&mut self) {
        self.cactus = self.cactus.child(RefCell::new(EnvironmentNode::default()));
    }

    pub fn pop(&mut self) {
        if let Some(parent) = self.cactus.parent() {
            self.cactus = parent;
        }
    }

    pub fn define(&mut self, name: String, value: Type) {
        if let Some(env) = &self.cactus.node {
            env.val.borrow_mut().define(name, value);
        }
    }

    pub fn get(&self, name: &Token) -> Option<Type> {
        for env in self.cactus.values() {
            if let Some(value) = env.borrow().get(name) {
                return Some(value);
            }
        }
        None
    }

    pub fn assign(&mut self, name: &Token, new_value: Type) -> bool {
        for env in self.cactus.values() {
            if env.borrow_mut().assign(name, &new_value) {
                return true;
            }
        }
        false
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::global()
    }
}

#[derive(Default)]
pub struct EnvironmentNode {
    values: HashMap<String, Type>,
}

impl EnvironmentNode {
    pub fn define(&mut self, name: String, value: Type) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Option<Type> {
        self.values.get(&name.lexeme).cloned()
    }

    pub fn assign(&mut self, name: &Token, new_value: &Type) -> bool {
        match self.values.get_mut(&name.lexeme) {
            Some(value) => {
                *value = new_value.clone();
                true
            }
            None => false,
        }
    }
}

#[derive(Clone, Default)]
pub struct Cactus<T> {
    node: Option<Rc<Node<T>>>,
}

struct Node<T> {
    val: T,
    parent: Option<Rc<Node<T>>>,
}

impl<T> Cactus<T> {
    pub fn new() -> Self {
        Cactus { node: None }
    }

    pub fn child(&self, val: T) -> Self {
        Cactus {
            node: Some(Rc::new(Node {
                val,
                parent: self.node.clone(),
            })),
        }
    }

    pub fn parent(&self) -> Option<Cactus<T>> {
        self.node.as_ref().map(|n| Cactus {
            node: n.parent.clone(),
        })
    }

    pub fn val(&self) -> Option<&T> {
        self.node.as_ref().map(|n| &n.val)
    }

    pub fn nodes(&self) -> CactusIterator<T> {
        CactusIterator {
            next: self.node.as_ref(),
        }
    }

    pub fn values(&self) -> CactusValueIterator<T> {
        CactusValueIterator {
            next: self.node.as_ref(),
        }
    }
}

pub struct CactusIterator<'a, T>
where
    T: 'a,
{
    next: Option<&'a Rc<Node<T>>>,
}

impl<'a, T> Iterator for CactusIterator<'a, T> {
    type Item = Cactus<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|node| {
            self.next = node.parent.as_ref();
            Cactus {
                node: Some(node.clone()),
            }
        })
    }
}

pub struct CactusValueIterator<'a, T>
where
    T: 'a,
{
    next: Option<&'a Rc<Node<T>>>,
}

impl<'a, T> Iterator for CactusValueIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|node| {
            self.next = node.parent.as_ref();
            &node.val
        })
    }
}
