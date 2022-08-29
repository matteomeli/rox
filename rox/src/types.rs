#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub enum Type {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Type {
    pub fn is_truthy(&self) -> bool {
        match self {
            Type::Nil => false,
            Type::Boolean(b) => *b,
            _ => true,
        }
    }
}
