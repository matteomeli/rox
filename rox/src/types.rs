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
