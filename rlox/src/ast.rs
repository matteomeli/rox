use crate::token::{Literal, Token};

#[derive(Debug)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
}

#[derive(Debug)]
pub struct Binary(pub Box<Expr>, pub Token, pub Box<Expr>);

#[derive(Debug)]
pub struct Grouping(pub Box<Expr>);

#[derive(Debug)]
pub struct Unary(pub Token, pub Box<Expr>);

pub trait Visitor {
    type Result;

    fn visit_binary_expr(&mut self, binary: &Binary) -> Self::Result;
    fn visit_grouping_expr(&mut self, grouping: &Grouping) -> Self::Result;
    fn visit_literal_expr(&mut self, literal: &Literal) -> Self::Result;
    fn visit_unary_expr(&mut self, unary: &Unary) -> Self::Result;
}

impl Expr {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Result {
        match self {
            Self::Binary(binary) => visitor.visit_binary_expr(binary),
            Self::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            Self::Literal(literal) => visitor.visit_literal_expr(literal),
            Self::Unary(unary) => visitor.visit_unary_expr(unary),
        }
    }
}
