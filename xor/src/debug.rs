use crate::{
    chunk::{Chunk, OpCode},
    vm::VM,
};
use std::convert::TryFrom;

pub(crate) fn disassemble_instruction(vm: &VM, chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);
    let line = chunk
        .get_line(offset)
        .expect("Instruction should have a line");

    if offset > 0
        && line
            == chunk
                .get_line(offset - 1)
                .expect("Instruction should have a line")
    {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }

    let byte = chunk.code[offset];
    match OpCode::try_from(byte) {
        Ok(instruction) => match instruction {
            OpCode::DefineGlobal => global_instruction("DEFINE_GLOBAL", vm, chunk, offset),
            OpCode::GetGlobal => global_instruction("GET_GLOBAL", vm, chunk, offset),
            OpCode::SetGlobal => global_instruction("SET_GLOBAL", vm, chunk, offset),
            OpCode::Pop => simple_instruction("POP", offset),
            OpCode::Print => simple_instruction("PRINT", offset),
            OpCode::Return => simple_instruction("RETURN", offset),
            OpCode::Constant => constant_instruction("CONSTANT", chunk, offset),
            OpCode::ConstantLong => constant_long_instruction("CONSTANT_LONG", chunk, offset),
            OpCode::Nil => simple_instruction("NIL", offset),
            OpCode::True => simple_instruction("TRUE", offset),
            OpCode::False => simple_instruction("FALSE", offset),
            OpCode::Not => simple_instruction("NOT", offset),
            OpCode::Equal => simple_instruction("EQUAL", offset),
            OpCode::Greater => simple_instruction("GREATER", offset),
            OpCode::Less => simple_instruction("LESS", offset),
            OpCode::Negate => simple_instruction("NEGATE", offset),
            OpCode::Add => simple_instruction("ADD", offset),
            OpCode::Subtract => simple_instruction("SUBTRACT", offset),
            OpCode::Multiply => simple_instruction("MULTIPLY", offset),
            OpCode::Divide => simple_instruction("DIVIDE", offset),
        },
        Err(_) => {
            println!("Unknown opcode {}", byte);
            offset + 1
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let index = chunk.code[offset + 1];
    print!("{:<16} {:<4}", name, index);
    println!("{}", chunk.constants[index as usize]);
    offset + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    // Decode constant index from "long" constant "u24" operand
    let index = u32::from_le_bytes([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
        0,
    ]);
    print!("{:<16} {:<4}", name, index);
    println!("{}", chunk.constants[index as usize]);
    offset + 4
}

fn global_instruction(name: &str, vm: &VM, chunk: &Chunk, offset: usize) -> usize {
    let index = chunk.code[offset + 1];
    print!("{:<16} {:<4}", name, index);
    println!("{}", vm.global_values[index as usize]);
    offset + 2
}

pub fn disassemble_chunk(vm: &VM, chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(vm, chunk, offset);
    }
}
