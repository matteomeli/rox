use crate::{
    chunk::{Chunk, OpCode},
    vm::VM,
};
use std::convert::TryFrom;

pub fn disassemble_instruction(vm: &VM, chunk: &Chunk, offset: usize) -> usize {
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
    let offset = match OpCode::try_from(byte) {
        Ok(instruction) => match instruction {
            OpCode::DefineGlobal => global_instruction("DEFINE_GLOBAL", vm, chunk, offset),
            OpCode::DefineGlobalLong => {
                global_long_instruction("DEFINE_GLOBAL_LONG", vm, chunk, offset)
            }
            OpCode::GetGlobal => global_instruction("GET_GLOBAL", vm, chunk, offset),
            OpCode::GetGlobalLong => global_long_instruction("GET_GLOBAL_LONG", vm, chunk, offset),
            OpCode::SetGlobal => global_instruction("SET_GLOBAL", vm, chunk, offset),
            OpCode::SetGlobalLong => global_long_instruction("SET_GLOBAL_LONG", vm, chunk, offset),
            OpCode::GetLocal => local_instruction("GET_LOCAL", chunk, offset),
            OpCode::GetLocalLong => local_long_instruction("GET_LOCAL_LONG", chunk, offset),
            OpCode::SetLocal => local_instruction("SET_LOCAL", chunk, offset),
            OpCode::SetLocalLong => local_long_instruction("SET_LOCAL_LONG", chunk, offset),
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
            OpCode::JumpIfFalse => jump_instruction("JUMP_IF_FALSE", chunk, offset, 1),
            OpCode::Jump => jump_instruction("JUMP", chunk, offset, 1),
            OpCode::Loop => jump_instruction("LOOP", chunk, offset, -1),
        },
        Err(_) => {
            println!("Unknown opcode {}", byte);
            offset + 1
        }
    };

    println!();

    offset
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    print!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:<16} {:>4} ", name, slot);
    print!("'{}'", chunk.constants[slot as usize]);
    offset + 2
}

fn constant_long_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    // Decode constant slot from "long" constant "u24" operand
    let slot = u32::from_le_bytes([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
        0,
    ]);
    print!("{:<16} {:>4} ", name, slot);
    print!("'{}'", chunk.constants[slot as usize]);
    offset + 4
}

fn global_instruction(name: &str, vm: &VM, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:<16} {:>4} ", name, slot);
    // Global variables are late bound, so they will be always undefined at this stage, print their names instead
    print!("{}", vm.globals[slot as usize].0);
    offset + 2
}

fn global_long_instruction(name: &str, vm: &VM, chunk: &Chunk, offset: usize) -> usize {
    // Decode constant slot from "long" constant "u24" operand
    let slot = u32::from_le_bytes([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
        0,
    ]);
    print!("{:<16} {:>4} ", name, slot);
    // Global variables are late bound, so they will be always undefined at this stage, print their names instead
    print!("{}", vm.globals[slot as usize].0);
    offset + 4
}

fn local_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:<16} {:>4} ", name, slot);
    // TODO: This doesn't allow to track/debug the local name, only its slot
    print!("'{}'", slot);
    offset + 2
}

fn local_long_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    // Decode constant slot from "long" constant "u24" operand
    let slot = u32::from_le_bytes([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
        0,
    ]);
    print!("{:<16} {:>4} ", name, slot);
    // TODO: This doesn't allow to track/debug the local name, only its slot
    print!("'{}'", slot);
    offset + 4
}

fn jump_instruction(name: &str, chunk: &Chunk, offset: usize, sign: isize) -> usize {
    // Decode constant slot from "long" constant "u24" operand
    let jump = u16::from_le_bytes([chunk.code[offset + 1], chunk.code[offset + 2]]) as isize;
    let jump_offset = (offset as isize + 3 + sign * jump) as usize;
    print!("{:<16} {:>4} -> {:<4}", name, offset, jump_offset);
    offset + 3
}

pub fn disassemble_chunk(vm: &VM, chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(vm, chunk, offset);
    }
}
