use xor::{
    chunk::{Chunk, OpCode},
    value::Value,
    vm::VM,
};

#[cfg(feature = "disassemble")]
use xor::debug::disassemble_chunk;

#[allow(unused_variables, unreachable_code, unused_mut)]
fn main() {
    let mut vm = VM::default();

    // Create chunk
    let mut chunk = Chunk::default();
    let mut constant_index = chunk.add_constant(Value::Number(1.2));
    chunk.write(OpCode::Constant.into(), 123);
    chunk.write(constant_index as u8, 123);

    constant_index = chunk.add_constant(Value::Number(3.4));
    chunk.write(OpCode::Constant.into(), 123);
    chunk.write(constant_index as u8, 123);

    chunk.write(OpCode::Add.into(), 123);

    constant_index = chunk.add_constant(Value::Number(5.6));
    chunk.write(OpCode::Constant.into(), 123);
    chunk.write(constant_index as u8, 123);

    chunk.write(OpCode::Divide.into(), 123);
    chunk.write(OpCode::Negate.into(), 123);

    chunk.write(OpCode::Return.into(), 123);

    #[cfg(feature = "disassemble")]
    {
        // Disassemble chunk
        disassemble_chunk(&chunk, "test chunk");
        return;
    }

    // Run chunk in VM
    vm.run(&chunk).unwrap();
}
