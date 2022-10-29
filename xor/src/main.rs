use xor::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    value::Value,
};

fn main() {
    let mut chunk = Chunk::default();
    let constant_index = chunk.add_constant(Value::Number(1.2));
    chunk.write(OpCode::Constant.into(), 123);
    chunk.write(constant_index as u8, 123);
    chunk.write(OpCode::Return.into(), 123);

    // for i in 0..255u32 {
    //     chunk.write_constant(Value::Number(i as f64), i);
    // }

    disassemble_chunk(&chunk, "test chunk");
}
