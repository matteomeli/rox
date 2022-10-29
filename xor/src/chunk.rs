use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::value::Value;

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    ConstantLong,
    Return,
}

pub struct LineStart {
    offset: usize,
    line_no: u32,
}

impl LineStart {
    fn new(offset: usize, line_no: u32) -> Self {
        LineStart { offset, line_no }
    }
}

#[derive(Default)]
pub struct Chunk {
    pub(crate) code: Vec<u8>,
    pub(crate) constants: Vec<Value>,
    pub(crate) lines: Vec<LineStart>,
}

impl Chunk {
    pub fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        match self.lines.last() {
            Some(&LineStart { line_no, .. }) if line_no == line => (),
            _ => self.lines.push(LineStart::new(self.code.len() - 1, line)),
        }
    }

    pub fn write_constant(&mut self, value: Value, line: u32) {
        let index = self.add_constant(value);
        if self.constants.len() < (u8::MAX as usize) {
            self.write(OpCode::Constant.into(), line);
            self.write(index as u8, line);
        } else {
            // Store "long" constant index with 24 bit operand
            self.write(OpCode::ConstantLong.into(), line);
            // Convert to "u24" from usize with little-endian ordering
            let [a, b, c, ..] = index.to_le_bytes();
            self.write(a, line);
            self.write(b, line);
            self.write(c, line);
            // TODO: Handle index > u24::MAX
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    // TODO: This is very expensive as it has to walk the whole array of line starts every time
    pub fn get_line(&self, instruction: usize) -> Option<u32> {
        let mut line: Option<u32> = None;
        for &LineStart { offset, line_no } in self.lines.iter() {
            if offset > instruction {
                break;
            }
            line = Some(line_no);
        }

        line
    }
}
