use std::collections::HashMap;

use codegen::{CompiledFunction, Module, opcode::Op};

#[derive(Debug, Clone)]
pub enum Value {
    Never,
    Null,
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    String(u16),
}

impl Value {
    // Helper methods for type checking and extraction
    pub fn as_i32(&self) -> Result<i32, String> {
        match self {
            Value::I32(v) => Ok(*v),
            _ => Err(format!("Expected I32, got {:?}", self)),
        }
    }

    pub fn as_i64(&self) -> Result<i64, String> {
        match self {
            Value::I64(v) => Ok(*v),
            _ => Err(format!("Expected I64, got {:?}", self)),
        }
    }

    pub fn as_f32(&self) -> Result<f32, String> {
        match self {
            Value::F32(v) => Ok(*v),
            _ => Err(format!("Expected F32, got {:?}", self)),
        }
    }

    pub fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::F64(v) => Ok(*v),
            _ => Err(format!("Expected F64, got {:?}", self)),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::I32(0) | Value::I64(0) => false,
            _ => false,
        }
    }
}

struct CallFrame {
    _func_id: u16,
    base_stack: usize,
    return_address: usize,
    locals: Vec<Value>,
}

enum State {
    Executing,
    Finished,
    Halted,
}

pub struct VMStack {
    inner: Vec<Value>,
    strings: Vec<String>,
}

impl VMStack {
    fn new(strings: Vec<String>) -> Self {
        Self {
            inner: Vec::new(),
            strings,
        }
    }

    pub fn push(&mut self, value: Value) { self.inner.push(value); }

    pub fn pop(&mut self) -> Result<Value, String> { self.inner.pop().ok_or("Stack underflow".to_string()) }

    pub fn string(&self, str_id: u16) -> Option<&String> { self.strings.get(str_id as usize) }

    fn truncate(&mut self, len: usize) { self.inner.truncate(len); }

    fn len(&self) -> usize { self.inner.len() }
}

type ExternalFunction = dyn Fn(&mut VMStack) -> Result<(), String>;

pub struct VM {
    state: State,
    stack: VMStack,
    functions: Vec<CompiledFunction>,
    external_functions: HashMap<u16, Box<ExternalFunction>>,
    code: Vec<u8>,
    call_frames: Vec<CallFrame>,
    ip: usize,
    errors: Vec<String>,
}

impl CallFrame {
    pub fn new(func_id: u16, base_stack: usize, return_address: usize, local_count: u16) -> Self {
        Self {
            _func_id: func_id,
            base_stack,
            return_address,
            locals: vec![Value::Null; local_count as usize],
        }
    }

    pub fn get_local(&self, index: u16) -> Option<&Value> { self.locals.get(index as usize) }

    pub fn set_local(&mut self, index: u16, value: Value) -> Option<()> {
        self.locals.get_mut(index as usize)?.clone_from(&value);
        Some(())
    }
}

impl VM {
    pub fn new(module: Module) -> Self {
        Self {
            state: State::Executing,
            functions: module.functions,
            external_functions: HashMap::new(),
            stack: VMStack::new(module.strings),
            code: module.code,
            call_frames: Vec::new(),
            ip: 0,
            errors: Vec::new(),
        }
    }

    pub fn define_external<F>(&mut self, id: u16, f: F)
    where
        F: Fn(&mut VMStack) -> Result<(), String> + 'static,
    {
        self.external_functions.insert(id, Box::new(f));
    }

    pub fn execute_function(&mut self, func_id: u16) -> Result<Value, String> {
        let func = self
            .functions
            .get(func_id as usize)
            .ok_or_else(|| format!("Invalid function Id {func_id}"))?;

        if !func.is_external {
            let mut frame = CallFrame::new(func.id, self.stack.len(), self.ip, func.local_count);
            for i in (0..func.param_count).rev() {
                let arg = self.stack.pop()?;
                frame.locals[i as usize] = arg;
            }

            self.call_frames.push(frame);
            self.ip = func.address as usize;

            while matches!(self.state, State::Executing) {
                self.execute_op()?;
            }

            if matches!(self.state, State::Halted) {
                return Err("VM halted!".to_string());
            }
        } else if let Some(f) = self.external_functions.get(&func_id) {
            f(&mut self.stack)?;
        }

        Ok(self.stack.pop().unwrap_or(Value::Never))
    }

    fn execute_op(&mut self) -> Result<(), String> {
        if self.ip >= self.code.len() {
            return Err("Instruction pointer is out of bounds".to_string());
        }

        let op = Op::from_u8(self.code[self.ip]).ok_or("Invalid Instruction".to_string())?;
        self.ip += 1;

        match op {
            Op::Nop => {},
            Op::PushNull => self.stack.push(Value::Null),
            Op::PushTrue => self.stack.push(Value::Bool(true)),
            Op::PushFalse => self.stack.push(Value::Bool(false)),
            Op::PushZero => self.stack.push(Value::I32(0)),
            Op::PushOne => self.stack.push(Value::I32(1)),
            Op::PushI8 => {
                let val = self.read_i8()?;
                self.stack.push(Value::I8(val));
            },
            Op::PushI16 => {
                let val = self.read_i16()?;
                self.stack.push(Value::I16(val));
            },
            Op::PushI32 => {
                let val = self.read_i32()?;
                self.stack.push(Value::I32(val));
            },
            Op::PushF32 => {
                let val = self.read_f32()?;
                self.stack.push(Value::F32(val));
            },
            Op::PushI64 => {
                let val = self.read_i64()?;
                self.stack.push(Value::I64(val));
            },
            Op::PushF64 => {
                let val = self.read_f64()?;
                self.stack.push(Value::F64(val));
            },
            Op::PushString => {
                let str_id = self.read_i16()?;
                self.stack.push(Value::String(str_id as u16));
            },
            Op::AddI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                let (val, overflowed) = a.overflowing_add(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I32(val));
            },
            Op::SubI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                let (val, overflowed) = a.overflowing_sub(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I32(val));
            },
            Op::MulI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                let (val, overflowed) = a.overflowing_mul(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I32(val));
            },
            Op::DivI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                if b == 0 {
                    return Err("Division by zero".to_string());
                }
                let (val, overflowed) = a.overflowing_div(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I32(val));
            },
            Op::AddI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                let (val, overflowed) = a.overflowing_add(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I64(val));
            },
            Op::SubI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                let (val, overflowed) = a.overflowing_sub(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I64(val));
            },
            Op::MulI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                let (val, overflowed) = a.overflowing_mul(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I64(val));
            },
            Op::DivI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                if b == 0 {
                    return Err("Division by zero".to_string());
                }
                let (val, overflowed) = a.overflowing_div(b);
                if overflowed {
                    self.runtime_error(format!("Integer overflow at instr {}", self.ip));
                }
                self.stack.push(Value::I64(val));
            },
            Op::AddF32 => {
                let b = self.stack.pop()?.as_f32()?;
                let a = self.stack.pop()?.as_f32()?;
                self.stack.push(Value::F32(a + b));
            },
            Op::SubF32 => {
                let b = self.stack.pop()?.as_f32()?;
                let a = self.stack.pop()?.as_f32()?;
                self.stack.push(Value::F32(a - b));
            },
            Op::MulF32 => {
                let b = self.stack.pop()?.as_f32()?;
                let a = self.stack.pop()?.as_f32()?;
                self.stack.push(Value::F32(a * b));
            },
            Op::DivF32 => {
                let b = self.stack.pop()?.as_f32()?;
                let a = self.stack.pop()?.as_f32()?;
                self.stack.push(Value::F32(a / b));
            },
            Op::AddF64 => {
                let b = self.stack.pop()?.as_f64()?;
                let a = self.stack.pop()?.as_f64()?;
                self.stack.push(Value::F64(a + b));
            },
            Op::SubF64 => {
                let b = self.stack.pop()?.as_f64()?;
                let a = self.stack.pop()?.as_f64()?;
                self.stack.push(Value::F64(a - b));
            },
            Op::MulF64 => {
                let b = self.stack.pop()?.as_f64()?;
                let a = self.stack.pop()?.as_f64()?;
                self.stack.push(Value::F64(a * b));
            },
            Op::DivF64 => {
                let b = self.stack.pop()?.as_f64()?;
                let a = self.stack.pop()?.as_f64()?;
                self.stack.push(Value::F64(a / b));
            },
            Op::ModI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a % b));
            },
            Op::ModI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a % b));
            },
            Op::LoadLocal => {
                let slot = self.read_u16()?;
                let value = self
                    .get_local(slot)
                    .ok_or_else(|| format!("invalid local slot {slot}"))?;
                self.stack.push(value.clone());
            },
            Op::StoreLocal => {
                let slot = self.read_u16()?;
                let value = self.stack.pop()?;
                self.set_local(slot, value);
            },
            Op::Jump => {
                let offset = self.read_u32()?;
                self.ip = offset as usize;
            },
            Op::Call => {
                let func_id = self.read_u16()?;
                let _arg_count = self.read_u8()? as usize;

                self.execute_function(func_id)?;
            },
            Op::Ret => {
                if self.call_frames.len() <= 1 {
                    self.state = State::Finished;
                } else if let Some(next_frame) = self.call_frames.pop() {
                    self.stack.truncate(next_frame.base_stack);
                    self.ip = next_frame.return_address;
                } else {
                    self.runtime_error("Unhandled return".to_string());
                    self.state = State::Halted;
                }
            },
            Op::RetValue => {
                let return_value = self.stack.pop()?;

                if self.call_frames.len() <= 1 {
                    self.stack.push(return_value);
                    self.state = State::Finished;
                } else if let Some(next_frame) = self.call_frames.pop() {
                    self.stack.truncate(next_frame.base_stack);
                    self.stack.push(return_value);
                    self.ip = next_frame.return_address;
                } else {
                    self.runtime_error("Unhandled return".to_string());
                    self.state = State::Halted;
                }
            },
            Op::EqualI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a == b));
            },
            Op::NotEqualI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a != b));
            },
            Op::GreaterThanI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a > b));
            },
            Op::GreaterThanEqualI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a >= b));
            },
            Op::LessThanI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a < b));
            },
            Op::LessThanEqualI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::Bool(a <= b));
            },
            Op::EqualI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a == b));
            },
            Op::NotEqualI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a != b));
            },
            Op::GreaterThanI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a > b));
            },
            Op::GreaterThanEqualI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a >= b));
            },
            Op::LessThanI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a < b));
            },
            Op::LessThanEqualI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::Bool(a <= b));
            },
            Op::LogicalAnd => {
                let b = self.stack.pop()?.as_bool();
                let a = self.stack.pop()?.as_bool();
                self.stack.push(Value::Bool(a && b));
            },
            Op::LogicalOr => {
                let b = self.stack.pop()?.as_bool();
                let a = self.stack.pop()?.as_bool();
                self.stack.push(Value::Bool(a || b));
            },
            Op::JumpEqual => {
                let offset = self.read_u32()?;
                let cond = self.stack.pop()?.as_bool();
                if cond {
                    self.ip = offset as usize;
                }
            },
            Op::JumpNotEqual => {
                let offset = self.read_u32()?;
                let cond = self.stack.pop()?.as_bool();
                if !cond {
                    self.ip = offset as usize;
                }
            },
            Op::BitAndI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a & b));
            },
            Op::BitOrI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a | b));
            },
            Op::BitXorI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a ^ b));
            },
            Op::BitAndI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a & b));
            },
            Op::BitOrI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a | b));
            },
            Op::BitXorI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a ^ b));
            },
            Op::BitNotI32 => {
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(!a));
            },
            Op::BitNotI64 => {
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(!a));
            },
            Op::ShiftLeftI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a << b));
            },
            Op::ShiftRightI32 => {
                let b = self.stack.pop()?.as_i32()?;
                let a = self.stack.pop()?.as_i32()?;
                self.stack.push(Value::I32(a >> b));
            },
            Op::ShiftLeftI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a << b));
            },
            Op::ShiftRightI64 => {
                let b = self.stack.pop()?.as_i64()?;
                let a = self.stack.pop()?.as_i64()?;
                self.stack.push(Value::I64(a >> b));
            },
            Op::LessThanU32 => {
                let b = self.stack.pop()?.as_i32()? as u32;
                let a = self.stack.pop()?.as_i32()? as u32;
                self.stack.push(Value::Bool(a < b));
            },
            Op::LessThanEqualU32 => {
                let b = self.stack.pop()?.as_i32()? as u32;
                let a = self.stack.pop()?.as_i32()? as u32;
                self.stack.push(Value::Bool(a <= b));
            },
            Op::GreaterThanU32 => {
                let b = self.stack.pop()?.as_i32()? as u32;
                let a = self.stack.pop()?.as_i32()? as u32;
                self.stack.push(Value::Bool(a > b));
            },
            Op::GreaterThanEqualU32 => {
                let b = self.stack.pop()?.as_i32()? as u32;
                let a = self.stack.pop()?.as_i32()? as u32;
                self.stack.push(Value::Bool(a >= b));
            },
            Op::LessThanU64 => {
                let b = self.stack.pop()?.as_i64()? as u64;
                let a = self.stack.pop()?.as_i64()? as u64;
                self.stack.push(Value::Bool(a < b));
            },
            Op::LessThanEqualU64 => {
                let b = self.stack.pop()?.as_i64()? as u64;
                let a = self.stack.pop()?.as_i64()? as u64;
                self.stack.push(Value::Bool(a <= b));
            },
            Op::GreaterThanU64 => {
                let b = self.stack.pop()?.as_i64()? as u64;
                let a = self.stack.pop()?.as_i64()? as u64;
                self.stack.push(Value::Bool(a > b));
            },
            Op::GreaterThanEqualU64 => {
                let b = self.stack.pop()?.as_i64()? as u64;
                let a = self.stack.pop()?.as_i64()? as u64;
                self.stack.push(Value::Bool(a >= b));
            },
        }

        Ok(())
    }

    fn current_frame(&self) -> Option<&CallFrame> { self.call_frames.last() }

    fn current_frame_mut(&mut self) -> Option<&mut CallFrame> { self.call_frames.last_mut() }

    fn get_local(&self, local_id: u16) -> Option<&Value> { self.current_frame()?.get_local(local_id) }

    fn set_local(&mut self, local_id: u16, value: Value) -> Option<()> {
        self.current_frame_mut()?.set_local(local_id, value)
    }

    fn read_u8(&mut self) -> Result<u8, String> {
        let val = self.code[self.ip];
        self.ip += 1;
        Ok(val)
    }

    fn read_i8(&mut self) -> Result<i8, String> { Ok(self.read_u8()? as i8) }

    fn read_u16(&mut self) -> Result<u16, String> {
        let bytes = &self.code[self.ip..self.ip + 2];
        self.ip += 2;
        Ok(u16::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read u16".to_string())?,
        ))
    }

    fn read_i16(&mut self) -> Result<i16, String> {
        let bytes = &self.code[self.ip..self.ip + 2];
        self.ip += 2;
        Ok(i16::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read i16".to_string())?,
        ))
    }

    fn read_i32(&mut self) -> Result<i32, String> {
        let bytes = &self.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(i32::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read i32".to_string())?,
        ))
    }

    fn read_u32(&mut self) -> Result<u32, String> {
        let bytes = &self.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(u32::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read u32".to_string())?,
        ))
    }

    fn _read_i64(&mut self) -> Result<i64, String> {
        let bytes = &self.code[self.ip..self.ip + 8];
        self.ip += 8;
        Ok(i64::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read i64".to_string())?,
        ))
    }

    fn read_f32(&mut self) -> Result<f32, String> {
        let bytes = &self.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(f32::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read f32".to_string())?,
        ))
    }

    fn read_i64(&mut self) -> Result<i64, String> {
        let bytes = &self.code[self.ip..self.ip + 8];
        self.ip += 8;
        Ok(i64::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read i64".to_string())?,
        ))
    }

    fn read_f64(&mut self) -> Result<f64, String> {
        let bytes = &self.code[self.ip..self.ip + 8];
        self.ip += 8;
        Ok(f64::from_le_bytes(
            bytes.try_into().map_err(|_| "Failed to read f64".to_string())?,
        ))
    }

    fn runtime_error(&mut self, err: impl std::fmt::Display) { self.errors.push(err.to_string()); }
}
