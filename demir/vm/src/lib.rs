use codegen::{Module, opcode::Op};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    True,
    False,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
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
            Value::True => true,
            Value::False => false,
            Value::Null => false,
            Value::I32(0) | Value::I64(0) => false,
            _ => true,
        }
    }
}

struct CallFrame {
    func_id: u16,
    base_stack: usize,
    return_address: usize,
    locals: Vec<Value>,
}

enum State {
    Executing,
    Finished,
    RuntimeError(String),
}

pub struct VM {
    state: State,
    module: Module,
    stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
    ip: usize,
}

impl CallFrame {
    pub fn new(func_id: u16, base_stack: usize, return_address: usize, local_count: u16) -> Self {
        Self {
            func_id,
            base_stack,
            return_address,
            locals: vec![Value::Null; local_count as usize],
        }
    }

    pub fn get_local(&self, index: u16) -> &Value { &self.locals[index as usize] }

    pub fn set_local(&mut self, index: u16, value: Value) { self.locals[index as usize] = value; }
}

impl VM {
    pub fn new(module: Module) -> Self {
        Self {
            state: State::Executing,
            module,
            stack: Vec::new(),
            call_frames: Vec::new(),
            ip: 0,
        }
    }

    pub fn execute_function(&mut self, func_id: u16) -> Result<Value, String> {
        let func = self
            .module
            .functions
            .get(func_id as usize)
            .expect("Invalid function Id");

        let mut frame = CallFrame::new(func.id, self.stack.len(), self.ip, func.local_count);
        for i in (0..func.param_count).rev() {
            if let Some(arg) = self.stack.pop() {
                frame.locals[i as usize] = arg;
            }
        }

        self.call_frames.push(frame);
        self.ip = func.address as usize;

        while matches!(self.state, State::Executing) {
            self.execute_op()?;
        }

        Ok(self.stack.last().unwrap_or(&Value::Null).clone())
    }

    fn execute_op(&mut self) -> Result<(), String> {
        if self.ip >= self.module.code.len() {
            return Err("Instruction pointer is out of bounds".to_string());
        }

        let op = Op::from_u8(self.module.code[self.ip]).ok_or("Invalid Instruction".to_string())?;
        self.ip += 1;

        match op {
            Op::Nop => {},

            // Constants
            Op::PushNull => self.stack.push(Value::Null),
            Op::PushTrue => self.stack.push(Value::True),
            Op::PushFalse => self.stack.push(Value::False),
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

            // Arithmetic - I32
            Op::AddI32 => {
                let b = self.pop()?.as_i32()?;
                let a = self.pop()?.as_i32()?;
                self.stack.push(Value::I32(a + b));
            },
            Op::SubI32 => {
                let b = self.pop()?.as_i32()?;
                let a = self.pop()?.as_i32()?;
                self.stack.push(Value::I32(a - b));
            },
            Op::MulI32 => {
                let b = self.pop()?.as_i32()?;
                let a = self.pop()?.as_i32()?;
                self.stack.push(Value::I32(a * b));
            },
            Op::DivI32 => {
                let b = self.pop()?.as_i32()?;
                let a = self.pop()?.as_i32()?;
                if b == 0 {
                    return Err("Division by zero".to_string());
                }
                self.stack.push(Value::I32(a / b));
            },

            // Arithmetic - I64
            Op::AddI64 => {
                let b = self.pop()?.as_i64()?;
                let a = self.pop()?.as_i64()?;
                self.stack.push(Value::I64(a + b));
            },
            Op::SubI64 => {
                let b = self.pop()?.as_i64()?;
                let a = self.pop()?.as_i64()?;
                self.stack.push(Value::I64(a - b));
            },
            Op::MulI64 => {
                let b = self.pop()?.as_i64()?;
                let a = self.pop()?.as_i64()?;
                self.stack.push(Value::I64(a * b));
            },
            Op::DivI64 => {
                let b = self.pop()?.as_i64()?;
                let a = self.pop()?.as_i64()?;
                if b == 0 {
                    return Err("Division by zero".to_string());
                }
                self.stack.push(Value::I64(a / b));
            },

            // Arithmetic - F32
            Op::AddF32 => {
                let b = self.pop()?.as_f32()?;
                let a = self.pop()?.as_f32()?;
                self.stack.push(Value::F32(a + b));
            },
            Op::SubF32 => {
                let b = self.pop()?.as_f32()?;
                let a = self.pop()?.as_f32()?;
                self.stack.push(Value::F32(a - b));
            },
            Op::MulF32 => {
                let b = self.pop()?.as_f32()?;
                let a = self.pop()?.as_f32()?;
                self.stack.push(Value::F32(a * b));
            },
            Op::DivF32 => {
                let b = self.pop()?.as_f32()?;
                let a = self.pop()?.as_f32()?;
                self.stack.push(Value::F32(a / b));
            },

            // Arithmetic - F64
            Op::AddF64 => {
                let b = self.pop()?.as_f64()?;
                let a = self.pop()?.as_f64()?;
                self.stack.push(Value::F64(a + b));
            },
            Op::SubF64 => {
                let b = self.pop()?.as_f64()?;
                let a = self.pop()?.as_f64()?;
                self.stack.push(Value::F64(a - b));
            },
            Op::MulF64 => {
                let b = self.pop()?.as_f64()?;
                let a = self.pop()?.as_f64()?;
                self.stack.push(Value::F64(a * b));
            },
            Op::DivF64 => {
                let b = self.pop()?.as_f64()?;
                let a = self.pop()?.as_f64()?;
                self.stack.push(Value::F64(a / b));
            },

            // Local variables
            Op::LoadLocal => {
                let slot = self.read_u16()?;
                let value = self.get_local(slot);
                self.stack.push(value.clone());
            },
            Op::StoreLocal => {
                let slot = self.read_u16()?;
                let value = self.pop()?;
                self.set_local(slot, value);
            },

            // Control Flow
            Op::Jump => {
                let offset = self.read_u32()? as i32;
                self.ip = (self.ip as i32 + offset) as usize;
            },
            Op::Call => {
                let func_id = self.read_u16()?;
                let _arg_count = self.read_u8()? as usize;

                self.execute_function(func_id)?;
            },
            Op::Ret => {
                if self.call_frames.len() <= 1 {
                    self.state = State::Finished;
                } else {
                    let frame = self.call_frames.pop().unwrap();
                    self.stack.truncate(frame.base_stack);
                    self.ip = frame.return_address;
                }
            },
            Op::RetValue => {
                let return_value = self.pop()?;

                if self.call_frames.len() <= 1 {
                    self.stack.push(return_value);
                    self.state = State::Finished;
                } else {
                    let frame = self.call_frames.pop().unwrap();
                    self.stack.truncate(frame.base_stack);
                    self.stack.push(return_value);
                    self.ip = frame.return_address;
                }
            },
            Op::PushString => todo!(),
        }

        Ok(())
    }

    fn current_frame(&self) -> &CallFrame { self.call_frames.last().expect("No call frame") }

    fn current_frame_mut(&mut self) -> &mut CallFrame { self.call_frames.last_mut().expect("No call frame") }

    fn get_local(&self, local_id: u16) -> &Value { self.current_frame().get_local(local_id) }

    fn set_local(&mut self, local_id: u16, value: Value) { self.current_frame_mut().set_local(local_id, value) }

    fn pop(&mut self) -> Result<Value, String> { self.stack.pop().ok_or("Stack underflow".to_string()) }

    fn read_u8(&mut self) -> Result<u8, String> {
        let val = self.module.code[self.ip];
        self.ip += 1;
        Ok(val)
    }

    fn read_i8(&mut self) -> Result<i8, String> { Ok(self.read_u8()? as i8) }

    fn read_u16(&mut self) -> Result<u16, String> {
        let bytes = &self.module.code[self.ip..self.ip + 2];
        self.ip += 2;
        Ok(u16::from_le_bytes(bytes.try_into().expect("Failed to read u16")))
    }

    fn read_i16(&mut self) -> Result<i16, String> {
        let bytes = &self.module.code[self.ip..self.ip + 2];
        self.ip += 2;
        Ok(i16::from_le_bytes(bytes.try_into().expect("Failed to read i16")))
    }

    fn read_i32(&mut self) -> Result<i32, String> {
        let bytes = &self.module.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(i32::from_le_bytes(bytes.try_into().expect("Failed to read i32")))
    }

    fn read_u32(&mut self) -> Result<u32, String> {
        let bytes = &self.module.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(u32::from_le_bytes(bytes.try_into().expect("Failed to read u32")))
    }

    fn read_i64(&mut self) -> Result<i64, String> {
        let bytes = &self.module.code[self.ip..self.ip + 8];
        self.ip += 8;
        Ok(i64::from_le_bytes(bytes.try_into().expect("Failed to read i64")))
    }

    fn read_f32(&mut self) -> Result<f32, String> {
        let bytes = &self.module.code[self.ip..self.ip + 4];
        self.ip += 4;
        Ok(f32::from_le_bytes(bytes.try_into().expect("Failed to read f32")))
    }

    fn read_f64(&mut self) -> Result<f64, String> {
        let bytes = &self.module.code[self.ip..self.ip + 8];
        self.ip += 8;
        Ok(f64::from_le_bytes(bytes.try_into().expect("Failed to read f64")))
    }
}
