use std::array::TryFromSliceError;

use crate::Op;

pub fn print_code(code: &[u8]) -> Result<(), TryFromSliceError> {
    let mut ip = 0;
    loop {
        if ip >= code.len() {
            return Ok(());
        }

        let op = Op::from_u8(code[ip]).unwrap();
        print!("{:08X}: {:?}", ip, op);
        ip += 1;

        match op {
            Op::PushI8 => {
                let val = code[ip];
                println!(" {}", val);
                ip += 1;
            },
            Op::PushI16 => {
                let bytes = &code[ip..ip + 2];
                let val = i16::from_le_bytes(bytes.try_into()?);
                println!(" {}", val);
                ip += 2;
            },
            Op::PushI32 => {
                let bytes = &code[ip..ip + 4];
                let val = i32::from_le_bytes(bytes.try_into()?);
                println!(" {}", val);
                ip += 4;
            },
            Op::PushF32 => {
                let bytes = &code[ip..ip + 4];
                let val = f32::from_le_bytes(bytes.try_into()?);
                println!(" {}", val);
                ip += 4;
            },
            Op::LoadLocal | Op::StoreLocal => {
                let bytes = &code[ip..ip + 2];
                let slot = u16::from_le_bytes(bytes.try_into()?);
                println!(" local[{}]", slot);
                ip += 2;
            },
            Op::Jump => {
                let bytes = &code[ip..ip + 4];
                let offset = u32::from_le_bytes(bytes.try_into()?);
                println!(" -> 0x{:08X}", ip as u32 + offset);
                ip += 4;
            },
            Op::Call => {
                let bytes = &code[ip..ip + 2];
                let func_id = u16::from_le_bytes(bytes.try_into()?);
                let arg_count = code[ip + 2];
                println!(" func_{} (args: {})", func_id, arg_count);
                ip += 3;
            },
            Op::PushString => {
                let bytes = &code[ip..ip + 2];
                let str_id = u16::from_le_bytes(bytes.try_into()?);
                println!(" {}", str_id);
                ip += 2;
            },
            _ => println!(),
        }
    }
}
