use crate::bytecode::{Bytecode, Opcode};
use crate::heap::{Heap, Word};
use std::time::Instant;
use std::io::{self, Read};

pub const STACK_SIZE: usize = 1 << 14;
pub const HEAP_SIZE: usize = 1 << 20;

/// The VM struct
pub struct VM {
    pub bytecode: Bytecode,
    stack: [Word; STACK_SIZE], // Fixed-size stack of words
    stack_ptr: usize,          // Stack pointer. Points to the next free slot in the stack.
    ip: usize,                 // Instruction pointer
    heap: Heap,                // The heap
    start_time: Instant
}

impl VM {
    /// Create a new `VM` with the given bytecode
    pub fn new(bytecode: Bytecode) -> Self {
        VM {
            bytecode,
            stack: [Word::from_int(0); STACK_SIZE], // Initialize stack with zeroes
            stack_ptr: 0,
            ip: 0,
            heap: Heap::new(HEAP_SIZE), // The heap
            start_time: Instant::now(),
        }
    }

    #[cfg(debug_assertions)]
    fn print_state(&self) {
        print!("Stack: ");
        for i in 0..self.stack_ptr {
            print!("| {:?} ", self.stack[i]);
        }
        println!("|");

        let opcode: Option<Opcode> = Opcode::from_u8(self.bytecode.instructions[self.ip]);

        println!("IP 0x{:X}: {:?}", self.ip, opcode);
    }

    #[cfg(not(debug_assertions))]
    fn print_state(&self) {}

    pub fn run(&mut self) {
        //panic!("Implement me!")

        while self.ip < self.bytecode.instructions.len() {
            // Fetch the current opcode
            let opcode = Opcode::from_u8(self.bytecode.instructions[self.ip]);
            self.ip += 1; // Move to the next instruction
    
            match opcode {
                // Control Flow Instructions
                Some(Opcode::Halt) => {
                    // Terminate the VM
                    //println!("Execution halted.");
                    break;
                }

                Some(Opcode::Jump) => {
                    /*
                    println!(
                        "Executing at IP: 0x{:X}, Opcode: 0x{:02X}",
                        self.ip, self.bytecode.instructions[self.ip]
                    );
                    println!(
                        "Executing at IP: 0x{:X}, Opcode: 0x{:02X}",
                        self.ip, self.bytecode.instructions[self.ip + 1]
                    );
                    println!(
                        "Executing at IP: 0x{:X}, Opcode: 0x{:02X}",
                        self.ip, self.bytecode.instructions[self.ip + 2]
                    );
                    */
                    // Read the jump address (2 bytes, little-endian)
                    // Read the jump address (little-endian order)
                    let addr = u16::from_le_bytes([
                        self.bytecode.instructions[self.ip],     // LSB (low byte)
                        self.bytecode.instructions[self.ip + 1], // MSB (high byte)
                    ]);
                    
                    /*
                    println!(
                        "jumping at 0x{:02X}", addr
                    );
                    */

                    // Validate the jump address
                    if addr as usize >= self.bytecode.instructions.len() {
                        panic!(
                            "Invalid jump address: 0x{:X} (bytecode length: 0x{:X})",
                            addr,
                            self.bytecode.instructions.len()
                        );
                    }
    
                    // Jump to the address
                    self.ip = addr as usize;
                }
    
                Some(Opcode::Jnz) => {
                    // Pop the top value from the stack
                    let value = self.stack[self.stack_ptr - 1].to_int();
                    self.stack_ptr -= 1;
    
                    // If the value is nonzero, perform the jump
                    if value != 0 {
                        // Read the jump address (2 bytes, little-endian)
                        let addr = u16::from_le_bytes([
                            self.bytecode.instructions[self.ip],     // LSB (low byte)
                            self.bytecode.instructions[self.ip + 1], // MSB (high byte)
                        ]);
                        self.ip += 2; // Move past the address bytes
    
                        // Validate the jump address
                        if addr as usize >= self.bytecode.instructions.len() {
                            panic!(
                                "Invalid jump address: 0x{:X} (bytecode length: 0x{:X})",
                                addr,
                                self.bytecode.instructions.len()
                            );
                        }
    
                        // Jump to the address
                        self.ip = addr as usize;
                    } else {
                        // If the value is zero, skip the jump and move to the next instruction
                        self.ip += 2;
                    }
                }
    
                Some(Opcode::Jumpi) => {
                    // Ensure the stack has at least one value
                    if self.stack_ptr == 0 {
                        panic!("Stack underflow: Cannot perform 'jumpi' with an empty stack");
                    }
                
                    // Pop the target address from the stack
                    let addr = self.stack[self.stack_ptr - 1].to_int() as usize;
                    self.stack_ptr -= 1;
                
                    // Validate the jump address
                    if addr >= self.bytecode.instructions.len() {
                        panic!(
                            "Invalid jump address: 0x{:X} (bytecode length: 0x{:X})",
                            addr,
                            self.bytecode.instructions.len()
                        );
                    }
                
                    // Jump to the new address
                    self.ip = addr;
                }
                

                // Stack Manipulation Instructions
                Some(Opcode::Push1) => {
                    // Read a 1-byte signed integer
                    let lit = self.bytecode.instructions[self.ip] as i8;
                    self.ip += 1;
                
                    // Push the integer onto the stack
                    self.stack[self.stack_ptr] = Word::from_int(lit as i32);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Push2) => {
                    // Read a 2-byte signed integer (little-endian)
                    let lit = (self.bytecode.instructions[self.ip + 1] as i16) << 8
                        | (self.bytecode.instructions[self.ip] as i16);
                    self.ip += 2;
                
                    // Push the integer onto the stack
                    self.stack[self.stack_ptr] = Word::from_int(lit as i32);
                    self.stack_ptr += 1;
                }
    
                Some(Opcode::Push4) => {
                    // Read a 4-byte signed integer (little-endian)
                    let lit = (self.bytecode.instructions[self.ip + 3] as i32) << 24
                        | (self.bytecode.instructions[self.ip + 2] as i32) << 16
                        | (self.bytecode.instructions[self.ip + 1] as i32) << 8
                        | (self.bytecode.instructions[self.ip] as i32);
                    self.ip += 4;
                
                    self.stack[self.stack_ptr] = Word::from_int(lit as i32);
                    self.stack_ptr += 1;
                }
                

                // Memory Management Instructions
                Some(Opcode::Alloc) => { // alloc (0x1A)
                    // Ensure the stack has at least two values for tag and size
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'alloc'");
                    }
                
                    // Pop size and tag (tag is popped first, then size)
                    let tag = self.stack[self.stack_ptr - 1].to_int() as u8;
                    let size = self.stack[self.stack_ptr - 2].to_int() as usize;
                    self.stack_ptr -= 2;
                
                    // Ensure the stack has enough elements for the block fields
                    if self.stack_ptr < size {
                        panic!(
                            "Stack underflow: Not enough fields for allocation (expected {}, got {})",
                            size, self.stack_ptr
                        );
                    }
                
                    // Pop `size` elements from the stack (last popped is the first field)
                    let mut fields = Vec::with_capacity(size);
                    for _ in 0..size {
                        fields.push(self.stack[self.stack_ptr - 1]);
                        self.stack_ptr -= 1;
                    }
                    fields.reverse(); // Ensure correct field order
                
                    // Allocate block in the heap
                    let pointer = self.heap.alloc(size, tag, &fields)
                        .expect("Heap allocation failed");
                
                    // Push the address of the allocated block to the stack
                    self.stack[self.stack_ptr] = Word::from_pointer(pointer);
                    self.stack_ptr += 1;
                }        

                Some(Opcode::Load) => { // load (0x1B)
                    // Ensure the stack has at least one value (pointer)
                    if self.stack_ptr == 0 {
                        panic!("Stack underflow: Cannot perform 'load'");
                    }
                
                    // Pop pointer from the stack
                    let pointer = self.stack[self.stack_ptr - 1].to_pointer();
                    self.stack_ptr -= 1;
                
                    // Ensure bytecode has 4 more bytes for offset
                    if self.ip + 4 > self.bytecode.instructions.len() {
                        panic!("Load instruction at 0x{:X} is missing offset bytes", self.ip - 1);
                    }
                
                    // Read offset (4-byte unsigned integer, little-endian)
                    let offset = u32::from_le_bytes([
                        self.bytecode.instructions[self.ip],
                        self.bytecode.instructions[self.ip + 1],
                        self.bytecode.instructions[self.ip + 2],
                        self.bytecode.instructions[self.ip + 3],
                    ]) as usize;
                    /*
                    println!(
                        "jumping at 0x{:02X}", offset
                    );
                    */
                    self.ip += 4; // Move past offset bytes
                
                    // Retrieve value from the heap
                    let value = self.heap.load(pointer, offset)
                        .expect("Invalid heap access");
                
                    // Push the loaded value onto the stack
                    self.stack[self.stack_ptr] = value;
                    self.stack_ptr += 1;
                }     

                Some(Opcode::Dup) => {
                    // Ensure the bytecode has at least 1 more byte for depth i
                    if self.ip >= self.bytecode.instructions.len() {
                        panic!(
                            "Dup instruction at 0x{:X} is missing depth byte",
                            self.ip - 1
                        );
                    }
                
                    // Read depth i (next byte)
                    let i = self.bytecode.instructions[self.ip] as usize;
                    self.ip += 1; // Move past the depth byte
                
                    // Ensure stack has enough elements to duplicate
                    if self.stack_ptr <= i {
                        panic!(
                            "Stack underflow: Cannot duplicate depth {} (stack size: {})",
                            i, self.stack_ptr
                        );
                    }
                
                    // Get the value at depth i
                    let value = self.stack[self.stack_ptr - 1 - i];
                
                    // Push the copied value to the top of the stack
                    self.stack[self.stack_ptr] = value;
                    self.stack_ptr += 1;
                }

                Some(Opcode::Swap) => { // swap (0x05)
                    if self.ip >= self.bytecode.instructions.len() {
                        panic!("Swap instruction at 0x{:X} is missing depth byte", self.ip - 1);
                    }
                
                    let i = self.bytecode.instructions[self.ip] as usize; // Read depth i
                    self.ip += 1;
                
                    if self.stack_ptr <= i {
                        panic!("Stack underflow: Cannot swap depth {} (stack size: {})", i, self.stack_ptr);
                    }
                
                    let top = self.stack[self.stack_ptr - 1]; // Top of stack
                    let depth_value = self.stack[self.stack_ptr - 1 - i]; // Value at depth i
                
                    // Swap them
                    self.stack[self.stack_ptr - 1] = depth_value;
                    self.stack[self.stack_ptr - 1 - i] = top;
                }

                Some(Opcode::Drop) => { // drop (0x06)
                    if self.stack_ptr == 0 {
                        panic!("Stack underflow: Cannot drop value from an empty stack");
                    }
                    self.stack_ptr -= 1; // Simply remove the top value
                }
                
    
                // Arithmetic Operations
                Some(Opcode::Add) => { // add (0x0A)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'add'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a + b);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Sub) => { // sub (0x0B)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'sub'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a - b);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Mul) => { // mul (0x0C)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'mul'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a * b);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Div) => { // div (0x0D)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'div'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    if b == 0 {
                        panic!("Division by zero error");
                    }
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a / b);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Mod) => { // mod (0x0E)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'mod'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    if b == 0 {
                        panic!("Modulo by zero error");
                    }
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a % b);
                    self.stack_ptr += 1;
                }               

                //Comparison Operations
                Some(Opcode::Eq) => { // eq (0x0F)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'eq'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a == b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Ne) => { // ne (0x10)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'ne'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a != b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Lt) => { // lt (0x11)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'lt'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a < b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Gt) => { // gt (0x12)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'gt'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a > b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Le) => { // le (0x13)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'le'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a <= b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Ge) => { // ge (0x14)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'ge'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(if a >= b { 1 } else { 0 });
                    self.stack_ptr += 1;
                }

                //logical operations
                Some(Opcode::Not) => { // not (0x15)
                    if self.stack_ptr == 0 {
                        panic!("Stack underflow: Cannot perform 'not'");
                    }
                    let value = self.stack[self.stack_ptr - 1].to_int();
                    self.stack_ptr -= 1;
                    self.stack[self.stack_ptr] = Word::from_int(if value == 0 { 1 } else { 0 });
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::And) => { // and (0x16)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'and'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a & b);
                    self.stack_ptr += 1;
                }
                
                Some(Opcode::Or) => { // or (0x17)
                    if self.stack_ptr < 2 {
                        panic!("Stack underflow: Cannot perform 'or'");
                    }
                    let b = self.stack[self.stack_ptr - 1].to_int();
                    let a = self.stack[self.stack_ptr - 2].to_int();
                    self.stack_ptr -= 2;
                    self.stack[self.stack_ptr] = Word::from_int(a | b);
                    self.stack_ptr += 1;
                }

                Some(Opcode::Input) => { // 0x18
                    let mut buffer = [0; 1]; // Buffer to store one byte
                    io::stdin().read_exact(&mut buffer).expect("Failed to read input");
                
                    let ascii_code = buffer[0] as i32; // Convert to ASCII integer
                
                    // Push ASCII code onto the stack
                    self.stack[self.stack_ptr] = Word::from_int(ascii_code);
                    self.stack_ptr += 1;
                }   

                // Input/Output Instructions
                Some(Opcode::Output) => {
                    // Pop the top value from the stack
                    let value = self.stack[self.stack_ptr - 1].to_int();
                    self.stack_ptr -= 1;
    
                    // Print the corresponding ASCII character
                    //print!("{}", value as u8 as char);
                    print!("{}", value as u8 as char);
                }

                Some(Opcode::Clock) => {
                    // Calculate elapsed time in seconds with high precision
                    let elapsed = self.start_time.elapsed().as_secs_f64();
                
                    // Print elapsed time with 4 decimal places
                    println!("{:.4} seconds", elapsed);
                }
                
                /*
                // Unimplemented Instructions
                Some(op) => {
                    panic!("Unimplemented opcode: {:?}", op);
                }
                */
                None => {
                    panic!("Invalid opcode at address {}", self.ip - 1);
                }
            }
    
            // Print the VM state for debugging (if enabled)
            self.print_state();
        }
    }
}
