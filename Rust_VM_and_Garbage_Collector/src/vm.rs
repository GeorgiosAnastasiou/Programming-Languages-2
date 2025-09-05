use crate::bytecode::{Bytecode, Opcode};
use crate::heap::{Heap, Word};
use std::io::{self, Read, Write};
use std::time::Instant;

pub const STACK_SIZE: usize = 1 << 14;
pub const HEAP_SIZE: usize = 1 << 20;

/// The VM struct
pub struct VM {
    pub bytecode: Bytecode,
    stack: [Word; STACK_SIZE], // Fixed-size stack of words
    stack_ptr: usize,          // Points to the next free slot in the stack.
    ip: usize,                 // Instruction pointer into the bytecode
    heap: Heap,                // The heap for dynamic memory
}

// edw einai pou crasharei. 'the size is 1 but the pointer is 1' kati tetoio
// telika itan apla lousimo sto pws to kalousa lol

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        VM {
            bytecode,
            stack: [Word::from_int(0); STACK_SIZE],
            stack_ptr: 0,
            ip: 0,
            heap: Heap::new(HEAP_SIZE),
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

    // Push a word onto the stack.
    fn push(&mut self, word: Word) { //wraia, edw kanei store mia leksi basically
        if self.stack_ptr >= STACK_SIZE {
            panic!("Stack overflow");
        }
        self.stack[self.stack_ptr] = word;
        self.stack_ptr += 1;
    }

    // Pop a word from the stack. //ena apo auta ta duo den paei opws thaeprepe
    fn pop(&mut self) -> Word {
        if self.stack_ptr == 0 {
            panic!("Stack underflow");
        }
        // debugging
//        println!("Alloc: Popped tag_word (int representation): {}", tag_word.to_int());

//        println!("Alloc: Popped size_word (int representation): {}", size_word.to_int());
        self.stack_ptr -= 1;
        self.stack[self.stack_ptr]
    }

    // Helper to read one byte from the bytecode.
    fn read_byte(&mut self) -> u8 {
        let byte = self.bytecode.instructions[self.ip];
        self.ip += 1;
        byte
    }

    // Helper to read a 16-bit unsigned integer (little-endian).
    fn read_u16(&mut self) -> u16 {
        let low = self.read_byte() as u16;
        let high = self.read_byte() as u16;
        low | (high << 8)
    }

    // Helper to read a 32-bit unsigned integer (little-endian opws sto erg LS).
    fn read_u32(&mut self) -> u32 {
        let b0 = self.read_byte() as u32;
        let b1 = self.read_byte() as u32;
        let b2 = self.read_byte() as u32;
        let b3 = self.read_byte() as u32;
        b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }

    // Helper to read a 32-bit signed integer.
    fn read_i32(&mut self) -> i32 {
        self.read_u32() as i32
    }

    // Helper to read a 16-bit signed integer.
    fn read_i16(&mut self) -> i16 {
        self.read_u16() as i16
    }

    /// Execute the bytecode until a Halt opcode is encountered.
    pub fn run(&mut self) {
        let start_time = Instant::now();

        loop {
            #[cfg(debug_assertions)]
            self.print_state();

            // Ensure we haven't run off the end of the bytecode.
            if self.ip >= self.bytecode.instructions.len() {
                panic!("Reached end of bytecode without halt");
            }

            let opcode_byte = self.bytecode.instructions[self.ip];
            self.ip += 1;
            let opcode = Opcode::from_u8(opcode_byte)
                .unwrap_or_else(|| panic!("Invalid opcode {} at ip {}", opcode_byte, self.ip - 1));

            match opcode {
                // -- Control Flow ---
                Opcode::Halt => break,

                Opcode::Jump => {
                    let addr = self.read_u16() as usize;
                    self.ip = addr;
                }

                Opcode::Jnz => {
                    let addr = self.read_u16() as usize;
                    let cond = self.pop();
                    if cond.to_int() != 0 {
                        self.ip = addr;
                    }
                }

                Opcode::Jumpi => {
                    let target = self.pop().to_int() as usize;
                    self.ip = target;
                }

                // -- Stack Manipulation ---
                Opcode::Dup => {
                    let depth = self.read_byte() as usize;
                    if self.stack_ptr <= depth {
                        panic!("Dup: Stack does not have enough elements");
                    }
                    let index = self.stack_ptr - 1 - depth;
                    let value = self.stack[index];
                    self.push(value);
                }

                Opcode::Swap => {
                    let depth = self.read_byte() as usize;
                    if self.stack_ptr <= depth {
                        panic!("Swap: Stack does not have enough elements");
                    }
                    let top_index = self.stack_ptr - 1;
                    let swap_index = self.stack_ptr - 1 - depth;
                    self.stack.swap(top_index, swap_index);
                }

                Opcode::Drop => {
                    self.pop();
                }

                Opcode::Push4 => {
                    let val = self.read_i32();
                    self.push(Word::from_int(val));
                }

                Opcode::Push2 => {
                    let val = self.read_i16() as i32;
                    self.push(Word::from_int(val));
                }

                Opcode::Push1 => {
                    let val = self.read_byte() as i8 as i32;
                    self.push(Word::from_int(val));
                }

                // --- Arithmetic Operations ---
                Opcode::Add => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Word::from_int(a.to_int().wrapping_add(b.to_int())));
                }

                Opcode::Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Word::from_int(a.to_int().wrapping_sub(b.to_int())));
                }

                Opcode::Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Word::from_int(a.to_int().wrapping_mul(b.to_int())));
                }

                Opcode::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    if b.to_int() == 0 {
                        panic!("Division by zero");
                    }
                    self.push(Word::from_int(a.to_int() / b.to_int()));
                }

                Opcode::Mod => {
                    let b = self.pop();
                    let a = self.pop();
                    if b.to_int() == 0 {
                        panic!("Modulo by zero");
                    }
                    self.push(Word::from_int(a.to_int() % b.to_int()));
                }

                // --- binary operations (to apotelesma tous) ---
                Opcode::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() == b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::Ne => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() != b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() < b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() > b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::Le => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() <= b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::Ge => {
                    let b = self.pop();
                    let a = self.pop();
                    let res = if a.to_int() >= b.to_int() { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                // --- Logical Operations (pali binary apotelesma) ---
                Opcode::Not => {
                    let a = self.pop();
                    let res = if a.to_int() == 0 { 1 } else { 0 };
                    self.push(Word::from_int(res));
                }

                Opcode::And => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Word::from_int(a.to_int() & b.to_int()));
                }

                Opcode::Or => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Word::from_int(a.to_int() | b.to_int()));
                }

                // --- IO ---
                Opcode::Input => {
                    let mut input = [0];
                    io::stdin()
                        .read_exact(&mut input)
                        .expect("Failed to read input");
                    self.push(Word::from_int(input[0] as i32));
                }

                Opcode::Output => {
                    let a = self.pop();
                    let ch = (a.to_int() as u8) as char;
                    print!("{}", ch);
                    io::stdout().flush().unwrap();
                }

                // --- Memory Operations ---
                Opcode::Alloc => {
                    // Pops two values: tag then size.
                    // debug 
                    //println!("--- Opcode::Alloc ---");
                    let tag_word = self.pop();
                    let size_word = self.pop();
                    //println!("Alloc: Popped tag_word (int representation): {}", tag_word.to_int());
                    //println!("Alloc: Popped size_word (int representation): {}", size_word.to_int());
                    let tag = tag_word.to_int() as u8;
                    let n = size_word.to_int() as usize;
                    //println!("Alloc: Extracted tag: {}", tag);
                    //println!("Alloc: Extracted size (n): {}", n);
                    let mut fields = Vec::with_capacity(n);

                    for _ in 0..n {
                        let field_word = self.pop();
                        //println!("Alloc: Popped field_word (int representation): {}", field_word.to_int());
                        fields.push(field_word);
                    }

                    fields.reverse();
                    //println!("Alloc: Fields vector (int representations): {:?}", fields.iter().map(|w| w.to_int()).collect::<Vec<_>>());

                    //println!("Allocating {} words with tag {}", n, tag); 
                    match self.heap.alloc(n, tag, &fields) {
                        Some(ptr) => self.push(Word::from_pointer(ptr)),
                        None => {
                            // garbage collection
                            self.heap.gc(&self.stack[..self.stack_ptr]);
                            for word in &mut self.stack[..self.stack_ptr] {
                                if word.is_pointer() {
                                    let new_ptr = self.heap.forward(word.to_pointer());
                                    *word = Word::from_pointer(new_ptr);
                                }
                            }

                            // Retry allocation
                            match self.heap.alloc(n, tag, &fields) {
                                Some(ptr) => self.push(Word::from_pointer(ptr)),
                                None => panic!("Out of memory after garbage collection"),
                            }
                        }
                    }
                }


               
                Opcode::Load => {
                    // Reads a 4-byte little-endian offset.
                    let offset = self.read_u32() as usize;
                    let ptr_word = self.pop();

                    //println!("Load: ptr_word on stack before pop: {:?}", ptr_word);
                    //println!("Load: ptr_word.to_int() before check: {}", ptr_word.to_int());
                    // private... println!("Load: ptr_word.is_pointer() before check: {}", ptr_word.is_pointer()); // private

                    //if ptr_word.to_int() & 1 == 0 { // ara kanei 0 ara einai int
                    //if (ptr_word.w & 1) != 0 {
                    if !ptr_word.is_pointer() {
                        panic!("Load: Expected pointer, got integer");
                    }

                    let ptr = ptr_word.to_pointer();
                    if ptr + offset >= self.heap.heap.len() {
                        panic!("Load: Offset out of bounds");
                    }
                    let value = self.heap.heap[ptr + offset];
                    self.push(value);
                }
                /*
                Opcode::Load => {
                    let offset = self.read_u32() as usize;
                    let ptr_word = self.pop();

                    println!("Load: ptr_word on stack before pop: {:?}", ptr_word);
                    println!("Load: ptr_word.to_int() before check: {}", ptr_word.to_int());
                    println!("Load: ptr_word.is_pointer() before check: {}", ptr_word.is_pointer()); 

                    if !ptr_word.is_pointer() { 
                        panic!("Load: Expected pointer, got integer");
                    }

                    let ptr = ptr_word.to_pointer();
                    if ptr + offset >= self.heap.heap.len() {
                        panic!("Load: Offset out of bounds");
                    }
                    let value = self.heap.heap[ptr + offset];
                    self.push(value);
                } */

                // --- System Operations ---
                Opcode::Clock => {
                    let elapsed = start_time.elapsed();
                    println!("{:.4}", elapsed.as_secs_f64());
                }
            } 
        }
    }
}

                    //if !ptr_word.is_pointer() { //edw einai private. wraia. ti kanoume.
                    
                    //if !ptr_word.is_ptr() { //ptr_word == ptr_word.to_pointer() { // an to to_pointer den douleuei otan
                                                          // exeis eisodo pointers thewritika auto
                                                          // einai komple
                    
                    

                    //if (ptr_word.to_int() & 1) == 1 {  //bitwise AND
                    //    panic!("Load: Expected pointer, got integer");
                    //}
                    
                    //// let ptr_word2 = Word::from_pointer(ptr_word.to_pointer()); 
                                                                               
                    /*
                    if ptr_word != ptr_word2 {
                        panic!("Load expected pointer not integer");
                    } */

                    // if ptr_word == ptr_word2 {
                    //    let c  = 1; //apla uparxei
                    //}
                    

                    //else {
                    //    panic!("Load expected pointer not integer"); 
                    //}

                    //if (ptr_word.w & 1) != 0 {
                        //panic!("Load: Expected pointer, got integer");
                    //}
