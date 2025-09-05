use std::fmt::{self, Debug};

#[derive(Clone, Copy)]
pub struct Word {
    w: i32,
}

impl Word {
    pub fn from_pointer(ptr: usize) -> Word {
        Word {
            w: (ptr as i32) << 1 | 0,
        }
    }

    pub fn from_int(int: i32) -> Word {
        Word { w: int << 1 | 1 }
    }

    pub fn to_pointer(self) -> usize {
        (self.w >> 1) as usize
    }

    pub fn to_int(self) -> i32 {
        self.w >> 1
    }

    pub fn is_pointer(self) -> bool { //pub
        (self.w & 1) == 0 
    }
}

impl Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_pointer() {
            write!(f, "Ptr({})", self.to_pointer())
        } else {
            write!(f, "Int({})", self.to_int())
        }
    }
}

pub struct Heap {
    pub heap: Box<[Word]>,
    alloc_ptr: usize,



    from_space_start: usize,
    from_space_end: usize,
    to_space_start: usize,
    to_space_end: usize,

    from_space_alloc_ptr: usize,
    to_space_alloc_ptr: usize,
    scan_ptr: usize,

    is_from_space_active: bool,
}

impl Heap {
    pub fn new(total_words: usize) -> Self {
        let vec = vec![Word { w: 0 }; total_words];
        let memory = vec.into_boxed_slice();
        let half_heap_size = total_words / 2;
       
        Heap {
            heap: memory,
            alloc_ptr: 0,

            //gc
            from_space_start: 0,
            from_space_end: half_heap_size,
            to_space_start: half_heap_size,
            to_space_end: total_words,
            from_space_alloc_ptr: 0, // Start allocating in from-space
            to_space_alloc_ptr: half_heap_size, // Initialize for GC
            scan_ptr: half_heap_size,         // Initialize for GC
            is_from_space_active: true,
        }
    }


    /// gia pointer forwarding, to kalw stin alloc
    pub fn forward(&self, ptr: usize) -> usize {
        if ptr >= self.to_space_start && ptr < self.to_space_end {
            return ptr;
        }

        let forwarding_word = self.heap[ptr];
        if forwarding_word.is_pointer() {
            let forwarded_ptr = forwarding_word.to_pointer();
            if forwarded_ptr >= self.to_space_start && forwarded_ptr < self.to_space_end {
                return forwarded_ptr;
            }
        }

        panic!("Invalid pointer during forwarding: {}", ptr);
    }
    // allocate a new block
    /*pub fn alloc(&mut self, _size: usize, _tag: u8, _words: &[Word]) -> Option<usize> {
        let total_needed = 1 + _size; // gia conflicts
        if self.alloc_ptr + total_needed > self.heap.len() {
            //println
            //self.gc
            return None; // Not enough space in the heap.
        }

        // Build the header word.
        let header_value: i32 = ((_size as i32) << 9) | ((_tag as i32) << 1) | 1;
        self.heap[self.alloc_ptr] = Word { w: header_value };

        // Write the field words immediately after the header.
        for i in 0.._size {
            self.heap[self.alloc_ptr + 1 + i] = _words[i];
        }

        // Save the pointer (index) where this block starts.
        let ptr = self.alloc_ptr;
        // Update the allocation pointer.
        self.alloc_ptr += total_needed;
        Some(ptr)
    }*/
    pub fn alloc(&mut self, size: usize, tag: u8, words: &[Word]) -> Option<usize> {
        let total_needed = 1 + size;
        let current_alloc_ptr = if self.is_from_space_active {
            self.from_space_alloc_ptr
        } else {
            self.to_space_alloc_ptr
        };

        if current_alloc_ptr + total_needed > self.from_space_end {
            return None;
        }

        // Build header 
        let header_value = ((size as i32) << 9) | ((tag as i32) << 1);
        self.heap[current_alloc_ptr] = Word { w: header_value };

        // Copy fields
        for i in 0..size {
            self.heap[current_alloc_ptr + 1 + i] = words[i];
        }

        let ptr = current_alloc_ptr;
        
        // Update alloc pointer
        if self.is_from_space_active {
            self.from_space_alloc_ptr += total_needed;
        } else {
            self.to_space_alloc_ptr += total_needed;
        }

        Some(ptr)
    }

    // edw k katw
    // garbage collector:::
    pub fn gc(&mut self, stack: &[Word]) {
        //println!("--- Garbage Collection Started ---"); // debug

        self.to_space_alloc_ptr = self.to_space_start; // Reset to-space allocation pointer
        self.scan_ptr = self.to_space_start;         // Reset to-space scan pointer

        //println!("GC: Scanning Root Set (Stack)"); // debug 
        for word_from_stack in stack.iter() {
            if word_from_stack.is_pointer() { 
                let pointer = word_from_stack.to_pointer(); 
                //println!("GC: Found pointer in root set: Ptr({})", pointer); // debug
                self.copy_object_if_needed(pointer); 
            }
        }
        //println!("GC: Root Set Scanning Complete"); // debug

        // 3. To-Space Scanning:
        //println!("GC: Scanning To-Space"); //debug
        while self.scan_ptr < self.to_space_alloc_ptr {
            let current_object_ptr = self.scan_ptr;
            //println!("GC: Scanning object in to-space at: {}", current_object_ptr); // debug 

            let header_word = self.heap[current_object_ptr];
            let object_size = (header_word.to_int() >> 9) as usize; // Extract size from header
            //println!("GC: Object size: {}", object_size); // idebug
            for i in 0..object_size {
                let field_ptr = current_object_ptr + 1 + i; // Calculate field address
                let field_word = self.heap[field_ptr];
                if field_word.is_pointer() { // Check if field is a pointer
                    let pointer = field_word.to_pointer();
                    //println!("GC: Found pointer in field at offset {}: Ptr({})", i, pointer); // debug
                    self.copy_object_if_needed(pointer); // Copy referenced object if needed
                }
            }

            self.scan_ptr += 1 + object_size; 
        }
        //println!("GC: To-Space Scanning Complete"); //debug
        //println!("GC: Swapping Spaces"); // debug
        self.is_from_space_active = !self.is_from_space_active; 
        std::mem::swap(&mut self.from_space_start, &mut self.to_space_start);
        std::mem::swap(&mut self.from_space_end, &mut self.to_space_end);
        self.from_space_alloc_ptr = 0; // allocating from the beginning of new from-space (apo to
                                       // from-to space apo ton algorithmo

        //println!("--- Garbage Collection Finished ---"); // Debug print


    }

    fn copy_object_if_needed(&mut self, from_space_ptr: usize) -> usize {
        //println!("GC: copy_object_if_needed called for pointer: {}", from_space_ptr);
        if from_space_ptr < self.from_space_start || from_space_ptr >= self.from_space_end {
            //println!("GC: Pointer Ptr({}) is NOT in from-space, ignoring.", from_space_ptr); 
            return from_space_ptr;
        }
        //println!("GC: Pointer Ptr({}) is in from-space.", from_space_ptr); // debug
                                                                           //

        // 2. Check for Forwarding Pointer:
        let possible_forwarding_ptr_word = self.heap[from_space_ptr];
        if possible_forwarding_ptr_word.is_pointer() && possible_forwarding_ptr_word.to_pointer() >= self.to_space_start && possible_forwarding_ptr_word.to_pointer() < self.to_space_end {
            let forwarded_address = possible_forwarding_ptr_word.to_pointer();
            //println!("GC: Found forwarding pointer at Ptr({}), forwarded address: Ptr({})", from_space_ptr, forwarded_address); 
            return forwarded_address; // Return forwarded address
        }

        //println!("GC: No forwarding pointer found at Ptr({}), proceeding with copy.", from_space_ptr); 

        // 3. Copy Object:
        let header_word = self.heap[from_space_ptr];
        let object_size = (header_word.to_int() >> 9) as usize;
        let _object_end_ptr = from_space_ptr + 1 + object_size;

        let new_address_in_to_space = self.to_space_alloc_ptr;
        //println!("GC: Copying object from Ptr({}) to Ptr({}) in to-space, size: {}", from_space_ptr, new_address_in_to_space, 1 + object_size); //debug

        for i in 0..(1 + object_size) {
            self.heap[new_address_in_to_space + i] = self.heap[from_space_ptr + i];
        }

        let forwarding_ptr_word = Word::from_pointer(new_address_in_to_space); // Create forwarding pointer word
        self.heap[from_space_ptr] = forwarding_ptr_word; // Write forwarding pointer back to from-space

        self.to_space_alloc_ptr += 1 + object_size;

        //println!("GC: Object copied and forwarding pointer set at Ptr({}), new address: Ptr({})", from_space_ptr, new_address_in_to_space); // debug
        new_address_in_to_space // 
    }
    
}
