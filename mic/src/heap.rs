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

    fn is_pointer(self) -> bool {
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
    pub from_space: Box<[Word]>,
    pub to_space: Box<[Word]>,
    pub alloc_ptr: usize,
    pub scan_ptr: usize,
    pub using_from_space: bool,
}

impl Heap {
    pub fn new(total_words: usize) -> Self {
        let vec1 = vec![Word { w: 0 }; total_words / 2];
        let vec2 = vec![Word { w: 0 }; total_words / 2];

        Heap {
            from_space: vec1.into_boxed_slice(),
            to_space: vec2.into_boxed_slice(),
            alloc_ptr: 0,
            scan_ptr: 0,
            using_from_space: true,
        }
    }

    // allocate a new block
    // Allocate a new block of memory
    pub fn alloc(&mut self, size: usize, tag: u8, words: &[Word]) -> Option<usize> {
        let mut heap = if self.using_from_space {
            &mut self.from_space
        } else {
            &mut self.to_space
        };
    
        let start = self.alloc_ptr;
    
        // Ensure we have space before allocating
        if start + size + 1 > heap.len() {
            self.garbage_collect();
    
            // Update heap reference in case of space flip
            heap = if self.using_from_space {
                &mut self.from_space
            } else {
                &mut self.to_space
            };
    
            if self.alloc_ptr + size + 1 > heap.len() {
                return None; // No space even after GC
            }
        }
    
        let new_start = self.alloc_ptr;
        let header = ((size as i32) << 9) | ((tag as i32) << 1) | 1;
        heap[new_start] = Word { w: header };
    
        for i in 0..size {
            heap[new_start + 1 + i] = if i < words.len() {
                words[i]
            } else {
                Word::from_int(0)
            };
        }
    
        self.alloc_ptr += size + 1;
        Some(new_start)
    }    

    pub fn garbage_collect(&mut self) {
        println!("Running garbage collection...");
    
        // Swap from-space and to-space
        let (from_space_ptr, to_space_ptr) = if self.using_from_space {
            (&mut self.from_space as *mut Box<[Word]>, &mut self.to_space as *mut Box<[Word]>)
        } else {
            (&mut self.to_space as *mut Box<[Word]>, &mut self.from_space as *mut Box<[Word]>)
        };
    
        // SAFELY obtain mutable references to spaces (avoiding Rust borrow conflicts)
        let from_space = unsafe { &mut *from_space_ptr };
        let to_space = unsafe { &mut *to_space_ptr };
    
        self.alloc_ptr = 0;
        self.scan_ptr = 0;
    
        // Step 1: Collect root pointers (Assuming roots are stored in VM stack)
        let roots: Vec<usize> = from_space
            .iter()
            .filter(|word| word.is_pointer())
            .map(|word| word.to_pointer())
            .collect();
    
        // Step 2: Map old addresses to new addresses
        let mut new_addresses = vec![None; from_space.len()];
    
        // Step 3: Copy root objects first
        for &root in &roots {
            let new_addr = self.copy(root, from_space, to_space);
            new_addresses[root] = Some(new_addr);
        }
    
        // Step 4: Scan the to-space and relocate internal pointers
        while self.scan_ptr < self.alloc_ptr {
            let header = to_space[self.scan_ptr].w;
            let size = (header >> 9) as usize;
    
            for i in 1..=size {
                let old_addr = to_space[self.scan_ptr + i].to_pointer();
                if to_space[self.scan_ptr + i].is_pointer() {
                    if let Some(new_addr) = new_addresses[old_addr] {
                        // Update the pointer to the new address
                        to_space[self.scan_ptr + i] = Word::from_pointer(new_addr);
                    } else {
                        // Copy the object and update the pointer
                        let new_addr = self.copy(old_addr, from_space, to_space);
                        new_addresses[old_addr] = Some(new_addr);
                        to_space[self.scan_ptr + i] = Word::from_pointer(new_addr);
                    }
                }
            }
    
            self.scan_ptr += size + 1;
        }
    
        // Step 5: Flip spaces
        self.using_from_space = !self.using_from_space;
    }
    
    

    fn copy(&mut self, old_ptr: usize, from_heap: &mut [Word], to_heap: &mut [Word]) -> usize {
        if from_heap[old_ptr].is_pointer() {
            return from_heap[old_ptr].to_pointer(); // Already forwarded
        }
    
        let new_ptr = self.alloc_ptr;
        self.alloc_ptr += 1;
    
        let header = from_heap[old_ptr].w;
        let size = (header >> 9) as usize;
        to_heap[new_ptr] = Word { w: header };
    
        for i in 0..size {
            to_heap[new_ptr + 1 + i] = from_heap[old_ptr + 1 + i];
        }
    
        // Store forwarding pointer (to mark that it's already copied)
        from_heap[old_ptr] = Word::from_pointer(new_ptr); 
    
        new_ptr
    }
    

    pub fn load(&self, pointer: usize, offset: usize) -> Option<Word> {

        let heap = if self.using_from_space {
            &self.from_space
        } else {
            &self.to_space
        };

        if pointer >= heap.len() {
            return None; // Invalid pointer
        }
    
        let index = pointer + offset; // Offset starts from the first field
        if index >= heap.len() {
            return None; // Prevent out-of-bounds access
        }
    
        Some(heap[index])
    }
    

    pub fn debug_print(&self) {
        println!("--- Heap State ---");
        let heap = if self.using_from_space {
            &self.from_space
        } else {
            &self.to_space
        };

        for (i, word) in heap.iter().enumerate() {
            if word.w != 0 {
                println!("Heap[{}]: {:?}", i, word);
            }
        }
        println!("------------------");
    }

}
