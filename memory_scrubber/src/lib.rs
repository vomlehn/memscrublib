// Main memory scrubber function callable from C
type ecc_data = u64;

#[repr(C)]
pub struct CMemoryScrubber {
    start:  *mut ecc_data,
    size:   usize,
    p:      *mut ecc_data,
}

pub struct MemoryScrubber {
    c_memory_scrubber: CMemoryScrubber,
}


#[no_mangle]
pub extern "C" memory_scrubber_new(start: *mut ecc_data, size: usize) ->
    CMemoryScrubber {
    MemoryScrubber::new(start, size)
        .c_memory_scrubber;
}

#[no_mangle]
pub extern "C" memory_scrubber_scrub(self: CMemoryScrubber, usize: n) {
    MemoryScrubber {
        c_memory_scrubber: self,
    }.scrub(n);
}

impl MemoryScrubber {
    pub fn new(start: *mut ecc_data, size: usize) -> MemoryScrubber {
        MemoryScrubber {
            start: start,
            size: size,
            p: start,
        }
    }

    pub fn scrub(self: &mut MemoryScrubber) {
    }
}
