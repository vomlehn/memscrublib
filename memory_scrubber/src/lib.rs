// Main memory scrubber function callable from C

use std::error::Error;
use std::fmt;

// Number of bits in an address field:
// CACHELINE_INDEX - Number of bits required to address all bytes in
//      a cache line.
// CACHELINE_SIZE - Number of bytes in a cacheline
// CACHE_INDEX - Number of bits required to address all cache lines in the
//      cache
const CACHELINE_INDEX: usize = 6;
const CACHELINE_SIZE: usize = 1 << CACHELINE_INDEX;
const CACHE_INDEX: usize = 10;

// Data type on which the ECC operations. So, if it operates on a 64-but
// value, this should be u64
type ECCData = u64;

const ECCDATA_PER_CACHELINE: usize =
    CACHELINE_SIZE as usize / std::mem::size_of::<ECCData>();
type Cacheline = [ECCData; ECCDATA_PER_CACHELINE];

/*
#[repr(C)]
pub struct CMemoryScrubberResult {
    result: MemoryScrubberResults,
    memory_scrubber: CMemoryScrubber,
}

#[repr(C)]
pub struct CMemoryScrubber {
    start:  *const ECCData,
    size:   usize,
    offset: usize,
}

#[no_mangle]
pub extern "C" fn memory_scrubber_new(start: *const ECCData, size: usize) ->
    CMemoryScrubber {
    let memory_scrubber = MemoryScrubber::new(start, size);
    match memory_scrubber {
        MemoryScrubber::new(start, size)
            .c_memory_scrubber
    }
}

#[no_mangle]
pub extern "C" fn memory_scrubber_scrub(memory_scrubber: CMemoryScrubber,
    n: usize) -> MemoryScrubberResults {
    MemoryScrubber {
        c_memory_scrubber: memory_scrubber,
    }.scrub(n);
}
*/

#[derive(Debug)]
pub enum MemoryScrubberError {
    UnalignedAddress,
    UnalignedSize,
}

impl Error for MemoryScrubberError {
}

impl fmt::Display for MemoryScrubberError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// Structure to use for memory scrubbing
// start: Beginning address of memory, which is aligned on a cache line
//      boundary
// size: Number of cache lines in the area to be scrubbed
// offset: Current cache line being scrubbed
pub struct MemoryScrubber {
    start:  *const Cacheline,
    size:   isize,
    offset: isize,
}

impl MemoryScrubber {
    // Create a new MemoryScrubber.
    // start: pointer to the beginning of the memory area to be scrubbed. Must
    //      be a multiple of the cache line size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache line size.
    pub fn new(start: *const u8, size: usize) ->
        Result<MemoryScrubber, MemoryScrubberError> {
        let start_addr = start as usize;
        if (start_addr % CACHELINE_SIZE) != 0 {
            return Err(MemoryScrubberError::UnalignedAddress);
        }
        if (size % CACHELINE_SIZE) != 0 {
            return Err(MemoryScrubberError::UnalignedSize);
        }
        Ok(MemoryScrubber {
            start: start as *const Cacheline,
            size: (size / CACHELINE_SIZE) as isize,
            offset: 0,
        })
    }

    pub fn scrub(&self, n: usize) -> Result<(), MemoryScrubberError> {
        for i in 0..n {
            let offset = self.offset;

            unsafe {
                let p = self.start.offset(offset);
                // Need to ensure this is not optimized away
                let _dummy = *p.offset(offset);
            }
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use std::mem;
    use super::*;

    #[test]
    fn it_works() {
        const buf: [u32; 5] = [0, 1, 2, 3, 4];
        let size = mem::size_of_val(&buf);
        let memory_scrubber =
            match MemoryScrubber::new(buf.as_ptr() as *const u8, size) {
            Err(e) => panic!("Unable to create MemoryScrubber: {}", e),
            Ok(memory_scrubber) => memory_scrubber,
        };

        match memory_scrubber.scrub(CACHELINE_SIZE * 10) {
            Err(e) => panic!("Scrub failed: {}", e),
            Ok(_) => println!("Scrub succeeded!"),
        };
    }
}
