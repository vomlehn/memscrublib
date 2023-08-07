// Main memory scrubber function callable from C

use thiserror::Error;

// ***************
// Architecture-dependent definitions
//
// The following definitions are for the cache with the largest cache line.
//
// CACHELINE_WIDTH - Number of bits required to address all bytes in
//      a cache line.
// CACHELINE_SIZE - Number of bytes in a cacheline
// CACHE_WIDTH - Number of bits required to address all cache lines in the
//      cache
// CACHE_SIZE - Number of cache lines in the cache

const CACHELINE_WIDTH: usize = 6;
const CACHELINE_SIZE: usize = 1 << CACHELINE_WIDTH;
const CACHE_WIDTH: usize = 10;
const CACHE_SIZE: usize = 1 << CACHE_WIDTH;

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// Data type on which the ECC operations. So, if it operates on a 64-but
// value, this should be u64
type ECCData = u64;
//------------------------------------------
// This mask divides the lower cacheline index and cache index information
// from the upper bits of an address.
const addr_mask: usize = !((1 << (CACHELINE_WIDTH + CACHE_WIDTH)) - 1);

const ECCDATA_PER_CACHELINE: usize =
    CACHELINE_SIZE as usize / std::mem::size_of::<ECCData>();
type Cacheline = [ECCData; ECCDATA_PER_CACHELINE];


// ***************

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

#[derive(Clone, Copy, Debug, Error, PartialEq)]
pub enum MemoryScrubberError {
    #[error("Address must be aligned on cache line boundary")]
    UnalignedAddress,

    #[error("Size must be a multiple of cache line size")]
    UnalignedSize,

    #[error("Size may not be zero")]
    ZeroSize,
}

struct MemoryScrubber {
    iterator:   MemoryScrubberIterator,
}

impl MemoryScrubber {

    pub fn new(start: *const u8, size: usize) ->
        Result<MemoryScrubber, MemoryScrubberError> {
        match MemoryScrubberIterator::new(start, size) {
            Err(e) => Err(e),
            Ok(iterator) => Ok(MemoryScrubber {
                iterator:   iterator,
            })
        }
    }

    pub fn scrub(&mut self, n: usize) -> Result<(), MemoryScrubberError> {
        Ok(())
    }
}

/*
    pub fn scrub(&self, n: usize) -> Result<(), MemoryScrubberError> {
println!("There are {} cache lines", CACHE_SIZE);
println!("start {:x}", self.start as Addr);
println!("shift is {}", CACHELINE_WIDTH + CACHE_WIDTH);
        // Get a mask that will allow extracting the address bits higher
        // than the cache index
        let addr_mask = !((1 << (CACHELINE_WIDTH + CACHE_WIDTH)) - 1);

        // Get the highest address less that the starting address that
        // corresponds to the zero index in the cache.
        let start_addr = self.start as Addr & addr_mask;

        // Pull out the cache index corresponding the the starting address
        let start_offset = (self.start as Addr & !addr_mask) >> CACHELINE_WIDTH;
        println!("mask {:x} masked {:x} start_offset {} 0x{:x}",
                 addr_mask,
                 self.start as Addr & addr_mask,
                 start_offset, start_offset);

        // Scan through each cache line
        for cache_line in 0..CACHE_SIZE {
            if cache_line == 2 {
                break;
            }
            let base_offset = cache_line + start_offset;

            // If the first address would be past the end of the memory area,
            // we're done.
            if base_offset as isize > self.size {
                break;
            }

            let base_addr = start_addr + (base_offset << CACHELINE_WIDTH);
            println!("base_addr {:x}", base_addr);
        }
        Ok(())
    }
}
*/

// Iterator to scan a region of memory, keeping on a single cache line as
// long as possible.
//
// start:   Beginning address of memory, which is aligned on a cache line
//          boundary
// size:    Number of cache lines in the area to be scrubbed
// index:   Value that, when added to the cache index value of start, yields
//          the index of the cache line being scrubbed
// offset:  Number of cache lines between the first address corresponding to
//          the given cache index and the address that will be read. This is
//          a multiple of the number cache lines in the cache.
pub struct MemoryScrubberIterator {
    start:  *const Cacheline,
    size:   isize,
    index:  isize,
    offset: isize,
}

impl MemoryScrubberIterator {
    // Create a new MemoryScrubber.
    // start: pointer to the beginning of the memory area to be scrubbed. Must
    //      be a multiple of the cache line size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache line size.
    pub fn new(start: *const u8, size: usize) ->
        Result<MemoryScrubberIterator, MemoryScrubberError> {
        let start_addr = start as Addr;
        if (start_addr % CACHELINE_SIZE) != 0 {
            return Err(MemoryScrubberError::UnalignedAddress);
        }

        if size == 0 {
            return Err(MemoryScrubberError::ZeroSize);
        }

        if (size % CACHELINE_SIZE) != 0 {
            return Err(MemoryScrubberError::UnalignedSize);
        }

        Ok(MemoryScrubberIterator {
            start: start as *const Cacheline,
            size: (size / CACHELINE_SIZE) as isize,
            index: 0,
            offset: 0,
        })
    }

    // Return the cache index for the given address
    fn cache_index(addr: *const Cacheline) -> isize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !addr_mask) >> CACHELINE_WIDTH) as isize
    }
}

impl Iterator for MemoryScrubberIterator {
    type Item = *const Cacheline;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If we've scanned all cache lines, we're finished.
            if self.index > self.size {
                return None;
            }

            // We need to get the offset, in cache lines, of the address that
            // we are scrubbing. First we sum:
            //
            // o    The offset into the cache of the starting address
            // o    The offset into the cache of the set of cache lines we
            //      are scrubbing
            //
            // This, modulo the cache size, is the cache index for the addresses
            // in a pass through that cache index.
            //
            // To this is added the number of cache lines in the cache. This is
            // the offset, in cache lines, from the starting address.
            let base_offset = MemoryScrubberIterator::cache_index(self.start) +
                self.index as isize + self.offset;

            if base_offset < self.size {
                self.offset += CACHE_SIZE as isize;
                let res = unsafe {
                    self.start.offset(base_offset)
                };
                return Some(res);
            }
            self.index += 1;
            self.offset = 0;
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aligned() {
        let size = CACHELINE_SIZE * CACHE_SIZE * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);

        let mut memory_scrubber = match MemoryScrubber::new(p, size) {
            Err(e) => panic!("Unable to create MemoryScrubberIterator: {}", e),
            Ok(memory_scrubber) => memory_scrubber,
        };

        match memory_scrubber.scrub(CACHELINE_SIZE * 10) {
            Err(e) => panic!("Scrub failed: {}", e),
            Ok(_) => println!("Scrub succeeded!"),
        };
    }

    #[test]
    fn test_unaligned_address() {
        let size = CACHELINE_SIZE * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);
        let p = unsafe {
            p.offset(1)
        };

        let memory_scrubber = MemoryScrubberIterator::new(p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedAddress);
    }

    #[test]
    fn test_unaligned_size() {
        let size = CACHELINE_SIZE * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);
        let size = size - 1;

        let memory_scrubber = MemoryScrubberIterator::new(p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedSize);
    }

    #[test]
    fn test_zero_size() {
        let size = CACHELINE_SIZE * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);
        let size = 0;

        let memory_scrubber = MemoryScrubberIterator::new(p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::ZeroSize);
    }

    fn align_area(p: *const u8, size: usize) -> (*const u8, usize) {
        let lower_p = (p as Addr) % CACHELINE_SIZE;
        let p_offset = CACHELINE_SIZE - lower_p;
        let p = unsafe { p.offset(p_offset as isize) };

        let size = size - p_offset;
        let size = size - (size % CACHELINE_SIZE);
        (p, size)
    }
}
