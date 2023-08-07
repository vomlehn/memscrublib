// Main memory scrubber function callable from C

use thiserror::Error;

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// CacheDesc - Description of the cache
// cacheline_width - Number of bits in the index into the cachline bytes
// cache_width - Number of bits of the cache index.
#[derive(Clone, Copy, Debug)]
pub struct CacheDesc {
    cacheline_width:    usize,
    cache_width:        usize,
}

impl CacheDesc {
    pub fn cacheline_size(&self) -> usize {
        1 << self.cacheline_width
    }

    pub fn cache_size(&self) -> usize {
        1 << self.cache_width
    }
}

//------------------------------------------

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

struct MemoryScrubber<Cacheline> {
    cache_desc: CacheDesc,
    start:      *const u8,
    size:       usize,
    iterator:   MemoryScrubberIterator<Cacheline>,
}

impl<Cacheline> MemoryScrubber<Cacheline> {

    pub fn new(cache_desc: &CacheDesc, start: *const u8,
        size: usize) -> Result<MemoryScrubber<Cacheline>, MemoryScrubberError> {
        let iterator = MemoryScrubberIterator::new(&cache_desc, start, size)?;
        Ok(MemoryScrubber {
            cache_desc: (*cache_desc).clone(),
            start:      start,
            size:       size,
            iterator:   iterator,
        })
    }

    pub fn scrub(&mut self, n: usize) -> Result<(), MemoryScrubberError> {
        if (n % self.cache_desc.cacheline_size()) != 0 {
            return Err(MemoryScrubberError::UnalignedSize);
        }

        for _i in 0..n {
            let _p = match self.iterator.next() {
                None => {
                    self.iterator =
                        match MemoryScrubberIterator::new(&self.cache_desc,
                        self.start, self.size) {
                        Err(e) => return Err(e),
                        Ok(iterator) => iterator,
                    };
                    self.iterator.next().unwrap()
                },
                Some(p) => p,
            };
        }
        
        Ok(())
    }
}

// Iterator to scan a region of memory, keeping on a single cache line as
// long as possible.
//
// start:       Beginning address of memory, which is aligned on a cache line
//              boundary
// size:        Number of cache lines in the area to be scrubbed
// index:       Value that, when added to the cache index value of start, yields
//              the index of the cache line being scrubbed
// offset:      Number of cache lines between the first address corresponding to
//              the given cache index and the address that will be read. This is
//              a multiple of the number cache lines in the cache.
// addr_mask:   This mask divides the lower cacheline index and cache index
//              information from the upper bits of an address.
pub struct MemoryScrubberIterator<Cacheline> {
    cache_desc: CacheDesc,
    start:      *const Cacheline,
    size:       isize,
    index:      isize,
    offset:     isize,
    addr_mask:  usize,
}

impl<Cacheline> MemoryScrubberIterator<Cacheline> {

    // Create a new MemoryScrubber.
    // start: pointer to the beginning of the memory area to be scrubbed. Must
    //      be a multiple of the cache line size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache line size.
    pub fn new(cache_desc: &CacheDesc, start: *const u8, size: usize) ->
        Result<MemoryScrubberIterator<Cacheline>, MemoryScrubberError> {
        let start_addr = start as Addr;
        if (start_addr % cache_desc.cacheline_size()) != 0 {
            return Err(MemoryScrubberError::UnalignedAddress);
        }

        if size == 0 {
            return Err(MemoryScrubberError::ZeroSize);
        }

        if (size % cache_desc.cacheline_size()) != 0 {
            return Err(MemoryScrubberError::UnalignedSize);
        }

        Ok(MemoryScrubberIterator {
            cache_desc: (*cache_desc).clone(),
            start:      start as *const Cacheline,
            size:       (size / cache_desc.cacheline_size()) as isize,
            index:      0,
            offset:     0,
            addr_mask:  !((1 << (cache_desc.cacheline_width +
                             cache_desc.cache_width)) - 1),
        })
    }

    // Return the cache index for the given address
    fn cache_index(&self, addr: *const Cacheline) -> isize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !self.addr_mask) >>
            self.cache_desc.cacheline_width) as isize
    }
}

impl<Cacheline> Iterator for MemoryScrubberIterator<Cacheline> {
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
            let base_offset = self.cache_index(self.start) +
                self.index as isize + self.offset;

            if base_offset < self.size {
                self.offset += self.cache_desc.cache_size() as isize;
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

    // Cache characteristics
    // CACHELINE_WIDTH - number of bits required to index a byte in a cache line
    // CACHE_WIDTH - number of bits used as a cache line index in the cache
    const CACHELINE_WIDTH: usize = 6;
    const CACHE_WIDTH: usize = 10;

    // Cache descriptor to pass to the memory scrubbing functions.
    const CACHE_DESC: CacheDesc = CacheDesc {
        cacheline_width:    CACHELINE_WIDTH,
        cache_width:        CACHE_WIDTH,
    };

    // ECCData - The data size used to compute the ECC
    // Cacheline - the data type of a cache line
    type ECCData = u64;
    type Cacheline = [u64; (1 << CACHELINE_WIDTH) / std::mem::size_of::<ECCData>()];


    #[test]
    fn test_aligned() {
        let size = CACHE_DESC.cacheline_size() * CACHE_DESC.cache_size() * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);

        let mut memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(&CACHE_DESC, p, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        match memory_scrubber.scrub(CACHE_DESC.cacheline_size() * 10) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };
    }

    #[test]
    fn test_unaligned_address() {
        let size = CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);
        let p = unsafe {
            p.offset(1)
        };

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedAddress);
    }

    #[test]
    fn test_unaligned_size() {
        let size = CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(p, size);
        let size = size - 1;

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedSize);
    }

    #[test]
    fn test_zero_size() {
        let size = CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, _size) = align_area(p, size);
        let size = 0;

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::ZeroSize);
    }

    fn align_area(p: *const u8, size: usize) -> (*const u8, usize) {
        let lower_p = (p as Addr) % CACHE_DESC.cacheline_size();
        let p_offset = CACHE_DESC.cacheline_size() - lower_p;
        let p = unsafe { p.offset(p_offset as isize) };

        let size = size - p_offset;
        let size = size - (size % CACHE_DESC.cacheline_size());
        (p, size)
    }
}
