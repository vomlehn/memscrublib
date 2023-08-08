// Main memory scrubber function callable from C
//
// When computers are used in environments that can cause a large number of
// memory bits to assume incorrect values, error correction code (ECC)
// hardware allows bad bits to be detected and corrected. This works well for
// memory which is frequently used because the checking and correction is done
// for each write. Memory which is rarely used, however, may accumulate an
// ever increasing number of errors. ECC is only capable of correcting a limited
// number of bits. If memory has more errors than ECC can correct, a hard
// fault will occur.
//
// The definitions in this file are intended to counter this problem. It should
// be used to scan all of memory on a periodic basis. If this is done
// frequently enough, the latent errors that have built up will be corrected
// before exceeding the number than can be corrected. The process of
// periodically scanning all of memory is referred to as memory scrubbing.
//
// Although memory scrubbing is a relatively fast operation, it still take
// more time than is available as a single block on a given system. Because
// of this, the scrubbing operation is given the number of bytes that should
// be scrubbed. It will start from where it left off and scan the specified
// number of bytes.
//
// The number of bytes to be scrubbed at a time is a system-dependent
// parameter, but the overall rate at which memory should be scrubbed depends
// on the likelihood of a given bit becoming bad and the number of bits that
// can be corrected by the ECC.

use thiserror::Error;

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// Describe the cache sizes and pull in all elements of the cache line.
pub trait MemoryScrubberCacheDesc<Cacheline> {
    // NOTE: You are unlikely to ever need to implement this
    // Return the number of bits required to hold an index into the bytes of
    // a cacheline. So, if you have an eight-byte cache line (unlikely), this
    // would return 3.
    fn cacheline_width(&self) -> usize {
        usize::BITS as usize - 1 - std::mem::size_of::<Cacheline>()
            .leading_zeros() as usize
    }

    // NOTE: You are unlikely to ever need to implement this
    // Return the number of bytes in the cache line. A cache line width of 4
    // bits will have a cache line size of 16 bytes.
    fn cacheline_size(&self) -> usize {
        1 << self.cacheline_width()
    }

    // Return the number of bits used to index into the cache, i.e. the index
    // of a cache line in the cache. A cache with 1024 lines will have an
    // index using 10 bits.
    fn cache_width(&self) -> usize;

    // NOTE: You are unlikely to ever need to implement this
    // Return the number of cache lines in the index. For a 1024 line cache
    // and a 16 byte cache line, this will be 64.
    fn cache_size(&self) -> usize {
        1 << self.cache_width()
    }

    // This function is given a pointer to a cache line-aligned address with
    // as many bytes as are in a cache line. The implementation should do
    // whatever is necessary to ensure all bytes are read in order to trigger
    // a fault if any bits have an unexpected value. So long as the number
    // of bad bits is small enough (hardware-dependent), corrected data should
    // be written back to that location, preventing the accumulation of so many
    // bad bits that the correct value cannot be determined.
    fn read_cacheline(&self, p: *const Cacheline);
}

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

    #[error("std::mem::size_of::<Cacheline> should equal CacheDesc.cacheline_size()")]
    CachelineSizeMismatch,
}

struct MemoryScrubber<'a, Cacheline> {
    cache_desc: &'a dyn MemoryScrubberCacheDesc<Cacheline>,
    start:      *const u8,
    size:       usize,
    iterator:   MemoryScrubberIterator<'a, Cacheline>,
}

impl<'a, Cacheline> MemoryScrubber<'a, Cacheline> {

    pub fn new(cache_desc: &'a dyn MemoryScrubberCacheDesc<Cacheline>, start: *const u8,
        size: usize) -> Result<MemoryScrubber<Cacheline>, MemoryScrubberError> {
        let iterator = MemoryScrubberIterator::new(cache_desc, start, size)?;
        Ok(MemoryScrubber {
            cache_desc: cache_desc,
            start:      start,
            size:       size,
            iterator:   iterator,
        })
    }

    // Scrub
    pub fn scrub(&mut self, n: usize) -> Result<(), MemoryScrubberError> {
        if (n % self.cache_desc.cacheline_size()) != 0 {
            return Err(MemoryScrubberError::UnalignedSize);
        }

println!("cache at {:x}", self.start as usize);
        // Convert to the number of cachelines to scrub
        let n = n / self.cache_desc.cacheline_size();
println!("Scrubbing {} cache lines", n);

        for _i in 0..n {
            // If we don't already have an iterator, get a new one.
            let p = match self.iterator.next() {
                None => {
                    self.iterator =
                        match MemoryScrubberIterator::new(self.cache_desc,
                        self.start, self.size) {
                        Err(e) => return Err(e),
                        Ok(iterator) => iterator,
                    };
                    self.iterator.next().unwrap()
                },
                Some(p) => p,
            };

            self.cache_desc.read_cacheline(p);
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
pub struct MemoryScrubberIterator<'a, Cacheline> {
    cache_desc: &'a dyn MemoryScrubberCacheDesc<Cacheline>,
    start:      *const Cacheline,
    size:       isize,
    index:      isize,
    offset:     isize,
    addr_mask:  usize,
}

impl<'a, Cacheline> MemoryScrubberIterator<'a, Cacheline> {

    // Create a new MemoryScrubber.
    // start: pointer to the beginning of the memory area to be scrubbed. Must
    //      be a multiple of the cache line size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache line size.
    pub fn new(cache_desc: &'a dyn MemoryScrubberCacheDesc<Cacheline>, start: *const u8, size: usize)
        -> Result<MemoryScrubberIterator<Cacheline>, MemoryScrubberError> {
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
            cache_desc: cache_desc,
            start:      start as *const Cacheline,
            size:       (size / cache_desc.cacheline_size()) as isize,
            index:      0,
            offset:     0,
            addr_mask:  !((1 << (cache_desc.cacheline_width() +
                             cache_desc.cache_width())) - 1),
        })
    }

    // Return the cache index for the given address
    fn cache_index(&self, addr: *const Cacheline) -> isize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !self.addr_mask) >>
            self.cache_desc.cacheline_width()) as isize
    }
}

impl<Cacheline> Iterator for MemoryScrubberIterator<'_, Cacheline> {
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

    // ECCData - The data size used to compute the ECC
    type ECCData = u64;

    // Cacheline - the data type of a cache line
    type Cacheline =
        [u64; (1 << BASIC_CACHELINE_WIDTH) / std::mem::size_of::<ECCData>()];

    // Cache characteristics
    // BASIC_CACHELINE_WIDTH - number of bits required to index a byte in a
    //  cache line
    // BASIC_CACHE_WIDTH - number of bits used as a cache line index in the
    //  cache
    const BASIC_CACHELINE_WIDTH: usize = 6;
    const BASIC_CACHE_WIDTH: usize = 10;

    // BasicTestCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_width - Number of bits of the cache index.
    #[derive(Clone, Copy, Debug)]
    pub struct BasicTestCacheDesc {
        cacheline_width:    usize,
        cache_width:        usize,
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    const BASIC_CACHE_DESC: BasicTestCacheDesc = BasicTestCacheDesc {
        cacheline_width:    BASIC_CACHELINE_WIDTH,
        cache_width:        BASIC_CACHE_WIDTH,
    };

    impl MemoryScrubberCacheDesc<Cacheline> for BasicTestCacheDesc {
        fn cache_width(&self) -> usize {
            self.cache_width
        }

        fn read_cacheline(&self, cacheline: *const Cacheline) {
            println!("Read cacheline at {:x}", cacheline as usize);
        }
    }

    // Cache characteristics
    // TOUCHING_CACHELINE_WIDTH - number of bits required to index a byte in a
    //  cache line
    // TOUCHING_CACHE_WIDTH - number of bits used as a cache line index in the
    //  cache
    const TOUCHING_CACHELINE_WIDTH: usize = 6;
    const TOUCHING_CACHE_WIDTH: usize = 10;

    // Cacheline - the data type of a cache line
    type TouchingCacheline =
        [u64; (1 << TOUCHING_CACHELINE_WIDTH) / std::mem::size_of::<ECCData>()];

    // TouchingTestCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_width - Number of bits of the cache index.
    #[derive(Clone, Copy, Debug)]
    pub struct TouchingTestCacheDesc {
        cacheline_width:    usize,
        cache_width:        usize,
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    const TOUCHING_CACHE_DESC: TouchingTestCacheDesc = TouchingTestCacheDesc {
        cacheline_width:    TOUCHING_CACHELINE_WIDTH,
        cache_width:        TOUCHING_CACHE_WIDTH,
    };

    impl MemoryScrubberCacheDesc<TouchingCacheline> for TouchingTestCacheDesc {
        fn cache_width(&self) -> usize {
            self.cache_width
        }

        fn read_cacheline(&self, cacheline: *const TouchingCacheline) {
            println!("Read cacheline at {:x}", cacheline as usize);
        }
    }

    // Verify that an error is returned if the address is not aligned on
    // a cache line boundary
    #[test]
    fn test_unaligned_address() {
        let size = BASIC_CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(&BASIC_CACHE_DESC, p, size);
        let p = unsafe {
            p.offset(1)
        };

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&BASIC_CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedAddress);
    }

    // Verify that an error is returned if the size is not a multiple of the
    // cache line size.
    #[test]
    fn test_unaligned_size() {
        let size = BASIC_CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(&BASIC_CACHE_DESC, p, size);
        let size = size - 1;

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&BASIC_CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::UnalignedSize);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let size = BASIC_CACHE_DESC.cacheline_size() * 16;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, _size) = align_area(&BASIC_CACHE_DESC, p, size);
        let size = 0;

        let memory_scrubber =
            MemoryScrubberIterator::<Cacheline>::new(&BASIC_CACHE_DESC, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            MemoryScrubberError::ZeroSize);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        let size = BASIC_CACHE_DESC.cacheline_size() * BASIC_CACHE_DESC.cache_size() * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(&BASIC_CACHE_DESC, p, size);

        let mut memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(&BASIC_CACHE_DESC, p, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        match memory_scrubber.scrub(BASIC_CACHE_DESC.cacheline_size() * 10) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };
    }

    // Verify that all specified locations are scrubbed on locations outside
    // the requested are are not touched.
    #[test]
    fn test_touching() {
        let size = TOUCHING_CACHE_DESC.cacheline_size() * TOUCHING_CACHE_DESC.cache_size() * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = align_area(&TOUCHING_CACHE_DESC, p, size);

        let mut memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(&TOUCHING_CACHE_DESC, p, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        match memory_scrubber.scrub(TOUCHING_CACHE_DESC.cacheline_size() * 10) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };
    }

    fn align_area(cache_desc: &dyn MemoryScrubberCacheDesc<Cacheline>, p: *const u8, size: usize) ->
        (*const u8, usize) {
        let lower_p = (p as Addr) % cache_desc.cacheline_size();
        let p_offset = cache_desc.cacheline_size() - lower_p;
        let p = unsafe { p.offset(p_offset as isize) };

        let size = size - p_offset;
        let size = size - (size % cache_desc.cacheline_size());
        (p, size)
    }
}
