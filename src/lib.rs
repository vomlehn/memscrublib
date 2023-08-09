// This is code for a memory scrubber.
//
// What is a memory scrubber and why would you use one?
//
// A memory scrubber is simply a piece of hardware or software that reads all
// bytes from a section of memory, usually from all of memory. This is an
// implementation of a software memory scrubber. When a processor reads from
// memory protected by an error correction code (ECC), it checks to see if
// there are errors in the piece of memory it has read. If so, in hardware or
// software, the ECC is used to correct the errors and the corrected value
// used to replace the bad value.
//
// ECCs are limited in the number of errors they can correct. These errors
// generally accumulate over time. So long as memory is read often enough,
// correction is applied with enough frequency that the number of errors
// stays within the bounds of what is correctable. If, however, a piece of
// memory is rarely accessed, it can accumulate multiple errors. When that
// memory is eventually used, it will not be possible to determine the corrected
// value and a fatal error will result. This is where a memory scrubber comes
// in.
//
// In general, memory is scrubbed at a rate high enough that the number of
// accumulated errors remains low enough that the number of uncorrectable
// errors is extremely low. Since it isn't possible to predict which areas of
// memory are read frequently enough to avoid error accumulation, the usual
// practice is to scan all of memory. With modern systems, this can be quite be
// a large amount of work and the scrubbing work is broken into smaller pieces
// to avoid any significant amount of performance impact.
//
// The choice of how often memory is scrubbed depends on:
// o    The probability that an uncorrectable number of errors will accumulate
//      in a particular section
// o    How many sections of memory are present in the system
// o    The goal for the probability that a fault due to an uncorrectable
//      number of errors anywhere in memory will occur.
//
// Choosing how the scrubbing work is divided into smaller piece depends on
// many implementation details, like:
// o    Will the scrubbing be done with preemption blocked?
// o    How long does it take to scrub each section of memory?
// o    What is the overhead of entering and leaving the scrubbing code each
//      time it is run?
//
// One key performance impact of memory scrubbing is that read memory will
// evict the memory cache contents being used by other software on the system.
// When that software resumes, it will have to re-read all the data it wants
// to use, which can cause a substantial performance impact all at once.  This
// code is written to perform its reads all of memory corresponding to a
// single cache line at a time. If memory scrubbing is broken into smaller
// chunks, data will be evicted from only a few cache lines each time scrubbing
// is done, producing a more even performance impact.

use std::cell::RefCell;
use std::iter;
use std::rc::Rc;
use thiserror::Error;

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// Describe the cache sizes and pull in all elements of the cache line.
pub trait CacheDescBase<Cacheline> {
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
    // of bad bits is small enough (ECC-dependent), corrected data should
    // be written back to that location, preventing the accumulation of so many
    // bad bits that the correct value cannot be determined.
    fn read_cacheline(&mut self, p: *const Cacheline);
}

type CacheDesc<'a, Cacheline> =
    Rc<RefCell<&'a mut dyn CacheDescBase<Cacheline>>>;

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
pub enum Error {
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
    cache_desc: CacheDesc<'a, Cacheline>,
    start:      *const u8,
    size:       usize,
    iterator:   Iterator<'a, Cacheline>,
}

impl<'a, Cacheline> MemoryScrubber<'a, Cacheline> {

    pub fn new(cache_desc: CacheDesc<'a, Cacheline>,
        start: *const u8, size: usize) ->
        Result<MemoryScrubber<'a, Cacheline>, Error> {
        let iterator = {
            Iterator::new(cache_desc.clone(), start, size)?
        };

        Ok(MemoryScrubber::<'a> {
            cache_desc: cache_desc,
            start:      start,
            size:       size,
            iterator:   iterator,
        })
    }

    // Scrub some number of bytes. This could be larger than the total memory
    // area, in which case the scrubbing will start again at the beginning
    // of the memory area, but it seems unlikely that this would be useful.
    // n - Number of bytes to scrub
    pub fn scrub(&mut self, n: usize) -> Result<(), Error> {
        let cacheline_size = {
            self.cache_desc.clone().borrow().cacheline_size()
        };

        if (n % cacheline_size) != 0 {
            return Err(Error::UnalignedSize);
        }

        // Convert to the number of cachelines to scrub
        let n = n / cacheline_size;

        for _i in 0..n {
            // If we don't already have an iterator, get a new one.
            let p = match self.iterator.next() {
                None => {
                    self.iterator =
                        match Iterator::new(self.cache_desc.clone(),
                        self.start, self.size) {
                        Err(e) => return Err(e),
                        Ok(iterator) => iterator,
                    };
                    self.iterator.next().unwrap()
                },
                Some(p) => p,
            };

            self.cache_desc.clone().borrow_mut().read_cacheline(p);
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
pub struct Iterator<'a, Cacheline> {
    cache_desc: CacheDesc<'a, Cacheline>,
    start:      *const Cacheline,
    size:       isize,
    index:      isize,
    offset:     isize,
    addr_mask:  usize,
}

impl<'a, Cacheline> Iterator<'a, Cacheline> {

    // Create a new MemoryScrubber.
    // start: pointer to the beginning of the memory area to be scrubbed. Must
    //      be a multiple of the cache line size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache line size. There is no way to specify the
    //      full address range starting at zero. Simply split memory into two
    //      pieces and alternate scrubbing them.
    //
    // Returns: Ok(Iterator) on success, Err(Error) on failure
    pub fn new(cache_desc: CacheDesc<'a, Cacheline>,
        start: *const u8, size: usize) -> Result<Iterator<'a, Cacheline>, Error> {
        let start_addr = start as Addr;
        let cacheline_size = {
            cache_desc.borrow().cacheline_size()
        };

        if (start_addr % cacheline_size) != 0 {
            return Err(Error::UnalignedAddress);
        }

        if size == 0 {
            return Err(Error::ZeroSize);
        }

        if (size % cacheline_size) != 0 {
            return Err(Error::UnalignedSize);
        }

        let cacheline_width = {
            cache_desc.borrow().cacheline_width()
        };

        let cache_width = {
            cache_desc.borrow().cache_width()
        };

        Ok(Iterator {
            cache_desc: cache_desc,
            start:      start as *const Cacheline,
            size:       (size / cacheline_size) as isize,
            index:      0,
            offset:     0,
            addr_mask:  !((1 << (cacheline_width + cache_width)) - 1),
        })
    }

    // Return the cache index for the given address
    fn cache_index(&self, addr: *const Cacheline) -> isize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !self.addr_mask) >>
            self.cache_desc.borrow().cacheline_width()) as isize
    }
}

impl<Cacheline> iter::Iterator for Iterator<'_, Cacheline> {
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
                self.offset += self.cache_desc.borrow().cache_size() as isize;
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
    const BASIC_CACHE_AREA_SIZE: usize =
        1 << (BASIC_CACHELINE_WIDTH + BASIC_CACHE_WIDTH);

    // BasicTestCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_width - Number of bits of the cache index.
    #[derive(Clone, Copy, Debug)]
    pub struct BasicTestCacheDesc {
        cache_width:        usize,
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    static BASIC_CACHE_DESC: BasicTestCacheDesc = BasicTestCacheDesc {
        cache_width:        BASIC_CACHE_WIDTH,
    };

    impl CacheDescBase<Cacheline> for BasicTestCacheDesc {
        fn cache_width(&self) -> usize {
            self.cache_width
        }

        fn read_cacheline(&mut self, cacheline: *const Cacheline) {
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
    const TOUCHING_CACHE_SIZE: usize = 1 << TOUCHING_CACHE_WIDTH;
    const TOUCHING_CACHE_NUM_TOUCHED: usize = 3;

    // This is the number of cache line-sized areas we use for testing
    const TOUCHING_SANDBOX_SIZE: usize = TOUCHING_CACHE_SIZE + // For checking before the area we touch
        TOUCHING_CACHE_SIZE * TOUCHING_CACHE_NUM_TOUCHED + // For touching
        TOUCHING_CACHE_SIZE; // For checking after the area we touch
    // Cacheline - the data type of a cache line
    type TouchingCacheline =
        [u64; (1 << TOUCHING_CACHELINE_WIDTH) / std::mem::size_of::<ECCData>()];

    // TouchingTestCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_width - Number of bits of the cache index.
    #[derive(Clone, Debug)]
    pub struct TouchingTestCacheDesc {
        cache_width:        usize,
        hit:                Option<Vec<bool>>,
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    static TOUCHING_CACHE_DESC: TouchingTestCacheDesc = TouchingTestCacheDesc {
        cache_width:        TOUCHING_CACHE_WIDTH,
        hit:                None,
    };

    impl CacheDescBase<TouchingCacheline> for TouchingTestCacheDesc {
        fn cache_width(&self) -> usize {
            self.cache_width
        }

        fn read_cacheline(&mut self, cacheline: *const TouchingCacheline) {
            if self.hit.is_none() {
                self.hit = Some(vec![false; TOUCHING_SANDBOX_SIZE]);
            }

            println!("Read cacheline at {:x}", cacheline as usize);
        }
    }

    // Verify that an error is returned if the address is not aligned on
    // a cache line boundary
    #[test]
    fn test_unaligned_address() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        let size = BASIC_CACHE_AREA_SIZE + BASIC_CACHE_AREA_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.clone(), p, size) {
            None => panic!("Bad aligned size"),
            Some((p, size)) => (p, size),
        };

        let p = unsafe {
            p.offset(1)
        };

        let memory_scrubber =
            Iterator::<Cacheline>::new(cache_desc, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedAddress);
    }

    // Verify that an error is returned if the size is not a multiple of the
    // cache line size.
    #[test]
    fn test_unaligned_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        let size = BASIC_CACHE_AREA_SIZE + BASIC_CACHE_AREA_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.clone(), p, size) {
            None => panic!("Bad aligned size"),
            Some((p, size)) => (p, size),
        };
        let size = size - 1;

        let memory_scrubber =
            Iterator::<Cacheline>::new(cache_desc, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedSize);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        // Make sure we have at least a cache size
        let size = BASIC_CACHE_AREA_SIZE + BASIC_CACHE_AREA_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, _size) = match align_area(cache_desc.clone(), p, size) {
            None => panic!("Bad aligned size"),
            Some((p, size)) => (p, size),
        };
        let size = 0;

        let memory_scrubber =
            Iterator::<Cacheline>::new(cache_desc, p, size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::ZeroSize);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        let size = cache_desc.borrow().cacheline_size() *
            cache_desc.borrow().cache_size() * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.clone(), p, size) {
            None => panic!("Bad aligned size"),
            Some((p, size)) => (p, size),
        };

        let mut memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(cache_desc.clone(), p, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        let cacheline_size = cache_desc.borrow().cacheline_size();

        match memory_scrubber.scrub(cacheline_size * 10) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };
    }

    // Verify that all specified locations are scrubbed on locations outside
    // the requested are are not touched.

    // Test scrubbing:
    // o    Zero cache lines
    // o    One cache line
    // o    Fifty cache lines
    // o    The entire size of the cache area
    // o    Double the cache area size plus fifty (test wrapping)
    #[test]
    fn test_touching() {
        let touching_cache_desc = &mut TOUCHING_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(touching_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        // Figure out the size, adding in space to handle alignment
        let size =
            cache_desc.borrow().cacheline_size() * TOUCHING_SANDBOX_SIZE +
            TOUCHING_CACHE_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.clone(), p, size) {
            None => panic!("Bad aligned size"),
            Some((p, size)) => (p, size),
        };

        let mut memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(cache_desc.clone(), p, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        let cacheline_size = cache_desc.borrow().cacheline_size();

        match memory_scrubber.scrub(cacheline_size * 10) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };
    }

    // Align the pointer to a multiple of the product of the size of the cache
    // line and the maximum cache index (cache_area_size). This address will
    // therefore have a cache index of zero.
    // p - pointer to the memory area
    // size - size of memory area in bytes
    //
    // Returns Some(p, size) on success, where p is aligned to the next higher
    // multiple of cache_area_size. The size is the original size, decremented
    // by the number of bytes p was incremented, then rounded to a multple of
    // cache_area_size. None is returned if p would be incremented by more than
    // the original size or rounded down to zero.
    fn align_area(cache_desc: CacheDesc<Cacheline>, p: *const u8, size: usize) ->
        Option<(*const u8, usize)> {
        // If we go through memory with addresses offset by the cacheline size,
        // we will be incrementing the cache index by one and thus hitting a
        // different cache line each time. The cache index will wrap when the
        // address has been incremented by the product of the cache line size
        // and the maximum cache index value. Call this product cache_area_size.
        //
        // If we start this walk through memory at an address that is a
        // multiple of cache_area_size, cache index of that address will be
        // zero.  This is a terrifc starting place for the tests. We round the
        // pointer up to the next multiple of cache_area_size because there
        // might be no memory before it. This means the returned pointer might
        // be incremented as much as cache_area_size - 1 and additional memory
        // may need to be allocated to ensure enough is available to perform the
        // tests.
        let cache_area_size = cache_desc.borrow().cache_size() *
            cache_desc.borrow().cacheline_size();

        // Compute the number of bytes to add to p to reach the next address
        // that is a multiple of cache_area_size;
        let p_offset = cache_area_size - ((p as Addr) % cache_area_size);
        if p_offset >= size {
            return None;
        }

        let p = unsafe { p.offset(p_offset as isize) };

        // We just gobbled up p_offset bytes at the start of the memory area
        // and so need to decrement the size accordingly. Then we align the
        // size to a multiple of the c
        let adjusted_size = size - p_offset;
        let size = adjusted_size - (adjusted_size % cache_area_size);
        if size == 0 {
            return None;
        }

        Some((p, size))
    }
}
