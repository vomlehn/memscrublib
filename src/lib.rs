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
use std::sync::{Mutex, Arc};
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
    fn cache_index_width(&self) -> usize;

    // NOTE: You are unlikely to ever need to implement this
    // Return the number of cache lines in the index. For a 1024 line cache
    // and a 16 byte cache line, this will be 64.
    fn cache_lines(&self) -> usize {
        1 << self.cache_index_width()
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
    //      be a multiple of the cache size.
    // size: number of bytes in the memory area to be scrubbed. Must be a
    //      multiple of the cache size. There is no way to specify the
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

        let cacheline_width = {
            cache_desc.borrow().cacheline_width()
        };

        let cache_index_width = {
            cache_desc.borrow().cache_index_width()
        };

        // Number of bytes in the cache or, if the bits in the address above
        // the cache index are held constant, the number of bytes covered
        // by the cache. In both cases, ignoring ways.
        let cache_size = 1 << (cache_index_width + cacheline_width);

        if (start_addr % cache_size) != 0 {
            return Err(Error::UnalignedAddress);
        }

        if size == 0 {
            return Err(Error::ZeroSize);
        }

        if (size % cache_size) != 0 {
            return Err(Error::UnalignedSize);
        }

        Ok(Iterator {
            cache_desc: cache_desc,
            start:      start as *const Cacheline,
            size:       (size / cacheline_size) as isize,
            index:      0,
            offset:     0,
            addr_mask:  !(cache_size - 1),
        })
    }

    // Return the cache index for the given address
    fn cache_index(&self, addr: *const Cacheline) -> isize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !self.addr_mask) >>
            self.cache_desc.borrow().cacheline_width()) as isize
    }
}

// Return a pointer into a series of Cacheline items. To get a byte address
// from the return value of next(), call it ret_val, use:
//
//      ret_val.unwrap().offset(0) as *const u8
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
                self.offset += self.cache_desc.borrow().cache_lines() as isize;
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
    // BASIC_CACHE_INDEX_WIDTH - number of bits used as a cache line index in the
    //  cache
    const BASIC_CACHELINE_WIDTH: usize = 6;
    const BASIC_CACHE_INDEX_WIDTH: usize = 10;
    const BASIC_CACHE_SIZE: usize =
        1 << (BASIC_CACHELINE_WIDTH + BASIC_CACHE_INDEX_WIDTH);

    // BasicTestCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_index_width - Number of bits of the cache index.
    #[derive(Clone, Copy, Debug)]
    pub struct BasicTestCacheDesc {
        cache_index_width:        usize,
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    static BASIC_CACHE_DESC: BasicTestCacheDesc = BasicTestCacheDesc {
        cache_index_width:        BASIC_CACHE_INDEX_WIDTH,
    };

    impl CacheDescBase<Cacheline> for BasicTestCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&mut self, cacheline: *const Cacheline) {
        }
    }

    // Cache characteristics
    // TOUCHING_CACHELINE_WIDTH - number of bits required to index a byte in a
    //  cache line
    // TOUCHING_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    const TOUCHING_CACHELINE_WIDTH: usize = 6;
    const TOUCHING_CACHE_INDEX_WIDTH: usize = 10;
    const TOUCHING_CACHE_LINES: usize = 1 << TOUCHING_CACHE_INDEX_WIDTH;

    // Number of cache footprints we use for testing
    const TOUCHING_CACHE_NUM_TOUCHED: usize = 3;

    // Number of cachelines we actually expect to touch
    const TOUCHING_SIZE: usize =
        TOUCHING_CACHE_LINES * TOUCHING_CACHE_NUM_TOUCHED;

    // This is the number of cache line-sized areas we use for testing
    const TOUCHING_SANDBOX_SIZE: usize = TOUCHING_CACHE_LINES + // For checking before the area we touch
        TOUCHING_SIZE +
        TOUCHING_CACHE_LINES; // For checking after the area we touch
    // Cacheline - the data type of a cache line
    type TouchingCacheline =
        [u64; (1 << TOUCHING_CACHELINE_WIDTH) / std::mem::size_of::<ECCData>()];

    // This allows passing a *const pointer
    #[derive(Clone, Debug)]
    struct TouchingPtr {
        ptr:    *const TouchingCacheline,
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for TouchingPtr {}

    // TouchingCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_index_width - Number of times this cacheline was hit during the
    //      scrub
    #[derive(Clone, Debug)]
    pub struct TouchingCacheDesc {
        cache_index_width:  usize,
        hit:                Option<Arc<Mutex<Vec<u32>>>>,
        p:                  TouchingPtr,
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Send for TouchingCacheDesc {}

    // Cache descriptor to pass to the memory scrubbing functions.
    static TOUCHING_CACHE_DESC: TouchingCacheDesc = TouchingCacheDesc {
        cache_index_width:  TOUCHING_CACHE_INDEX_WIDTH,
        hit:                None,
        p:                  TouchingPtr { ptr: std::ptr::null() },
    };

    impl CacheDescBase<TouchingCacheline> for TouchingCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&mut self, cacheline: *const TouchingCacheline) {
println!("Testing read cacheline at {:x}", cacheline as usize);
            let offset = (self.p.ptr as usize - cacheline as usize) /
                self.cacheline_size();
println!("offset {}", offset);
            let i = self.cache_lines() + offset;
println!("hit {:?}", self.hit.as_ref().unwrap().lock().unwrap().as_ptr());
println!("i {:?}", i);
            self.hit.as_ref().unwrap().lock().unwrap()[i] += 1;
        }
    }

    // Verify that an error is returned if the address is not aligned on
    // a cache line boundary
    #[test]
    fn test_unaligned_address() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &mut dyn CacheDescBase<Cacheline>));
        let size = BASIC_CACHE_SIZE + BASIC_CACHE_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.borrow().cache_lines(),
            cache_desc.borrow().cacheline_size(), p, size) {
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
        let size = BASIC_CACHE_SIZE + BASIC_CACHE_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.borrow().cache_lines(),
            cache_desc.borrow().cacheline_size(), p, size) {
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
        let size = BASIC_CACHE_SIZE + BASIC_CACHE_SIZE;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, _size) = match align_area(cache_desc.borrow().cache_lines(),
            cache_desc.borrow().cacheline_size(), p, size) {
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
            cache_desc.borrow().cache_lines() * 14;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        let (p, size) = match align_area(cache_desc.borrow().cache_lines(),
            cache_desc.borrow().cacheline_size(), p, size) {
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

    // Verify that all specified locations are scrubbed and locations outside
    // the requested are are not touched.

    // Test scrubbing:
    // o    Zero cache lines
    // o    One cache line
    // o    Fifty cache lines
    // o    The entire size of the cache area
    // o    Double the cache area size plus fifty (test wrapping)
    #[test]
    fn test_touch_zero() {
        let scrubbable = TOUCHING_SIZE * TOUCHING_CACHE_DESC.cacheline_size();
        test_scrubber(&[0], &[(0, scrubbable, 0)]);
    }

    #[test]
    fn test_touch_one() {
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let scrubbable = (TOUCHING_SIZE - 1) * cacheline_size;
        test_scrubber(&[cacheline_size],
            &[(0, cacheline_size, 1), (cacheline_size, scrubbable, 0)]);
    }

    // Test support function that scrubs a section of memory, then verifies that
    // things were properly referred.
    // scrubs - description of regions to scrub: (start, length), where start
    //      is a byte offset from the beginning of the memory area and
    //      length is the number of byte to scrub. Both must be a multiple of
    //      the cacheline size
    // expecteds - expected values in the touching area
    fn test_scrubber(scrubs: &[usize],
        expecteds: &[(usize, usize, u32)]) {
        let mut touching_cache_desc = TOUCHING_CACHE_DESC.clone();
        touching_cache_desc.hit = Some(Arc::new(Mutex::new(vec![0;
            TOUCHING_CACHE_LINES + TOUCHING_SIZE + TOUCHING_CACHE_LINES])));
        let alloc = match touching_alloc(&touching_cache_desc) {
            Err(e) => panic!("Could not allocation touching descriptor: {}", e),
            Ok(alloc) => alloc,
        };
        touching_cache_desc.p =
            TouchingPtr { ptr: alloc.0 as *const TouchingCacheline, };
println!("p is {:?}", alloc.0);
/*
        touching_cache_desc.hit = Arc::new(Mutex::new(vec![0;
            TOUCHING_CACHE_LINES + TOUCHING_SIZE + TOUCHING_CACHE_LINES]));
*/
/*
        let cache_desc = Rc::new(RefCell::new(touching_cache_desc as &mut dyn CacheDescBase<Cacheline>));
*/
        let tcd_clone = touching_cache_desc.clone();

        let mut memory_scrubber =
            match touching_init(&mut touching_cache_desc, alloc) {
            Err(e) => panic!("Unable to initialize touching area: {}", e),
            Ok(memory_scrubber) => memory_scrubber,
        };

        let cacheline_size =
            memory_scrubber.cache_desc.borrow().cacheline_size();

        for scrub in scrubs {
            assert_eq!(scrub % cacheline_size, 0);

            match memory_scrubber.scrub(*scrub) {
                Err(e) => panic!("scrub failed: {}", e),
                Ok(_) => println!("scrub succeeded!"),
            };
        }

        let cache_lines = memory_scrubber.cache_desc.borrow().cache_lines();

        // Verify the cache size-sized area before the memory area. This should
        // not have been seen and so should have a zero value
        for i in 0..cache_lines {
            let actual = tcd_clone.hit.as_ref().unwrap().lock().unwrap()[i];
            assert_eq!(actual, 0);
        }

        // Now verify the contents of the memory to see whether they were
        // touched the expected number of times
        for expected in expecteds {
            assert_eq!(expected.0 % cacheline_size, 0);
            let start = expected.0 / cacheline_size;
            assert_eq!(expected.1 % cacheline_size, 0);
            let n = expected.1 / cacheline_size;
            let expected_val = expected.2;
println!("expected {:?} start {:?} n {:?}", expected, start, n);


            for i in start..start + n {
                let actual =
                    tcd_clone.hit.as_ref().unwrap().lock().unwrap()[cache_lines + i];
//println!("checking hit[{}]", cache_lines + i);
if actual != expected_val {
}
                assert_eq!(actual, expected_val);
            }
        }

        // Verify the cache size-sized area after the memory area. This should
        // not have been seen and so should have a zero value
        for i in 0..cache_lines {
            let start = cache_lines + TOUCHING_SIZE;
            let actual = tcd_clone.hit.as_ref().unwrap().lock().unwrap()[start + i];
            assert_eq!(actual, 0);
        }
    }

    fn touching_init<'a>(touching_cache_desc: &'a mut TouchingCacheDesc,
            alloc: (*const u8, usize)) ->
        Result<MemoryScrubber<'a, Cacheline>, Error> {

        // Set up the CacheDesc for the part of memory that will be scrubbed
        // and include debugging information
        let cache_desc = Rc::new(RefCell::new(touching_cache_desc as &mut dyn CacheDescBase<Cacheline>));

        let memory_scrubber =
            match MemoryScrubber::<Cacheline>::new(cache_desc, alloc.0,
                alloc.1) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        Ok(memory_scrubber)
    }

    fn touching_alloc(touching_cache_desc: &TouchingCacheDesc) ->
        Result<(*const u8, usize), Error> {
        // Allocate memory, which includes a cache size-sided area before what
        // we are touching and a cache size-sized area after it. These areas
        // should not be touched by scrubbing.
        let size =
            touching_cache_desc.cacheline_size() * TOUCHING_SANDBOX_SIZE +
            TOUCHING_CACHE_LINES;
        let mem_area: Vec<u8> = vec![0; size];
        let p = mem_area.as_ptr() as *const u8;
        match align_area(touching_cache_desc.cache_lines(),
            touching_cache_desc.cacheline_size(), p, size) {
            None => Err(Error::UnalignedSize),
            Some((p, size)) => Ok((p, size)),
        }
    }

    // Align the pointer to a multiple of the product of the size of the cache
    // line and the maximum cache index (cache_area_size). This address will
    // therefore have a cache index of zero.
    // cache_lines - number of cache lines in the cache
    // cacheline_size - number of bytes in a cache line
    // p - pointer to the memory area
    // size - size of memory area in bytes
    //
    // Returns Some(p, size) on success, where p is aligned to the next higher
    // multiple of cache_area_size. The size is the original size, decremented
    // by the number of bytes p was incremented, then rounded to a multple of
    // cache_area_size. None is returned if p would be incremented by more than
    // the original size or rounded down to zero.
    fn align_area(cache_lines: usize, cacheline_size: usize, p: *const u8,
        size: usize) ->
        Option<(*const u8, usize)> {
        // If we go through memory with addresses offset by the cacheline size,
        // we will be incrementing the cache index by one and thus hitting a
        // different cache line each time. The cache index will wrap when the
        // address has been incremented by the product of the cache line size
        // and the maximum cache index value. Call this product cache_area_size.
        //
        // If we start this walk through memory at an address that is a
        // multiple of cache_area_size, the cache index of that address will be
        // zero.  This is a terrifc starting place for the tests. We round the
        // pointer up to the next multiple of cache_area_size because there
        // might be no memory before it. This means the returned pointer might
        // be incremented as much as cache_area_size - 1 and additional memory
        // may need to be allocated to ensure enough is available to perform the
        // tests.
        let cache_area_size = cache_lines * cacheline_size;

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
