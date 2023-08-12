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
//
// This code assumes that an address can be broken into three parts:
//
//       _____________________________________________________________
//      |                                |               |            |
//      |     Address upper bits         |  Cache index  | Cache line |
//      |                                |               |   index    |
//      |________________________________|_______________|____________|
//
// The cache index is the index of the cache line in the cache. The cache
// line index is the index of a particular byte in the cache line indicated
// by the cache index. The upper bits of the address might be used to select
// a specific way within the cache line specified by the cache index but don't
// usually otherwise participate in cache operations.
//
// To use this, it recommended you do the following:
//
// 1.   Determine a suitable data type to represent the size of object that
//      is used by the unit the computes the ECC. This is probably either u32
//      or u64, and we'll call it ECCData here, though you can call it anything
//      you wish.
//
// 2.   Define the structure of a cache line by implementing Cacheline for
//      the particular layout for your processor. We'll call the structure
//      MyCacheline. It usually the case that cache lines are arrays of ECCData
//      items, such as:
//
//          type MyCacheline = [u64; 8];
//
//      Since many systems have more than one level of cache memory, note
//      that this should be the longest cache line in in the system.
//
// 3.   Create a structure that holds definitions of your cache. This is
//      an implementation of CacheDesc. In most cases, the only things you
//      need to define are the following:
//
//      a.  A function that returns the number of bits in the cache index
//          portion of an address. For example:
//
//              fn cache_index_width(&self) -> usize {
//                  10
//              }
//
//      b.  A function that will cause all bytes in a cache line to be read
//          and checked for a correct ECC.  If the entire cache is read when
//          any element is read, this could be:
//
//              fn read_cacheline(&self, cacheline: *const Cacheline) {
//                  let _dummy = unsafe {
//                      ptr::read(&((*cacheline)[0]) as *const _);
//                  };
//              }
//
//          It is possible that the longest cache line will not be entirely
//          read when a single element is read. Since any memory not read
//          will not be checked for errors, it is important that this function
//          implement a full cache line read.

use std::cell::RefCell;
use std::iter;
use std::ptr;
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
    fn read_cacheline(&self, p: *const Cacheline);
}

type CacheDesc<'a, Cacheline> =
    Rc<RefCell<&'a dyn CacheDescBase<Cacheline>>>;

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

// Memory scrubber
// cache_desc - Description of the cache
// start - Virtual address of memory to be scrubbed
// size - Number of bytes in the memory area being scrubbed
// iterator - Iterator used to walk through the memory being scrubbed
struct MemoryScrubber<'a, Cacheline> {
    cache_desc: CacheDesc<'a, Cacheline>,
    start:      *const u8,
    size:       usize,
    iterator:   Iterator<'a, Cacheline>,
}

impl<'a, Cacheline> MemoryScrubber<'a, Cacheline> {

    // Create a new memory scrubber
    // cache_desc - Description of the cache
    // start - Virtual address of memory being scrubbed
    // size - Number of bytes in the area being scrubbed
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
            self.cache_desc.borrow().cacheline_size()
        };

        if (n % cacheline_size) != 0 {
            return Err(Error::UnalignedSize);
        }

        // Convert to the number of cachelines to scrub
        let n_cachelines = n / cacheline_size;

        for _i in 0..n_cachelines {
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

            self.cache_desc.borrow().read_cacheline(p);
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
    size:       usize,
    index:      usize,
    offset:     usize,
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
            size:       (size / cacheline_size) as usize,
            index:      0,
            offset:     0,
            addr_mask:  !(cache_size - 1),
        })
    }

    // Return the cache index for the given address
    fn cache_index(&self, addr: *const Cacheline) -> usize {
        // Pull out the cache index corresponding the the starting address
        ((addr as Addr & !self.addr_mask) >>
            self.cache_desc.borrow().cacheline_width()) as usize
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
            if self.index == self.cache_desc.borrow().cache_lines() as usize {
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
            let offset = self.cache_index(self.start) + self.index +
                self.offset;

            if offset < self.size {
                let res = unsafe {
                    self.start.offset(offset as isize)
                };
                self.offset += self.cache_desc.borrow().cache_lines();
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

    // ECCData - The data size used to compute the ECC for basic tests
    type BasicECCData = u64;

    // Cacheline - the data type of a cache line
    type BasicCacheline =
        [u64; (1 << BASIC_CACHELINE_WIDTH) / std::mem::size_of::<BasicECCData>()];

    // Cache characteristics
    // BASIC_CACHELINE_WIDTH - number of bits required to index a byte in a
    //      cache line
    // BASIC_CACHE_INDEX_WIDTH - number of bits used as a cache line index in
    //      the cache
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

    impl CacheDescBase<BasicCacheline> for BasicTestCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&self, cacheline: *const BasicCacheline) {
            let _dummy = unsafe {
                ptr::read(&((*cacheline)[0]) as *const _)
            };
        }
    }

    // Cache characteristics
    // TOUCHING_CACHELINE_WIDTH - number of bits required to index a byte in a
    //  cache line
    // TOUCHING_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    // TOUCHING_CACHE_LINES - number of cache lines
    const TOUCHING_CACHELINE_WIDTH: usize = 6;
    const TOUCHING_CACHE_INDEX_WIDTH: usize = 10;
    const TOUCHING_CACHE_LINES: usize = 1 << TOUCHING_CACHE_INDEX_WIDTH;

    // Number of cache footprints we use for testing
    const TOUCHING_CACHE_NUM_TOUCHED: usize = 3;

    // Number of cachelines we actually expect to touch
    const TOUCHING_SANDBOX_SIZE: usize =
        TOUCHING_CACHE_LINES * TOUCHING_CACHE_NUM_TOUCHED;

    // TouchingECCData - The data size used to compute the ECC for basic tests
    type TouchingECCData = u64;

    // TouchingCacheline - the data type of a cache line
    type TouchingCacheline =
        [u64; (1 << TOUCHING_CACHELINE_WIDTH) / std::mem::size_of::<TouchingECCData>()];

    // Description of memory that is read into by the read_cacheline() function.
    // This keeps the actually allocation together with the pointer into that
    // allocation so that things go out of scope at the same time.
    //
    // mem_area - Vec<u8> of elements that can be read by read_cacheline()
    // p - Cache size-aligned point with at least size readable elements
    // size - Number of bytes after p that can be read
    #[derive(Clone, Debug)]
    struct TouchingMem {
        mem_area:   Vec<u8>,
        ptr:        *const u8,
        size:       usize,
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for TouchingMem {}

    // TouchingCacheDesc - Description of the cache for basic tests
    // cacheline_width - Number of bits in the index into the cachline bytes
    // cache_index_width - Number of times this cacheline was iit during the
    //      scrub
    #[derive(Clone, Debug)]
    pub struct TouchingCacheDesc {
        cache_index_width:  usize,
        n_reads:            Option<Arc<Mutex<Vec<u32>>>>,
        mem:                Option<TouchingMem>,
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Send for TouchingCacheDesc {}

    // Cache descriptor to pass to the memory scrubbing functions.
    static TOUCHING_CACHE_DESC: TouchingCacheDesc = TouchingCacheDesc {
        cache_index_width:  TOUCHING_CACHE_INDEX_WIDTH,
        n_reads:            None,
        mem:                None,
    };

    impl CacheDescBase<TouchingCacheline> for TouchingCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&self, cacheline: *const TouchingCacheline) {
            // First, actually do the read
            let _dummy = unsafe {
                ptr::read(&((*cacheline)[0]) as *const _)
            };

            // Now update the bookkeeping data so we verify we did the right
            // thing
            let cacheline_offset =
                (cacheline as usize - self.mem.as_ref().unwrap().ptr as usize) /
                self.cacheline_size();
            let i = self.cache_lines() + cacheline_offset;
            let mut n_reads = self.n_reads.as_ref().unwrap().lock().unwrap();
            n_reads[i] += 1;
        }
    }

    // Verify that an error is returned if the address is not aligned on
    // a cache line boundary
    #[test]
    fn test_unaligned_address() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &dyn CacheDescBase<BasicCacheline>));
        let mut mem = alloc_mem::<BasicCacheline>(basic_cache_desc, BASIC_CACHE_SIZE);
        mem.ptr = unsafe {
            mem.ptr.offset(1)
        };

        let memory_scrubber =
            Iterator::<BasicCacheline>::new(cache_desc, mem.ptr, mem.size);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedAddress);
    }

    // Verify that an error is returned if the size is not a multiple of the
    // cache line size.
    #[test]
    fn test_unaligned_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &dyn CacheDescBase<BasicCacheline>));
        let mem = alloc_mem::<BasicCacheline>(basic_cache_desc, BASIC_CACHE_SIZE);

        let memory_scrubber =
            Iterator::<BasicCacheline>::new(cache_desc, mem.ptr, mem.size - 1);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedSize);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &dyn CacheDescBase<BasicCacheline>));
        let mem = alloc_mem::<BasicCacheline>(basic_cache_desc, BASIC_CACHE_SIZE);

        let memory_scrubber =
            Iterator::<BasicCacheline>::new(cache_desc, mem.ptr, 0);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::ZeroSize);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cache_desc = Rc::new(RefCell::new(basic_cache_desc as &dyn CacheDescBase<BasicCacheline>));
        let mem = alloc_mem::<BasicCacheline>(basic_cache_desc,
            basic_cache_desc.cacheline_size() *
            basic_cache_desc.cache_lines() * 14);

        let mut memory_scrubber =
            match MemoryScrubber::<BasicCacheline>::new(cache_desc.clone(),
                mem.ptr, mem.size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        let cacheline_size = cache_desc.borrow().cacheline_size();

        if let Err(e) = memory_scrubber.scrub(cacheline_size * 10) {
            panic!("scrub failed: {}", e);
        }
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
        let first_area = 0;
        test_scrubber(first_area);
    }

    #[test]
    fn test_touch_one() {
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let first_area = cacheline_size;
        test_scrubber(first_area);
    }

    #[test]
    fn test_touch_many() {
        const MANY: usize = 50;
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let first_area = cacheline_size * MANY;
        test_scrubber(first_area);
    }

    #[test]
    fn test_touch_all() {
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let first_area = cacheline_size * TOUCHING_SANDBOX_SIZE;
        test_scrubber(first_area);
    }

    #[test]
    fn test_touch_double_all() {
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let first_area = 2 * cacheline_size * TOUCHING_SANDBOX_SIZE;
        test_scrubber(first_area);
    }

    #[test] #[ignore]
    fn test_touch_many_many() {
        const MANY: usize = 72;
        let cacheline_size = TOUCHING_CACHE_DESC.cacheline_size();
        let first_area = 5 * cacheline_size * TOUCHING_SANDBOX_SIZE + MANY;
        test_scrubber(first_area);
    }

    // Test support function that scrubs a section of memory, then verifies that
    // things were properly referred.
    // n - number of cache lines to scrub
    fn test_scrubber(n: usize) {
        let mut touching_cache_desc = new_touching_cache_desc();
        let ptr = touching_cache_desc.mem.as_ref().unwrap().ptr;
        let size = touching_cache_desc.mem.as_ref().unwrap().size;
        let cacheline_size = touching_cache_desc.cacheline_size();
        let tcd = touching_cache_desc.clone();

        let cache_desc =
            Rc::new(RefCell::new(&mut touching_cache_desc as &dyn CacheDescBase<TouchingCacheline>));

        let mut memory_scrubber =
            match MemoryScrubber::new(cache_desc, ptr, size) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        match memory_scrubber.scrub(n) {
            Err(e) => panic!("scrub failed: {}", e),
            Ok(_) => println!("scrub succeeded!"),
        };

        let vec_mutex = tcd.n_reads.unwrap().clone();
        let vec = vec_mutex.lock().unwrap();
        let n_reads = vec.as_ref();
        touching_verify_scrub(memory_scrubber, n_reads, n / cacheline_size);
    }

    // Set up a new TouchingCacheDesc
    fn new_touching_cache_desc() -> TouchingCacheDesc {
        let mut touching_cache_desc = TOUCHING_CACHE_DESC.clone();

        // Allocate space for an array to hold the number of times a given
        // cache-aligned memory location has been read by the scrubber. We
        // allocate slop on both sides so we can detect if the read_cacheline()
        // function is a little broken
        let n_reads = Arc::new(Mutex::new(vec![0; TOUCHING_CACHE_LINES +
            TOUCHING_SANDBOX_SIZE + TOUCHING_CACHE_LINES]));

        // Get memory that will be scrubbed.
        let size = touching_cache_desc.cacheline_size () *
            TOUCHING_SANDBOX_SIZE;
        let mem = alloc_mem(&touching_cache_desc, size);

        touching_cache_desc.n_reads = Some(n_reads);
        touching_cache_desc.mem = Some(mem);

        touching_cache_desc
    }

    // Verify the proper locations were hit
    // memory_scrubber - MemoryScrubber to use
    // n_reads - array of read counts
    // n - number of cache lines scrubbed
    fn touching_verify_scrub(memory_scrubber: MemoryScrubber<TouchingCacheline>,
        n_reads: &Vec<u32>, n: usize) {
        let cache_desc = memory_scrubber.cache_desc.borrow();
        let cacheline_size = cache_desc.cacheline_size();
        let cache_lines = cache_desc.cache_lines();

        // Verify the cache size-sized area before the memory area. This should
        // not have been seen and so should have a zero value
        for i in 0..cache_lines {
            let actual = n_reads[i];
            assert_eq!(actual, 0);
        }

        // Now verify the contents of the memory to see whether they were
        // touched the expected number of times. The number of hits for a
        // location i in n_reads[] will be at least equal to the number of
        // complete scans of the memory area. Then, the remaining number of
        // items in the scan will be one larger.
        let size = memory_scrubber.size;
        let scrub_lines = size / cacheline_size;
        let n_min_reads = n / scrub_lines;
        let n_extra_reads = n % scrub_lines;
        let mut scanned: usize = 0;

        for line in 0..cache_lines {
            let mut i = line;

            while i < scrub_lines {
                let expected = n_min_reads +
                    if scanned < n_extra_reads { 1 } else { 0 };
                let actual = n_reads[cache_lines + i];
                assert_eq!(actual, expected as u32);
                scanned += 1;
                i += cache_lines;
            }
        }

        // Verify the cache size-sized area after the memory area. This should
        // not have been seen and so should have a zero value
        for i in 0..cache_lines {
            let start = cache_lines + TOUCHING_SANDBOX_SIZE;
            let actual = n_reads[start + i];
            assert_eq!(actual, 0);
        }
    }

    // Allocates a memory area
    //
    // size - The number of bytes to allocate
    // 
    // Returns: a TouchingMem with a Vec<u8>. The size of the Vec<u8> is
    // opaque but the p element in the TouchingMem has at least size bytes
    // starting at a cache size-aligned section of memory. The size element
    // is the size used to call this function.
    fn alloc_mem<Cacheline>(cache_desc: &dyn CacheDescBase<Cacheline>, size: usize) ->
        TouchingMem {
        // Allocate memory, which includes a cache size-sided area before what
        // we are touching. These areas should not be touched by scrubbing.
        let cacheline_size = cache_desc.cacheline_size();
        let cache_lines = cache_desc.cache_lines();
        let cache_size = cacheline_size * cache_lines;

        let mem_area: Vec<u8> = vec![0; cache_size + size];

        // Now find the first cache size-aligned pointer
        let p_addr = (mem_area.as_ptr() as usize + cache_size - 1) &
            !(cache_size - 1);
        let p = p_addr as *const u8;

        TouchingMem {
            mem_area:   mem_area,
            ptr:        p,
            size:       size,
        }
    }
}
