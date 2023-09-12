// FIXME: replace panic!() will something else
// FIXME: Convert Vec<ScrubArea> to [ScrubArea]
// FIXME: Maintain cur_index in MemoryScrubber. Necessary?
// FIXME: Remove #[ignore]
// FIXME: add replace_scrub_area()
// FIXME: Rename Cacheline to Cacheline?
// FIXME: use <const n> for test data types
// This is code for a memory scrubber.
//
// INTRODUCTION
// ============
// What is a memory scrubber and why would you use one?
//
// A memory scrubber is simply a piece of hardware or software that reads all
// bytes from a section of memory, usually from all of memory. This is an
// implementation of a software memory scrubber. When a processor reads from
// memory protected by an error correction code (ECC), it checks to see if
// there are errors in the piece of memory it has read. If so, in hardware or
// software, the ECC is used to correct the errors and the corrected value
// used to replace the bad value. The memory scrubber is run frequently
// enough that errors don't have a chance to accumulate
//
// This memory scrubber is specifically designed to allow the reduction of
// the impact of memory scrubbing. A simple memory scrubber might sequentially
// touch data one cache line apart. After touching a product of the number
// of cache lines in the cache and the number of ways per cache line, the
// previous contents of the cache will be complete evicted, requiring reloading
// when returning to the previous task. With 1024 cache lines and 4 ways,
// complete eviction will occur after 4096 touches.
//
// This memory scrubber is cache aware. As such, it scans through all addresses
// for a single cache line before advancing to the next cache line. With
// a 1 GB memory, 1024 cache lines, and a 64-byte cache line, it takes
// 16384 touches to evict a single cache line and 1677216 touches to evict
// the entire cache. This is a rate 2.4% of the simple approach.
//
// Using the cache aware memory scrubber is only useful if only part of
// memory is scrubbed at a time, but the rate of accumulation of error is
// slow enough that this is a very reasonable thing to do.
//
// NOTE: This is not intended to cover all possible cache
// implementations. Code to cover other variations is welcome.
//
// QUICK START
// ===========
// 1.   Add the lines:
//          use libmemscrub_arch::{CacheDesc, Cacheline,
//              MemoryScrubber, MemArea};
//
// 2.   Determine values for and define the following:
//
//      a.  The integer type that your ECC unit operates on.
//
//          type MyECCData = u64;
//
//      b.  The number of MyECC items that fit in the longest cache line of any
//          cache level in uour sysem.
//
//              const MY_CACHELINE_ITEMS: usize = 8;
//
//      c.  The number of bits in the address used for the cache index for the
//          longest cache line in your sysem.
//
//              const MY_CACHE_INDEX_WIDTH: usize = 10;
//
//      d.  A Cacheline structure that uses the above to define what a cache
//          line looks like. This has a very specific memory layout, so it
//          must be specified as a C structure:
//
//              #[repr(C)]
//              struct MyCacheline {
//                  data:   [MyECCData; MY_CACHELINE_ITEMS],
//              }
//
//              impl Cacheline for MyCacheline {}
//
//
// 2.   Define and implement a CacheDesc structure for your cache line. This
//      requires implementing the function cache_index_width(), which returns
//      the cache line width determined above, and read_cacheline(), which
//      causes the entire cacheline to be read:
//
//          struct MyCacheDesc {
//              cache_index_width: usize,
//          }
//
//          impl CacheDesc for MyCacheDesc {
//              fn cache_index_width(&self) -> usize { self.cache_index_width }
//              fn read_cacheline(&self,
//                  cacheline_ptr: *const MyCacheline) {
//                  let cacheline = unsafe { &*cacheline_ptr };
//                  let cacheline_data = &cacheline.data[0];
//                  let _dummy = unsafe { ptr::read(cacheline_data) };
//              }
//          }
//
//          static MY_CACHE_DESC: MyCacheDesc = MyCacheDesc {
//              cache_index_width:  MY_CACHE_INDEX_WIDTH,
//          };
// 
// 3.   Create an array with the virtual addresses of all memory areas to
//      scrub:
//
//          let my_scrub_areas = [
//              MemArea {
//                  start: 0xa0000000 as *const u8,
//                  end: 0xbfffffff as *const u8,
//              },
//              MemArea {
//                  start: 0xd0000000 as *const u8,
//                  end: 0xefffffff as *const u8,
//              },
//          ];
//
// 4.   The simplest thing to do is to use the autoscrub() function. The work
//      it does can be broken down, see below. Using autoscrub() consists of:
//
//      a.  Create a structure implementing the AutoScrubDesc trait. The
//          only thing that needs to be defined is a function that returns
//          the number of bytes to be scrubbed. If it returns zero, the
//          autoscrub() function will return:
//
//              struct MyAutoScrubDesc {
//                  scrub_size: usize,
//              }
//
//              impl AutoScrubDesc for MyAutoScrubDesc {
//                  fn next(&mut self) -> usize {
//                      self.scrub_size
//                  }
//              }
//
//              let mut my_autoscrub_desc = MyAutoScrubDesc {
//                  scrub_size: my_cache_desc.cacheline_size() * 5000,
//              };
//
//      c.  Invoke autoscrub():
//
//              let my_cache_desc = MyCacheDesc.clone();
//              let scrub = AutoScrub.autoscrub(&mut my_cache_desc,
//                  &my_scrub_areas, &my_mut autoscrub_desc)?;
//
// DETAILS
// =======
// ECCs are limited in the number of errors they can correct. These errors
// generally accumulate over time. So long as memory is read often enough,
// correction is applied with enough frequency that the number of errors
// stays within the bounds of what is correctable. However, a piece of
// memory that is rarely accessed can accumulate multiple errors. When that
// memory is eventually used, it will not be possible to determine the corrected
// value and a fatal error will result. This is where a memory scrubber comes
// in.
//
// In general, memory should be scrubbed at a rate high enough that the number
// of accumulated errors remains low enough that the probability that there are
// memory words with uncorrectable errors is extremely low. Since it isn't
// possible to predict which areas of memory are read frequently enough to
// avoid error accumulation, the usual practice is to scan all of memory. With
// modern systems, this can be quite be a large amount of work and the
// scrubbing work is broken into smaller pieces to avoid any significant
// amount of performance impact.
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
// evict the memory cache contents being used by other software on the system,
// with modified cache lines being written to memory.  When that software
// resumes, it will have to re-read all the data it wants to use. This may
// cause a substantial performance impact all at once.  This library is written
// to perform its reads all of memory corresponding to a single cache line at
// a time. If memory scrubbing is broken into smaller chunks, data will be
// evicted from only a few cache lines each time scrubbing is done, producing
// a more even performance impact.
//
// CACHE ORGANIZATION AND ADDRESSES
// ================================
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
// USAGE
// =====
// FIXME; This all has to be reviewed.
// To use this, it recommended you do the following:
//
// 1.   Determine a suitable data type to represent the size of object that
//      is used by the unit the computes the ECC. This is probably either u32
//      or u64, and we'll call it ECCData here, though you can use anything
//      appropriate to your error correction hardware.
//
// 2.   Define the structure of a cache line by implementing Cacheline for
//      the particular layout for your processor. We'll call the structure
//      MyCacheline. It usually the case that cache lines are arrays of ECCData
//      items, such as:
//
//          #[repr(C)]
//          struct MyCacheline {
//              data: [ECCData; 8];
//          }
//
//      Since many systems have more than one level of cache memory, note
//      that this should be the longest cache line in in the system.
//
// 3.   Determine the address of the first and last bytes of the memory area
//      which you want to scrub. Call these my_start and my_end. The start
//      must be a multiple of the cache line size, the end must be one less
//      than a multiple of the cache line size. If your cache has multiple ways
//      (likely), the cache line size is the number of bytes in a single way.
//
// 3.   Create a structure that holds definitions of your cache. This is
//      an implementation of the CacheDesc trait. For example purposes, call
//      this MyCacheDesc. In most cases, the default functions provide
//      everything you need, so only things you need to define are the
//      following:
//
//      a.  The cache_index_width() function, which returns the number of
//          bits in the cache index portion of an address. For example, a
//          ten-bit wide cache index would be implemented by:
//
//              fn cache_index_width(&self) -> usize {
//                  10
//              }
//
//      b.  A function that will cause all bytes in a cache line to be read
//          and checked for a correct ECC.  If the entire cache is read when
//          any element is read, this can be done with a minimal amount of
//          unsafe code:
//
//              fn read_cacheline(&self, cacheline_ptr: *const MyCacheline) {
//                  // Get a safe reference to the cache line
//                  let cacheline = unsafe {
//                      &*cacheline_ptr
//                  };
//                  // Get a reference to the first element
//                  let cacheline_data = &cacheline.data[0];
//                  // Read from the first element
//                  let _dummy = unsafe {
//                      ptr::read(cacheline_data)
//                  };
//              }
//
//          There is a conceivable architecture in which only part of the
//          longest cache line will be read when a single element is read.
//          Since any memory not read will not be checked for errors, it is
//          important that this function implement a full cache line read.
//          Check your processor's reference manual to determine how to do
//          this.
//
// 4.   Create a new MemoryScrubber:
//
//          let scrubber = match MemoryScrubber::<MyCacheline>::
//              new(&MyCacheDesc::<MyCacheline> {...}, my_start, my_end) {
//              Err(e) => ...
//
// 5.   Scrub some number of bytes. You could scrub a quarter of the memory area
//      with:
//
//          match scrubber.scrub(size / 4) {
//              Err(e) => ...
//
//      The size passed to scrub_scrub_areIa() must be a multiple of the cache
//      line size.
//
// BREAKING UP SCANS
// =================
// The decision of how to break up a single scan of all of memory depends on
// system factors such as:
// o    Is the scan preemptible?
// o    Does the scan cause a context switch and how many can be tolerated
//      in a given interval?
// o    What is the performance impact of evicting and reloading the section
//      of cache corresponding to the scrubbed memory.
//
// FREQUENCY OF SCANS
// ==================
// Start by determining the number of errors an ECC unit that operates on
// words with w bits can correct.
//
// GPT4 Query
// ----------
//  Assume a memory with S words of W bits, with the probability that a single
//  bit will be flipped in time Tf is P. What is probability that at least one
//  word will have more than N bits flipped in the interval T?
//
// Unedited answer (has not yet been verified)
// -------------------------------------------
//  The problem you're describing involves complex probabilities and involves
//  calculations related to binomial distributions. While there isn't a single
//  "mathematical function" that directly provides the answer, you can break
//  down the problem into components based on probability theory. Here's a
//  breakdown of the approach in mathematical terms:
//
//  Let:
//
//  - `S` be the number of words.
//  - `W` be the number of bits in a word.
//  - `P` be the probability that a single bit will be flipped in time Tf.
//  - `N` be the maximum number of flipped bits in a word.
//  - `k` be the number of bits flipped in a word (0 <= k <= W).
//
//  The probability that exactly `k` bits are flipped in a single word can be
//  calculated using the binomial distribution formula:
//
//  ```
//  P(k) = C(W, k) * (1 - P)^(W - k) * P^k
//  ```
//
//  Where `C(W, k)` is the binomial coefficient, given by:
//
//  ```
//  C(W, k) = W! / (k! * (W - k)!)
//  ```
//
//  The probability that a single word doesn't have more than `N` bits flipped
//  is the sum of probabilities for `k` from 0 to `N`:
//
//  ```
//  P_single_word = Σ(P(k)) for k = 0 to N
//  ```
//
//  Finally, the probability that at least one word has more than `N` bits
//  flipped in the interval T can be calculated using the complement rule:
//
//  ```
//  P_at_least_one_word = 1 - (1 - P_single_word)^S
//  ```
//
//  Keep in mind that these formulas involve factorials and exponentials, which
//  can lead to large computations for larger values of `S`, `W`, and `N`. You
//  might need to use specialized libraries or numerical approximations if you
//  intend to calculate these probabilities for significant values of these
//  parameters.
//
// ChatGPT Result For Determination Of The Time Until The First Word Goes Bad
// --------------------------------------------------------------------------
//  To compute the time interval for a given `P_at_least_one_word`, you would
//  need to rearrange the formula for `P_at_least_one_word` to solve for the
//  time interval `T`:
//
//  ```
//  P_at_least_one_word = 1 - (1 - P_single_word)^S
//  ```
//
//  Let's rearrange the formula:
//
//  ```
//  1 - P_at_least_one_word = (1 - P_single_word)^S
//  ```
//
//  ```
//  T = (-ln(1 - P_at_least_one_word)) / ln(1 - P_single_word)
//  ```
//
//  In this formula, `ln` represents the natural logarithm function. You can
//  use Rust's `f64` math functions to compute this.
//
//  Here's how you can implement it in Rust:
//
//  ```rust
//  fn compute_time_interval(P_at_least_one_word: f64, P_single_word: f64, S: u32) -> f64 {
//      let numerator = -(1.0 - P_at_least_one_word).ln();
//      let denominator = (1.0 - P_single_word).ln();
//
//      numerator / (denominator * f64::from(S))
//  }
//
//  fn main() {
//      let P_at_least_one_word = 0.9; // Desired probability
//      let P_single_word = 0.01; // Probability for a single word
//      let S = 10; // Number of words
//
//      let time_interval = compute_time_interval(P_at_least_one_word, P_single_word, S);
//      println!("Time Interval: {}", time_interval);
//  }
//  ```
//
//  Replace the values of `P_at_least_one_word`, `P_single_word`, and `S` with
//  your specific values. Keep in mind that these calculations might not be
//  feasible for very small values of `P_at_least_one_word` or `P_single_word`
//  due to the precision of floating-point arithmetic. Additionally, the `ln`
//  function might return NaN (Not-a-Number) for certain inputs, so you should
//  handle potential edge cases in your code.
//
// So, that determines how often the entire memory must be scanned. Note that
// above, S is the number of words of memory. The total number of 8-bit bytes in
// memory is S * (W / 8).
//
// NOTE: The above assumes that, once a bit is flipped, it stays flipped. The
// probability of a bit being inverted, then inverted again is small enough
// that it can be ignored.

//#[macro_use]
extern crate lazy_static;
extern crate num_traits;

//use std::cell::RefCell;
use std::fmt;
use std::iter;
use std::marker::PhantomData;
//use std::rc::Rc;
use std::slice;

// Some basic definition

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// Structure used to define an area to be scrubbed
// start - lowest address of the area. Must be a multiple of the cache line size
// end - address of the last byte of the area. Must be one less than a multiple
//      of the cache line size
#[derive(Clone, Debug)]
#[repr(C)]
pub struct MemArea {
    pub start:              *const u8,
    pub end:                *const u8,
}

impl MemArea {
    pub fn new(start: *const u8, end: *const u8) -> MemArea {
        MemArea { start: start, end: end }
    }
}

// Definition of the cache line layout
trait CachelineData {
}

// Base cache line definitions. Cache lines must know their width in bytes.
pub trait CachelineBase<'a> {
}

// Standard cache line definition. It is given a data type which defines the
// layout of the actual cacheline. This is typically an array of items where
// the items are the size of data that the ECC unit works on. The cache line
// size must be a power of two
pub trait Cacheline<'a, D> {
}

// Describe cache parameters and pull in all elements of the cache line.
pub trait CacheDescBase {
    // Return the number of bits required to hold an index into the bytes of
    // a cache line. So, if you have an eight-byte cache line (unlikely), this
    // would return 3. Note that this must return a power of two.
    fn cacheline_width(&self) -> usize;

    // This function is given a pointer to a cache line-aligned address with
    // as many bytes as are in a cache line. The implementation should do
    // whatever is necessary to ensure all bytes are read in order to trigger
    // a fault if any bits have an unexpected value. So long as the number
    // of bad bits is small enough (ECC-dependent), corrected data should
    // be written back to that location, preventing the accumulation of so many
    // bad bits that the correct value cannot be determined.
    fn read_cacheline<'a>(&self, cacheline_ptr: *const dyn CachelineBase<'a>);

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

    // NOTE: You are unlikely to ever need to implement this
    // Return the size of a MemArea in cachelines
    fn size_in_cachelines(&self, scrub_area: &MemArea) -> usize {
        let start_in_cachelines =
            scrub_area.start as usize >> self.cacheline_width();
        // This will truncate the number of cache lines by one
        let end_in_cachelines =
            scrub_area.end as usize >> self.cacheline_width();
        (end_in_cachelines - start_in_cachelines) + 1
    }

    // NOTE: You are unlikely to ever need to implement this
    // Extract the cache index part of the address
    fn cache_index(&self, p: *const u8) -> usize {
        (p as usize >> self.cacheline_width()) &
            ((1 << self.cache_index_width()) - 1)
    }

    // NOTE: You are unlikely to ever need to implement this
    // Computes the offset, in cache lines, of the next address at or higher
    // than the given pointer for an address hitting the given cache index.
    //
    // p:       Address to start at
    // index:   Cache index to search for
    fn first_offset_for_index(&self, p: *const u8, index: usize) -> usize {
        let start_index = self.cache_index(p);
        let cache_lines = self.cache_lines();

        // Compute the offset from the start of the self.scrub_area to the
        // next highest address whose cache index is the one we are currently
        // scrubbing. 
        if index >= start_index { index - start_index }
        else { (index + cache_lines) - start_index }
    }
}

// Describe cache parameters and pull in all elements of the cache line, using
// a definition of the cache line. This enhances the basic CacheDescBase trait
// by taking a Cacheline generic and using its size
pub trait CacheDesc<'a, CL: Cacheline<'a, D> + ?Sized, D>: CacheDescBase {
    // NOTE: You are unlikely to ever need to implement this
    fn cacheline_width(&self) -> usize where Self: Sized {
        let cacheline_size = std::mem::size_of::<D>();
        let leading_zeros = cacheline_size.leading_zeros() as usize;
        let w = usize::BITS as usize - 1 - leading_zeros;
        assert_eq!(cacheline_size, 1 << w);
        w
    }
}

// C interfaces pieces

#[derive(Clone)]
#[repr(C)]
pub struct CCacheDesc {
    cacheline_width: usize,

    // NOTE: You are unlikely to ever need to implement this
    // Return the number of bytes in the cache line. A cache line width of 4
    // bits will have a cache line size of 16 bytes.
    c_cacheline_size: extern "C" fn(me: *const CCacheDesc) -> usize,

    // Return the number of bits used to index into the cache, i.e. the index
    // of a cache line in the cache. A cache with 1024 lines will have an
    // index using 10 bits.
    c_cache_index_width: extern "C" fn(me: *const CCacheDesc) -> usize,

    // This function is given a pointer to a cache line-aligned address with
    // as many bytes as are in a cache line. The implementation should do
    // whatever is necessary to ensure all bytes are read in order to trigger
    // a fault if any bits have an unexpected value. So long as the number
    // of bad bits is small enough (ECC-dependent), corrected data should
    // be written back to that location, preventing the accumulation of so many
    // bad bits that the correct value cannot be determined.
    c_read_cacheline: extern "C" fn(me: *const CCacheDesc,
        cacheline_ptr: *const dyn CachelineBase),

    // Return the size of a MemArea in cachelines
    c_size_in_cachelines: extern "C" fn(me: *const CCacheDesc,
        scrub_area: &MemArea) -> usize,

    // Returns the cache index part of the address
    c_cache_index: extern "C" fn(me: *const CCacheDesc,
        p: *const u8) -> usize,
}

impl CCacheDesc {
    pub fn new(cacheline_width: usize,
        c_cacheline_size: extern "C" fn(me: *const CCacheDesc) -> usize,
        c_cache_index_width: extern "C" fn(me: *const CCacheDesc) -> usize,
        c_read_cacheline: extern "C" fn(me: *const CCacheDesc,
            cacheline_ptr: *const dyn CachelineBase),
        c_size_in_cachelines: extern "C" fn(me: *const CCacheDesc,
            scrub_area: &MemArea) -> usize,
        c_cache_index: extern "C" fn(me: *const CCacheDesc,
            p: *const u8) -> usize) -> CCacheDesc {
        CCacheDesc {
            cacheline_width: cacheline_width,
            c_cacheline_size: c_cacheline_size,
            c_cache_index_width: c_cache_index_width,
            c_read_cacheline: c_read_cacheline,
            c_size_in_cachelines: c_size_in_cachelines,
            c_cache_index: c_cache_index,
        }
    }
}

impl CacheDescBase for CCacheDesc {
    fn cacheline_width(&self) -> usize {
        self.cacheline_width
    }

    fn cacheline_size(&self) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        1 << (self.c_cacheline_size)(self_ptr)
    }

    fn cache_index_width(&self) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        (self.c_cache_index_width)(self_ptr)
    }

    fn read_cacheline(&self, cacheline_ptr: *const dyn CachelineBase) {
        let self_ptr: *const CCacheDesc = self as *const _;
        let cacheline_ptr = cacheline_ptr as *const dyn CachelineBase;
        (self.c_read_cacheline)(self_ptr, cacheline_ptr)
    }

    fn size_in_cachelines(&self, scrub_area: &MemArea) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        (self.c_size_in_cachelines)(self_ptr, scrub_area)
    }
}

impl<'a, CL: Cacheline<'a, D>, D> CacheDesc<'a, CL, D> for CCacheDesc {
}

impl<'a, D> Cacheline<'a, D> for CCacheDesc {
}

// This is the C interface to autoscrub functionality
pub struct CCacheline {
}

impl<'a, D> Cacheline<'a, D> for CCacheline {
}

impl<'a> CachelineBase<'a> for CCacheline {
}

#[repr(C)]
pub struct CAutoScrubDesc<'a, CD, CL> {
    me:         *mut CAutoScrubDesc<'a, CD, CL>,
    c_next:     extern "C" fn(me: *mut CAutoScrubDesc<'a, CD, CL>) -> usize,
}

impl<'a, CD: CacheDescBase, CL: CachelineBase<'a>>
AutoScrubDesc<CD, CL> for CAutoScrubDesc<'a, CD, CL> {
    fn next(&mut self) -> usize {
        (self.c_next)(self.me)
    }
}

#[repr(C)]
pub struct CAutoScrubResult {
    is_err:  bool,
    error:  Error,
}

// Autoscrub function interface for C
//    scrub_areas_ptr: *const MemArea, n_scrub_areas: usize,
//    c_auto_scrub_desc: &'a mut dyn AutoScrubDesc) -> AutoScrubResult
#[no_mangle] // Can't use generics if using #[no_mangle]
pub extern "C" fn autoscrub<'a, D>(c_cache_desc: &'a mut CCacheDesc,
    scrub_areas_ptr: *const MemArea, n_scrub_areas: usize,
    c_auto_scrub_desc: &'a mut CAutoScrubDesc<CCacheDesc, CCacheline>) ->
        CAutoScrubResult {
    let scrub_result: Result<(), Error>;

    {
        let scrub_areas_slice = unsafe {
            // It looks like the slice constructed with from_raw_parts is
            // never freed. This is the behavior we want when we get a pointer
            // from C/C++.
            slice::from_raw_parts(scrub_areas_ptr, n_scrub_areas)
        };

        scrub_result = AutoScrub::<'a, CCacheDesc, CCacheline, D>
            ::autoscrub(c_cache_desc, &scrub_areas_slice, c_auto_scrub_desc);
    }

    match scrub_result {
        Err(e) => CAutoScrubResult { is_err: true, error: e },
        Ok(_) => CAutoScrubResult { is_err: false, error: Error::InternalError },
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
pub enum Error {
    InternalError,
    UnalignedStart,
    UnalignedEnd,
    UnalignedSize,
    NoMemAreas,
    EmptyMemArea,
    ZeroSize,
    IteratorFailed,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait AutoScrubDesc<CD, CL> {
    fn next(&mut self) -> usize;
}

struct AutoScrub<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> {
    scrubber:   MemoryScrubber<'a, CD, CL, D>,
    desc:       &'a mut dyn AutoScrubDesc<CD, CL>,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<CL>. So, this
    // works around that problem
    _marker:    PhantomData<CL>,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: 'a + Cacheline<'a, D> + CachelineBase<'a>, D> AutoScrub<'a, CD, CL, D> {
    fn new(cache_desc_in: &'a CD, scrub_areas: &'a [MemArea],
        desc: &'a mut dyn AutoScrubDesc<CD, CL>) ->
        Result<AutoScrub<'a, CD, CL, D>, Error> {
        let scrubber = match MemoryScrubber::<'a, CD, CL, D>
            ::new(cache_desc_in, scrub_areas) {
            Err(e) => return Err(e),
            Ok(scrubber) => scrubber,
        };

        Ok(AutoScrub {
            scrubber: scrubber,
            desc: desc,
            _marker:    PhantomData,
        })
    }

    fn scrub(&mut self) -> Result<(), Error> {
        loop {
            let n = self.desc.next();
            if n == 0 {
                return Ok(());
            }
            self.scrubber.scrub(n)?;
        }
    }

    pub fn autoscrub(cache_desc: &'a mut CD, scrub_areas: &'a [MemArea],
            desc: &'a mut dyn AutoScrubDesc<CD, CL>) -> Result<(), Error> {
        let mut autoscrub = Self::new(cache_desc, scrub_areas, desc)?;
        autoscrub.scrub()
    }
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D> + 'a, D>
    Cacheline<'a, D>
    for AutoScrub<'a, CD, CL, D> {
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D> + 'a, D>
    CacheDesc<'a, CL, D>
    for AutoScrub<'a, CD, CL, D> {
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D> + 'a, D>
    CacheDescBase
    for AutoScrub<'a, CD, CL, D> {
    fn cacheline_width(&self) -> usize {
        unimplemented!();
    }
    fn cache_index_width(&self) -> usize {
        unimplemented!();
    }
    fn read_cacheline<'b>(&self, _cacheline_ptr: *const dyn CachelineBase<'b>) {
        unimplemented!();
    }
}

// Memory scrubber
pub trait MemoryScrubberBase<'a, CD: CacheDescBase, CL: CachelineBase<'a>> {
}

// cache_desc - Description of the cache
// iterator - MemoryScrubberIterator used to walk through the memory being
//      scrubbed
// cur_index:   Current cache line index
pub struct MemoryScrubber<'a, CD, CL: ?Sized, D>
/*
where
    CD: CacheDesc<'a, CL, D>,
    CL: Cacheline<'a, D> + ?Sized,
    D,
*/
{
    cache_desc:     &'a CD,
    iterator:       ScrubLinesIterator<'a, CD, CL, D>,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<CL>. So, this
    // works around that problem
    _marker:    PhantomData<D>,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: 'a + Cacheline<'a, D> + CachelineBase<'a>, D>
MemoryScrubber<'a, CD, CL, D>
{
    // Create a new memory scrubber
    // cache_desc_in - Description of the cache
    // start - Virtual address of memory being scrubbed
    // end - Virtual address of the last byte of memory to be scrubbed
    pub fn new(cache_desc: &'a CD, scrub_areas: &'a [MemArea]) ->
        Result<MemoryScrubber<'a, CD, CL, D>, Error> {
        if scrub_areas.len() == 0 {
            return Err(Error::NoMemAreas);
        }

        let cacheline_size = {
            cache_desc.cacheline_size()
        };

        // Check each MemArea for errors
        for scrub_area in scrub_areas {
            // The code will actually handle this just fine, but it's extra
            // effort to no benefit, so it is expected to be a user error.
            if scrub_area.start == scrub_area.end {
                return Err(Error::EmptyMemArea);
            }

            let start_addr = scrub_area.start as Addr;
            let end_addr = scrub_area.end as Addr;

            if (start_addr & (cacheline_size - 1)) != 0 {
                return Err(Error::UnalignedStart);
            }

            if (end_addr & (cacheline_size - 1)) != cacheline_size - 1 {
                return Err(Error::UnalignedEnd);
            }
        }

        let iterator = ScrubLinesIterator::new(cache_desc, scrub_areas)?;

        let scrubber = MemoryScrubber::<'a, CD, CL, D> {
            cache_desc:     cache_desc,
            iterator:       iterator,
            _marker:        PhantomData,
        };

        Ok(scrubber)
    }

    // Scrub some number of bytes. This could be larger than the total memory
    // area, in which case the scrubbing will start again at the beginning
    // of the memory area, but it seems unlikely that this would be useful.
    // n - Number of bytes to scrub
    pub fn scrub(&mut self, n: usize) -> Result<(), Error> {
        let cacheline_width = {
            CacheDesc::<'a, CL, D>::cacheline_width(self.cache_desc)
        };

        let cacheline_size = {
            self.cache_desc.cacheline_size()
        };

        if (n & (cacheline_size - 1)) != 0 {
            return Err(Error::UnalignedSize);
        }

        // Convert to the number of cachelines to scrub
        let n_scrublines = n >> cacheline_width;

        // At this point, it's pretty much Iterators all the way down.
        for _i in 0..n_scrublines {
            let _p = match self.iterator.next() {
                None => panic!("ScrubLinesIterator return None"),
                Some(p) => p,
            };
/*
            self.cache_desc.read_cacheline(p);
*/
        }

        Ok(())
    }
}

// Walks through cache line-sized items. Never reaches an end, that is,
// next() never returns None
pub struct ScrubLinesIterator<'a, CD, CL: ?Sized, D> {
    cache_desc:     &'a CD,
    scrub_areas:    &'a [MemArea],
    iterator:       CacheIndexIterator<'a, CD, CL, D>,
    _marker:        PhantomData<D>,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> ScrubLinesIterator<'a, CD, CL, D> {
    pub fn new(cache_desc: &'a CD,
        scrub_areas: &'a [MemArea]) ->
        Result<ScrubLinesIterator<'a, CD, CL, D>, Error> {
        let iterator = CacheIndexIterator::<CD, CL, D>
            ::new(cache_desc, scrub_areas)?;

        Ok(ScrubLinesIterator {
            cache_desc:     cache_desc,
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            _marker:        PhantomData,
        })
    }
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> iter::Iterator for
    ScrubLinesIterator<'a, CD, CL, D> {
    type Item = *const CL;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.iterator.next();

            if next.is_some() {
                return next;
            }

            self.iterator = match CacheIndexIterator::<CD, CL, D>
                ::new(self.cache_desc.clone(), &self.scrub_areas) {
                Err(e) => panic!("CacheIndexIterator failed: {}", e),
                Ok(iterator) => iterator,
            };
        }
    }
}

// This goes through all cache indices. The current cache index is kept at a
// higher level to ensure continuity. That is, the previous cache index is
// passed down to us and incremented here so that we don't restart the indices
// each time this is called.
//
// cache_desc:  Cache descriptor
// scrub_areas: List of memory areas to be scrubbed
// iterator:    An iterator for a scrubbing a single memory area
// cur_index:   The index of the cache line we are scrubbing
pub struct CacheIndexIterator<'a, CD, CL: ?Sized, D> {
    cache_desc:     &'a CD,
    scrub_areas:    &'a [MemArea],
    iterator:       MemAreasIterator<'a, CD, CL, D>,
    cur_index:      usize,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> CacheIndexIterator<'a, CD, CL, D> {
    pub fn new(cache_desc: &'a CD,
        scrub_areas: &'a [MemArea]) ->
        Result<CacheIndexIterator<'a, CD, CL, D>, Error> {
        let cur_index = 0;
        let iterator = MemAreasIterator::<CD, CL, D>::new(cache_desc,
            &scrub_areas, cur_index)?;

        Ok(CacheIndexIterator {
            cache_desc:     cache_desc,
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            cur_index:      cur_index,
        })
    }
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> iter::Iterator for
    CacheIndexIterator<'a, CD, CL, D> {
    type Item = *const CL;

    fn next(&mut self) -> Option<Self::Item> {
        let cache_index_width = self.cache_desc.cache_index_width();
        let cache_lines = 1 << cache_index_width;

        loop {
            let next = self.iterator.next();

            if let Some(p) = next {
                return Some(p);
            }

            self.cur_index += 1;
 
            if self.cur_index == cache_lines {
                return None;
            }

            self.iterator = match MemAreasIterator::<CD, CL, D>
                ::new(self.cache_desc,
                &self.scrub_areas, self.cur_index) {
                Err(e) => panic!("MemAreasIterator::new failed: {}", e),
                Ok(iterator) => iterator,
            };
        }
    }
}

// This iterator goes through all defined scrub areas
// cache_desc:  Cache descriptor
// scrub_areas: List of memory areas to be scrubbed
// iterator:    An iterator for a scrubbing a single memory area
// i:           The index into the current MemArea
pub struct MemAreasIterator<'a, CD, CL: ?Sized, D> {

    cache_desc:     &'a CD,
    scrub_areas:    &'a [MemArea],
    iterator:       MemAreaIterator<'a, CD, CL, D>,
    i:              usize,
    cur_index:      usize,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> MemAreasIterator<'a, CD, CL, D> {
    pub fn new(cache_desc: &'a CD,
        scrub_areas: &'a [MemArea], cur_index: usize) ->
        Result<MemAreasIterator<'a, CD, CL, D>, Error> {
        if scrub_areas.len() == 0 {
            return Err(Error::NoMemAreas);
        }

        let iterator = MemAreaIterator::<CD, CL, D>
            ::new(cache_desc, &scrub_areas[0], cur_index)?;

        Ok(MemAreasIterator {
            cache_desc:     cache_desc,
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            i:              0,
            cur_index:      cur_index,
        })
    }
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> iter::Iterator for
    MemAreasIterator<'a, CD, CL, D> {
    type Item = *const CL;

    // Loop through all scrub areas
    fn next(&mut self) -> Option<Self::Item> {
        assert_ne!(self.scrub_areas.len(), 0);

        // Assuming not zero length
        loop {
//println!("MemAreasIterator::next: looking at scrub area {}", self.i);
            let next = self.iterator.next();

            if let Some(p) = next {
                return Some(p);
            }

            self.i += 1;
//println!("MemAreasIterator: bump scrub index to {}/{}", self.i, self.scrub_areas.len());
            if self.i == self.scrub_areas.len() {
                return None;
            }

            self.iterator = match MemAreaIterator::<CD, CL, D>
                ::new(self.cache_desc.clone(), &self.scrub_areas[self.i],
                self.cur_index) {
                Err(e) => panic!("MemAreaIterator failed: {}", e),
                Ok(iterator) => iterator,
            };
        }
    }
}

// MemAreaIterator to scan a MemArea, keeping on a single cache line as
// long as possible. This means that we increment the addresses we can by
// the cache size each time to keep the cache index constant.
//
// cache_desc   Description of the cache
// scrub_area:  Specifies the address of the scrub area
// i:           Number of cache line-sized items we've scanned in this
//              MemArea
// cur_index:   Cache index we are scrubbing
pub struct MemAreaIterator<'a, CD, CL: ?Sized, D> {
    cache_desc: &'a CD,
    scrub_area: &'a MemArea,
    i:          usize,
    cur_index:  usize,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<CL>. So, this
    // works around that problem
    _marker1:    PhantomData<CL>,
    _marker2:    PhantomData<D>,
}

impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> MemAreaIterator<'a, CD, CL, D> {
    // Create a new MemAreaIterator.
    // cache_desc: Description of the cache
    // scrub_area: Memory over which we Iterate
    // i:           Current element in scrub_area
    // cur_index:   Cache index we're looking for
    //
    // Returns: Ok(MemAreaIterator) on success, Err(Error) on failure
    pub fn new(cache_desc: &'a CD,
        scrub_area: &'a MemArea, cur_index: usize) ->
        Result<MemAreaIterator<'a, CD, CL, D>, Error> {
        if scrub_area.start == scrub_area.end {
            return Err(Error::NoMemAreas);
        }

        Ok(MemAreaIterator {
            cache_desc: cache_desc,
            scrub_area: scrub_area,
            i:          0,
            cur_index:  cur_index,
            _marker1:    PhantomData,
            _marker2:    PhantomData,
        })
    }
}

// Return a pointer into the next memory area of cache line size
impl<'a, CD: CacheDesc<'a, CL, D>, CL: Cacheline<'a, D>, D> iter::Iterator for
    MemAreaIterator<'a, CD, CL, D> {
    type Item = *const CL;

    fn next(&mut self) -> Option<Self::Item> {
        let cache_index_width = self.cache_desc.cache_index_width();

        let first_offset =
            self.cache_desc.first_offset_for_index(self.scrub_area.start,
                self.cur_index);

        // Add multiples of the number of cache lines in the cache to get to
        // the offset with the same cache index.
        let cur_offset = first_offset + (self.i << cache_index_width);

        // If this offset is greater than or equal to the number of cache
        // lines in the current scrub area, we're done.
        let size_in_cachelines = self.cache_desc
            .size_in_cachelines(&self.scrub_area);
        if cur_offset >= size_in_cachelines {
            return None;
        }

        let cacheline_width =
            CacheDesc::<'a, CL, D>::cacheline_width(self.cache_desc);
        self.i += 1;
        let _p = self.scrub_area.start as usize +
            (cur_offset << cacheline_width);
        return Some(_p as *const CL)
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;

    use num_traits::Num;

    use std::cell::RefCell;
    use std::ops::Index;
    use std::marker::PhantomData;
    use std::ptr;
    use std::slice;
    use std::time::Instant;

    use crate::{Addr, AutoScrub, AutoScrubDesc, CacheDesc, CacheDescBase,
        CacheIndexIterator, Cacheline, CachelineBase, CachelineData, Error,
        MemoryScrubber, MemArea, MemAreaIterator, MemAreasIterator,
        ScrubLinesIterator};

    // Type used for the read counter
    type NRead = u8;

    // We need to build up the structure of the cache line. Now, this isn't
    // so much the cache line, per se, but rather the structure of a cache
    // line overlaid on memory, with the alignment implementd by the cache line.
    // We assume a structure that is pretty standard: each cache line is an
    // array of data items of some size. For the purposes of this code, the
    // data item size is the size used by the ECC unit to compute the ECC.
    //
    // The definitions we use are:
    //
    // Item                           Type   Abbr    Description
    // ------------------------------ ------ ---- ---------------------------
    // num_traits::Num                u8-u64 D    Data items making up a
    //                                            cache line
    // usize                          usize  N    Number of D items in a cache
    //                                            line
    // TestCacheDataIndexed<D, N>     struct DI   Data part of the cache where
    //                                            each element is indexed by
    //                                            an integer value
    // TestCachelineBase<DI>          trait  CL   Entire data part of the cache
    //                                            line
    trait TestCachelineBase<DI: ?Sized> {
        fn read_cacheline(&self, cacheline_ptr: *const DI);
        fn write_cacheline(&self, cacheline_ptr: *mut DI);
        fn verify_cacheline(&self, cacheline_ptr: *const DI);
    }

    struct TestCacheline<DI: ?Sized> {
        _marker1:   PhantomData<DI>
    }

    impl<DI> TestCacheline<DI> {
        fn new() -> TestCacheline<DI> {
            TestCacheline::<DI> {
                _marker1:   PhantomData,
            }
        }
    }

    impl<DI> TestCachelineBase<DI> for TestCacheline<DI> {
        fn read_cacheline(&self, cacheline_ptr: *const DI) {
            unimplemented!();
        }
        fn write_cacheline(&self, cacheline_ptr: *mut DI) {
            unimplemented!();
        }
        fn verify_cacheline(&self, cacheline_ptr: *const DI) {
            unimplemented!();
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl<DI: ?Sized>
    Sync for TestCacheline<DI>
    {}

    trait TestCacheDataIndexedBase<D: Num, const N: usize> {
    }

    struct TestCacheDataIndexed<D: Num, const N: usize> {
        data: [D; N],
    }

    impl<D: Num, const N: usize>
    TestCacheDataIndexed<D, N> {
        fn new() -> Self {
            unimplemented!();
        }
    }

    impl<D: Num, const N: usize>
    TestCacheDataIndexedBase<D, N> for TestCacheDataIndexed<D, N> {
    }

/*
    impl<D: Num + std::marker::Copy, const N: usize>
    TestCacheDataIndexed<D, N> {
        fn new() -> TestCacheDataIndexed::<D, N> {
            TestCacheDataIndexed::<D, N> {
                data:   [D; N],
            }
        }

        // Return a reference to the ith element of the indexed data part
        // of the memory to which the cache line is mapped
        fn index_ref(&self, cacheline_ptr: *const D, i: usize) -> &D {
            &self.data[0]
        }
        fn read_one(&self, cacheline_ptr: *const D) {
            let cacheline_read_ptr = self.index_ref(cacheline_ptr, 0)
                as *const D;
            unsafe {
                ptr::read(cacheline_read_ptr);
            };
        }
    }
*/

/*
    // FIXME: Should I be using CachelineData and making these all structs?
    type TestCacheDataTiny = [u16; 2];
    type TestCacheDataSmall = [u32; 4];
    type TestCacheData32 = [u32; 8];

//    type TestCacheData64 = [u64; 16];
*/
    trait TestCacheDescBase<'a, CL:TestCachelineBase<D>, D>: Test<'a, CL, D> {
    }

    // TestCacheDesc - Description of the cache for basic tests
    // cache_index_width - Number of times this cacheline was iit during the
    //      scrub
    // read_infos:          Array of ReadInfo items>
    #[derive(Debug)]
    struct TestCacheDesc<CL: ?Sized, DI: ?Sized> {
        cache_index_width:  usize,
        // FIXME: Remove when possible. Right now, the compiler doesn't appear
        // to know that U is actually used when it's in CacheDesc<CL>. So, this
        // works around that problem
        _marker1:    PhantomData<CL>,
        _marker2:    PhantomData<DI>,
    }

    impl<'a, CL: TestCachelineBase<DI>, DI: ?Sized> TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI>,
        {
        // Set up a new TestCacheDesc
        // sizes: array of sizes of memory areas
        fn new(sizes: &[usize]) -> Self {
            let test_cache_desc = Self {
                cache_index_width:  std::mem::size_of::<CL>(),
                _marker1:            PhantomData,
                _marker2:            PhantomData,
            };

/*
            let tester = Tester::new(&test_cache_desc, sizes);
            test_cache_desc.test = Some(RefCell::new(tester));
*/

            test_cache_desc
        }
        fn cacheline_width(&self) -> usize {
            std::mem::size_of::<CL>()
        }
    }

    impl<'a, CL, DI>
    Test<'a, CL, DI> for TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI>,
    {
        fn test(&mut self, cacheline_ptr: *const CL) {
            unimplemented!();
        }
    }

    impl<'a, CL, DI>
    CacheDesc<'a, CL, DI> for TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI> + Cacheline<'a, DI>,
    {
    }

    impl<'a, CL, DI>
    CacheDescBase for TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI>,
    {
        fn cacheline_width(&self) -> usize {
            unimplemented!();
        }
        fn cache_index_width(&self) -> usize {
            unimplemented!();
        }
        fn read_cacheline(&self, cacheline_ptr: *const dyn CachelineBase) {
            unimplemented!();
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl<'a, CL, DI: ?Sized>
    Sync for TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI>,
    {}

    impl<'a, CL, DI>
    Clone for TestCacheDesc<CL, DI>
    where
        CL: TestCachelineBase<DI>,
    {
        fn clone(&self) -> Self {
            TestCacheDesc {
                cache_index_width:  self.cache_index_width,
                _marker1:            PhantomData,
                _marker2:            PhantomData,
            }
        }
    }

    trait Test<'a, CL: TestCachelineBase<D>, D> {
        fn test(&mut self, cacheline_ptr: *const CL) { unimplemented!(); }
    }

    #[derive(Debug)]
    struct Tester<'a, CL, DI>
    where
        CL: TestCachelineBase<DI>
    {
        cache_desc: &'a TestCacheDesc<CL, DI>,
// FIXME: can this be a reference to a slice?
        read_infos: Vec<ReadInfo>,
        _marker1:   PhantomData<CL>,
        _marker2:   PhantomData<DI>,
    }

    impl<'a, CL, D>
    Tester<'a, CL, D>
    where
        CL: TestCachelineBase<D>,
    {
        pub fn new(cache_desc: &'a TestCacheDesc<CL, D>,
            sizes: &'a [usize]) -> Tester<'a, CL, D> {
            let mut read_infos = Vec::<ReadInfo>::new();

            // Allocate memory and associated data structures
            for size in sizes {
                let mem = match Mem::new::<CL>(*size) {
                    Err(e) => panic!("Memory allocation error: {}", e),
                    Ok(mem) => mem,
                };

                let scrub_area_size = cache_desc
                    .size_in_cachelines(&mem.scrub_area);
                let n_reads_size = GUARD_LINES + scrub_area_size + GUARD_LINES;
                read_infos.push(ReadInfo::new(n_reads_size, mem));
            }
            Tester {
                cache_desc: &cache_desc,
                read_infos: read_infos,
                _marker1:   PhantomData,
                _marker2:   PhantomData,
            }
        }

        pub fn test(&mut self, cacheline_ptr: *const CL) {
            self.set_cookie(cacheline_ptr);
            self.increment_n_reads(cacheline_ptr);
        }

        fn set_cookie(&mut self, cacheline_ptr: *const CL) {
            unimplemented!();
        }

        fn increment_n_reads(&mut self, cacheline_ptr: *const CL) {
            // Update the read count
            let index = {
                self.read_index(self.cache_desc, cacheline_ptr)
            };
            let n_reads = {
                self.get_n_reads(cacheline_ptr)
            };

            n_reads[GUARD_LINES + index] += 1;
println!("n_reads[{}] became {}", index, n_reads[GUARD_LINES + index]);
        }

        // Compute the index into the n_read array for this address. This
        // array has GUARD_LINES elements surrounding the actual counts.
        // cacheline_ptr: Pointer to the address
        fn read_index(&mut self, cache_desc: &'a TestCacheDesc<CL, D>,
            cacheline_ptr: *const CL) -> usize {
            let cacheline_addr = cacheline_ptr as Addr;
            let cacheline_size = {
                cache_desc.cacheline_size()
            };

            let read_info = self.find_read_info(cacheline_ptr);
            let scrub_area = read_info.mem.scrub_area.clone();
            let n_n_reads = cache_desc.size_in_cachelines(&scrub_area);
            let start_addr = scrub_area.start as Addr;

            let index = (cacheline_addr - start_addr) / cacheline_size;
            assert!(index < n_n_reads);
            index
        }

        // Returns a reference to n_reads[], the array of count read counts
        fn get_n_reads(&mut self, cacheline_ptr: *const CL) ->
            & mut Vec<NRead> {
            let read_info = self.find_read_info(cacheline_ptr);
            read_info.n_reads.as_mut().unwrap()
        }

        // Find a ReadInfo corresponding to a given location in memory
        fn find_read_info(&mut self, cacheline_ptr: *const CL) ->
            &mut ReadInfo {
            let cacheline_addr = cacheline_ptr as Addr;
            
            for search_read_info in &mut self.read_infos.iter_mut() {
                let scrub_area = &search_read_info.mem.scrub_area;
                let start_addr = scrub_area.start as Addr;
                let end_addr = scrub_area.end as Addr;

                if cacheline_addr >= start_addr &&
                    cacheline_addr <= end_addr {
                    return search_read_info;
                }
            }

            // If we failed, it's because the cache addess wasn't in any of
            // the MemAreas.
            panic!("Unable to find address {:x}", cacheline_addr);
        }
    }

    impl<'a, CL, D>
    Clone for Tester<'a, CL, D>
    where
        CL: TestCachelineBase<D>,
    {
        fn clone(&self) -> Tester<'a, CL, D> {
            let read_infos = self.read_infos.clone();
            Tester {
                cache_desc: &self.cache_desc,
                read_infos: read_infos,
                _marker1:   PhantomData,
                _marker2:   PhantomData,
            }
        }
    }

    // Cache characteristics
    // BASIC_CACHELINE_WIDTH - number of bits required to index a byte in a
    //      cache line
    // BASIC_CACHE_INDEX_WIDTH - number of bits used as a cache line index in
    //      the cache
    // BASIC_MEM_SIZE - Cache size, in bytes
    const BASIC_ECCDATA_WIDTH: usize = usize::BITS as usize - 1 -
        std::mem::size_of::<BasicECCData>() .leading_zeros() as usize;
    const BASIC_CACHELINE_WIDTH: usize = 6 + BASIC_ECCDATA_WIDTH;
    const BASIC_CACHE_INDEX_WIDTH: usize = 10;
    const BASIC_MEM_SIZE: usize =
        1 << (BASIC_CACHELINE_WIDTH + BASIC_CACHE_INDEX_WIDTH);

    // ECCData - The data size used to compute the ECC for basic tests
    // BasicCacheline - the data type of a cache line
    type BasicECCData = u64;

/*
    #[repr(C)]
    struct BasicCacheline {
        data:   [BasicECCData;
            (1 << BASIC_CACHELINE_WIDTH) / std::mem::size_of::<BasicECCData>()],
    }

    impl<'a, D> Cacheline<'a, D> for BasicCacheline {
    }

    impl<'a, CL: TestCachelineBase<'a, D>, D: TestCacheData<D> + CachelineData + Index<usize>>
        CacheDesc<'a, CL, D>
        for BasicCacheDesc<'a, CL, D> {
        fn cacheline_width(&self) -> usize {
            unimplemented!();
        }
    }

    impl<'a, CL: TestCachelineBase<'a, D>, D: TestCacheData<D> + CachelineData + Index<usize>>
        CacheDescBase for BasicCacheDesc<'a, CL, D> {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }
        fn cacheline_width(&self) -> usize {
            unimplemented!();
        }
        fn read_cacheline<'b>(&self,
            cacheline_ptr: *const dyn CachelineBase<'b>) {
            unimplemented!();
        }
    }
*/
    // FIXME: can these be combined?
    lazy_static! {
        static ref BASIC_CACHE_DATA:
            TestCacheDataIndexed::<u32, 8> = {
            TestCacheDataIndexed::<u32, 8>::new()
        };
    }

    lazy_static! {
        // Cache descriptor to pass to the memory scrubbing functions.
        static ref BASIC_CACHE_DESC:
            TestCacheDesc::<&'static dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>,
            TestCacheDataIndexed<u32, 8>> = {
            TestCacheDesc::<&'static dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>,
            TestCacheDataIndexed<u32, 8>>::new(&[123])
        };
    }

// FIXME: move this
    impl TestCachelineBase<TestCacheDataIndexed<u32, 8>>
    for &'static (dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>> + 'static) {
        fn read_cacheline(&self,
            cacheline_ptr: *const TestCacheDataIndexed<u32, 8>) {
            unimplemented!();
        }
        fn write_cacheline(&self,
            cacheline_ptr: *mut TestCacheDataIndexed<u32, 8>) {
            unimplemented!();
        }
        fn verify_cacheline(&self,
            cacheline_ptr: *const TestCacheDataIndexed<u32, 8>) {
            unimplemented!();
        }
    }
    unsafe impl Sync for MemArea {
    }
    unsafe impl<'a, CD, CL: ?Sized, D> Sync for MemAreaIterator<'a, CD, CL, D> {
    }

    struct TestMemoryScrubber<'a, D: Num + 'static, const N: usize> {
        scrubber:   MemoryScrubber::<'a, BASIC_CACHE_DESC,
                    &'a (dyn TestCachelineBase<TestCacheDataIndexed<D, N>> + 'a),
                    TestCacheDataIndexed<D, N>>,
        _marker1:   PhantomData<D>,
    }

    impl<'a, D: Num, const N: usize> TestMemoryScrubber<'a, D, N> {
        pub fn new(mem_areas: &'a [MemArea]) -> 
            Result<TestMemoryScrubber<'a, D, N>, Error> {
            let scrubber = match MemoryScrubber::<BASIC_CACHE_DESC,
                &'static (dyn TestCachelineBase<TestCacheDataIndexed<D, N>> + 'static),
                TestCacheDataIndexed<D, N>>
                ::new(&BASIC_CACHE_DESC, mem_areas) {
                Err(e) => return Err(e),
                Ok(scrubber) => scrubber,
            };

            Ok(TestMemoryScrubber {
                scrubber:   scrubber,
                _marker1:   PhantomData,
            })
        }

        pub fn scrub(&mut self, n: usize) -> Result<(), Error> {
            self.scrubber.scrub(n)
        }
    }

    impl<'a, D: Num, const N: usize> CacheDesc<'a, &dyn TestCachelineBase<TestCacheDataIndexed<D, N>>, TestCacheDataIndexed<D, N>>
    for BASIC_CACHE_DESC
    {}

    impl<'a, D: Num, const N: usize> Cacheline<'a, TestCacheDataIndexed<D, N>>
    for &dyn TestCachelineBase<TestCacheDataIndexed<D, N>>
    {}

    impl<'a, D: Num, const N: usize> CachelineBase<'a>
    for &dyn TestCachelineBase<TestCacheDataIndexed<D, N>>
    {}
    
/*
    impl<'a, D: Num, const N: usize> &dyn TestCachelineBase<TestCacheDataIndexed<D, N>>
    for dyn CachelineBase<'a>
    {}
*/


/*
            MemoryScrubber::<'static, BASIC_CACHE_DESC,
            &'static dyn TestCachelineBase<TestCacheDataIndexed<D, N>>,
            TestCacheDataIndexed<D, N>> {
            MemoryScrubber::<BASIC_CACHE_DESC,
            &'static dyn TestCachelineBase<TestCacheDataIndexed<D, N>>,
            TestCacheDataIndexed<D, N>>
            ::new(&BASIC_CACHE_DESC, mem_areas).expect("Unable to create MemoryScrubber")
*/

/*
    lazy_static! {
        static ref XXX: Result<TestMemoryScrubber<'a, u32, 8>, Error> = {
            TestMemoryScrubber::<u32, 8>
                ::new(&[MemArea{start: 0 as *const u8, end: 0 as *const u8}])
        };
    }
*/
        
/*
    lazy_static! {
        static ref BASIC_MEMORY_SCRUBBER:
            MemoryScrubber::<'static, BASIC_CACHE_DESC,
            &'static dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>,
            TestCacheDataIndexed<u32, 8>> = {
            MemoryScrubber::<BASIC_CACHE_DESC,
            &'static dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>,
            TestCacheDataIndexed<u32, 8>>::new(&BASIC_CACHE_DESC, &[MemArea{start: 0 as *const u8, end: 0 as *const u8}]).expect("Unable to create MemoryScrubber")
        };
    }
*/

/*
    impl &dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>
    {
        fn new(mem_areas: &[usize]) -> Self {
            unimplemented!();
        }
    }
*/
/*
    impl<'a> CacheDesc<'a, &'a dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>, TestCacheDataIndexed<u32, 8>>
    for BASIC_CACHE_DESC
    {}
*/

    impl<'a>CacheDescBase
    for BASIC_CACHE_DESC
    {
        fn read_cacheline(&self,
            cacheline_ptr: *const dyn CachelineBase) {
            unimplemented!();
        }
        fn cacheline_width(&self) -> usize {
            unimplemented!();
        }
        fn cache_index_width(&self) -> usize {
            unimplemented!();
        }
    }

/*
    impl<'a> Cacheline<'a, TestCacheDataIndexed<u32, 8>>
    for &dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>
    {}

    impl<'a> CachelineBase<'a>
    for &dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>
    {}

    impl<'a, CL: Cacheline<'a, D> + ?Sized, D> dyn CacheDesc<'a, CL, D> {
    }
*/

/*
// FIXME: move up?
    impl TestCachelineBase<BASIC_CACHE_DATA>
    for BASIC_CACHELINE {
        fn read_cacheline(&self,
            cacheline_ptr: *const BASIC_CACHE_DATA) {
            unimplemented!();
        }
        fn write_cacheline(&self,
            cacheline_ptr: *mut BASIC_CACHE_DATA) {
            unimplemented!();
        }
        fn verify_cacheline(&self,
            cacheline_ptr: *const BASIC_CACHE_DATA) {
            unimplemented!();
        }
    }

    impl TestCacheline<dyn TestCacheDataIndexedBase<u32, 8>> {
        fn new() -> TestCacheline<dyn TestCacheDataIndexedBase<u32, 8>> {
            TestCacheline::<dyn TestCacheDataIndexedBase<u32, 8>> {
                _marker1: PhantomData,
            }
        }
    }
*/
/*
        fn read_cacheline(&self, cacheline_ptr: *const DI) {
            unimplemented!();
        }
        fn write_cacheline(&self, cacheline_ptr: *mut DI) {
            unimplemented!();
        }
        fn verify_cacheline(&self, cacheline_ptr: *const DI) {
            unimplemented!();
        }
*/

    // FIXME: is this needed?
    impl TestCachelineBase<dyn TestCacheDataIndexedBase<u32, 8>>
    for dyn TestCacheDataIndexedBase<u32, 8> {
        fn read_cacheline(&self,
            cacheline_ptr: *const dyn TestCacheDataIndexedBase<u32, 8>) {
            unimplemented!();
        }
        fn write_cacheline(&self,
            cacheline_ptr: *mut dyn TestCacheDataIndexedBase<u32, 8>) {
            unimplemented!();
        }
        fn verify_cacheline(&self,
            cacheline_ptr: *const dyn TestCacheDataIndexedBase<u32, 8>) {
            unimplemented!();
        }
    }

/*
    impl TestCachelineBase<BASIC_CACHE_DATA> for BASIC_CACHELINE {
        fn read_cacheline(&self, cacheline_ptr: *const BASIC_CACHE_DATA) {
            unimplemented!();
        }
        fn write_cacheline(&self, cacheline_ptr: *mut BASIC_CACHE_DATA) {
            unimplemented!();
        }
        fn verify_cacheline(&self, cacheline_ptr: *const BASIC_CACHE_DATA) {
            unimplemented!();
        }
    }
*/

    // Description of memory that is read into by the read_cacheline() function.
    // This keeps the actually allocation together with the pointer into that
    // allocation so that things go out of scope at the same time.
    //
    // allocated_area - Vec<u8> of elements that can be read by read_cacheline()
    // start - Cache size-aligned pointer of the first byte to use in allocated_area
    // end - Pointer to the last byte
    #[derive(Clone, Debug)]
    struct Mem {
        allocated_area:   Vec<u8>,
        scrub_area: MemArea,
    }

    impl Mem {
        // Allocates a memory area on a cache line boundary
        //
        // cacheline_size - Number of bytes in a cache line
        // size - The number of bytes to allocate
        // 
        // Returns: a Mem with a Vec<u8>. The size of the Vec<u8> is
        // opaque but the p element in the Mem has at least size bytes
        // starting at a cache line aligned section of memory. The size element
        // is the size used to call this function.
        fn new<CD>(size: usize) -> Result<Mem, Error> {
            let cacheline_size = std::mem::size_of::<CD>();
            Self::new_aligned(size, cacheline_size)
        }

        fn new_aligned(size: usize, alignment_size: usize) ->
            Result<Mem, Error>{
            if size == 0 {
                return Err(Error::ZeroSize);
            }

            let log2_alignment_size = usize::BITS as usize - 1 -
                alignment_size.leading_zeros() as usize;
            if (1usize << log2_alignment_size) != alignment_size {
                return Err(Error::UnalignedSize);
            }

            // Allocate memory, which includes a cache size-sided area before
            // what we are touching. These areas should not be touched by
            // scrubbing.
            let allocated_area: Vec<u8> = vec![0; alignment_size + size];

            // Now find the first cache line aligned pointer
            let start_addr = (allocated_area.as_ptr() as Addr + alignment_size - 1) &
                !(alignment_size - 1);
            let start = start_addr as *const u8;
            let end = (start_addr + size - 1) as *const u8;

            Ok(Mem {
                allocated_area:   allocated_area,
                scrub_area: MemArea { start: start, end: end },
            })
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for Mem {}

    // Data structure a memory allocation along with counters for how many
    // times cache line-sized memory areas has been read.
    // mem:     Boundaries of the address covered by this structure
    // n_reads: Counters, one per cache line-sized area.
    #[derive(Clone, Debug)]
    struct ReadInfo {
        mem:        Mem,
        n_reads:    Option<Vec<NRead>>
    }

    impl ReadInfo {
        // Allocate a vector for counters
        // size:    Size in cache lines
        // mem:     Associated memory
        fn new(size: usize, mem: Mem) -> ReadInfo {
            let n_reads = vec![0; size];
            ReadInfo {
                mem:        mem,
                n_reads:    Some(n_reads),
            }
        }
    }
/*

    // Cache characteristics
    // TEST_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    // TEST_CACHE_LINES - number of cache lines
/* FIXME: restore this
    const TEST_CACHE_INDEX_WIDTH: usize = 10;
*/
*/
    const TEST_CACHE_INDEX_WIDTH: usize = 2;
    const TEST_CACHE_LINES: usize = 1 << TEST_CACHE_INDEX_WIDTH;

    // GUARD_LINES - Number of cache line size items we allocate but don't
    //      touch, to verify we don't touch the wrong place.
    // TEST_CACHE_NUM_TOUCHED - Number of cache footprints we use for
    //      testing
    // TEST_SANDBOX_SIZE - Number of cachelines we actually expect to
    //      touch
    // TEST_COOKIE - Cookie written to all memory we're scrubbing
    const GUARD_LINES: usize = TEST_CACHE_LINES;
/*
    const TEST_CACHE_NUM_TOUCHED: usize = 3;
    const TEST_SANDBOX_SIZE: usize =
        TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED;
*/
    const TEST_COOKIE: usize = 17;


    // Verify that an error is returned if the starting address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_start() {
        let mem = match Mem::new::<&dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        let mut mem = match Mem::new::<&dyn TestCachelineBase<TestCacheDataIndexed<u32, 8>>
>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        mem.scrub_area.start = unsafe {
            mem.scrub_area.start.offset(1)
        };

        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(mem.scrub_area);
        let memory_scrubber = TestMemoryScrubber::<u32, 8>::new(&scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedStart);
/*
        let memory_scrubber = BASIC_MEMORY_SCRUBBER.clone();
            MemoryScrubber::<BASIC_CACHE_DESC,
            BASIC_CACHELINE,
            TestCacheDataIndexed<u32, 8>>::new(&scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedStart);
*/
    }
/*

/*
    // Verify that an error is returned if the ending address is not
    // aligned on a cache line boundary
    #[test] #[ignore]
    fn test_unaligned_end() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let mut mem =
            match Mem::new::<BasicCacheline>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        mem.scrub_area.end = unsafe {
            mem.scrub_area.end.offset(-1)
        };

        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(mem.scrub_area);
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc,
                BasicCacheline>::new(basic_cache_desc,
                &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedEnd);
    }
*/

    // Verify that an error is returned if the size is zero.
    #[test] #[ignore]
    fn test_null_areas() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();

        let scrub_areas = Vec::<MemArea>::new();
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::NoMemAreas);
    }

    // Verify that an error is returned if the size is zero.
    #[test] #[ignore]
    fn test_zero_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let mut mem =
            match Mem::new::<BasicCacheline>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        mem.scrub_area.end = mem.scrub_area.start;

        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(mem.scrub_area);
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc,
            BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::EmptyMemArea);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test] #[ignore]
    fn test_aligned() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cacheline_size = basic_cache_desc.cacheline_size();
        let mem =
            match Mem::new::<BasicCacheline>(basic_cache_desc.cacheline_size() *
                basic_cache_desc.cache_lines() * 14) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };

        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(mem.scrub_area);
        let mut memory_scrubber =
            match MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
                &scrub_areas) {
            Err(e) => panic!("MemoryScrubber::new() failed {}", e),
            Ok(scrubber) => scrubber,
        };

        if let Err(e) = memory_scrubber.scrub(cacheline_size * 10) {
            panic!("scrub failed: {}", e);
        }
    }

    // Verify that all specified locations are scrubbed and locations outside
    // the requested are are not touched.
    #[test] #[ignore]
    fn test_touch_zero() {
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = 0;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test] #[ignore]
    fn test_touch_one() {
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = cacheline_size;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_many() {
        const MANY: usize = 50;
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = cacheline_size * MANY;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test] #[ignore]
    fn test_touch_all() {
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test] #[ignore]
    fn test_touch_double_all() {
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = 2 * cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test] #[ignore]
    fn test_touch_more_many() {
        const MANY: usize = 72;
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = 5 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test] #[ignore]
    fn test_touch_multiple_areas() {
        const MANY: usize = 72;
        let cl: TestCachelineBase32;
        let cacheline_size = cl.size();
        let first_area = 2 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        let second_area = cacheline_size * TEST_SANDBOX_SIZE;
        let third_area = cacheline_size * MANY;
        let scrub_areas = [first_area, second_area, third_area];
        test_scrubber(&scrub_areas, first_area);
    }

    #[test] #[ignore]
    fn test_big() {
        const MEM_AREA_SIZE: usize = 1 * 1024 * 1024 * 1024;

        let cl: TestCachelineBase32;
        let mut basic_cache_desc = BASIC_CACHE_DESC.clone();
        let cache_desc = &mut basic_cache_desc;
        let mem =
            match Mem::new::<BasicCacheline>(MEM_AREA_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(mem.scrub_area);
        let mut scrubber =
            match MemoryScrubber::<BasicCacheDesc, BasicCacheline>::
            new(cache_desc, &scrub_areas) {
            Err(e) => panic!("Could not create MemoryScrubber: {}",
                e),
            Ok(scrubber) => scrubber,
        };

        // Use the first scrub to page in all memory
        match scrubber.scrub(MEM_AREA_SIZE) {
            Err(e) => panic!("Scrub failed: {}", e),
            Ok(_) => {},
        }

        println!("Please wait while timing scrub operation");
        let start_time = Instant::now();

        match scrubber.scrub(MEM_AREA_SIZE) {
            Err(e) => panic!("Scrub failed: {}", e),
            Ok(_) => {},
        }

        let end_time = start_time.elapsed();
        let duration = end_time.as_secs_f64();

        let mem_size = (MEM_AREA_SIZE as f64) / 1e9;
        println!("Scrub rate: {:.2} GBps", mem_size / duration);
    }

/* FIXME: figure this out
    #[test] #[ignore]
    fn test_autoscrub() {
        let cl: TestCachelineBase32;
        let one_size = TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED * cl.size();
        let single_scan = one_size / 2;
        let total_scan = single_scan * 4 + 3 * cl.size();

        struct TestAutoScrubDesc {
            count: usize,
            scrub_size: usize,
        }

        impl<'a, CD: TestCacheDescTrait<'a, CL, D>, CL: TestCachelineBase<'a, D>,
            D: TestCacheData<D> + Index<usize>>
            AutoScrubDesc<CD, CL>
            for TestAutoScrubDesc {
            fn next(&mut self) -> usize {
                let n = if self.count > self.scrub_size { self.scrub_size }
                    else { self.count };
                self.count -= n;
                n
            }
        }

        let sizes = [one_size, one_size, one_size];
        let (test_cache_desc, scrub_areas) =
            setup_test_desc_areas::<TestCachelineBase32>(&sizes);

        let mut autoscrub_desc = TestAutoScrubDesc {
            count: total_scan,
            scrub_size: single_scan,
        };

        let mut autoscrub = match AutoScrub::new(&test_cache_desc, &scrub_areas,
            &mut autoscrub_desc) {
            Err(e) => panic!("AutoScrub::new failed: {}", e),
            Ok(autoscrub) => autoscrub,
        };
        match autoscrub.scrub() {
            Err(e) => panic!("autoscrub() failed: {}", e),
            Ok(_) => {},
        };
        verify_scrub(&autoscrub.scrubber, total_scan);
    }
*/

    // Test iterators
    const ITER_CACHE_INDEX_WIDTH: usize = 4;

    struct IterCacheline {
        data: [u16; 8],
    }

    impl<'a, D> Cacheline<'a, D> for IterCacheline {
    }

    #[derive(Clone)]
    struct IterCacheDesc {
        cache_index_width: usize,
    }

    impl IterCacheDesc {
        fn new(cache_index_width: usize) -> IterCacheDesc {
            IterCacheDesc {
                cache_index_width: cache_index_width,
            }
        }
    }

    impl<'a, CL: Cacheline<'a, D>, D> CacheDesc<'a, CL, D> for IterCacheDesc {
    }

    impl CacheDescBase for IterCacheDesc {
        fn cacheline_width(&self) -> usize {
            unimplemented!();
        }

        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline<'a>(&self,
            cacheline_ptr: *const dyn CachelineBase<'a>) {
// FIXME: add more checks
            // Assure that IterCacheline::data is read
            unsafe {
                assert_eq!((*cacheline_ptr).data[0], (*cacheline_ptr).data[0]);
            }
        }
    }

    #[test] #[ignore]
    fn test_iter_scrublines() {
        let iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);

        let cl_width = iter_cache_desc.cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = iter_cache_desc.cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        // Create areas that increasingly, then decreasingly, overlap in their
        // cache indices.
        let mut scrub_areas = Vec::<MemArea>::new();
        for i in 0..5 {
            let offset = i * cl_size;
            let size = offset + 3 * cl_size;
            let mut mem = match Mem::new_aligned(size, c_size) {
                Err(e) => panic!("Unable to allocate {} bytes: {}", size, e),
                Ok(mem) => mem,
            };
            mem.scrub_area.start =
                (mem.scrub_area.start as usize + offset) as *const u8;
            scrub_areas.push(mem.scrub_area);
        }

        let mut b = Vec::<usize>::new();
        for scrub_area in &scrub_areas {
            b.push(scrub_area.start as usize);
        }

        // We have to have enough cache lines available to avoid wrapping
        // when we go through the memory areas
        assert!(iter_cache_desc.cache_lines() > scrub_areas.len());

        let expected_overlapping_areas_slice = [
            // Overlapping areas
            b[0] + 0 * cl_size,
            b[0] + 1 * cl_size, b[1] + 0 * cl_size,
            b[0] + 2 * cl_size, b[1] + 1 * cl_size, b[2] + 0 * cl_size,
            b[1] + 2 * cl_size, b[2] + 1 * cl_size, b[3] + 0 * cl_size,
            b[2] + 2 * cl_size, b[3] + 1 * cl_size, b[4] + 0 * cl_size,
            b[3] + 2 * cl_size, b[4] + 1 * cl_size,
            b[4] + 2 * cl_size,
        ];
        let expected_overlapping_areas =
            expected_overlapping_areas_slice.to_vec();

        let mut expecteds = Vec::<usize>::new();
        expecteds.append(&mut expected_overlapping_areas.clone());
        expecteds.append(&mut expected_overlapping_areas.clone());

        let mut iterator = ScrubLinesIterator::new(&iter_cache_desc,
            &scrub_areas).expect("ScrubLinesIterator failed");

        for (i, expected) in (0..expecteds.len()).into_iter()
            .zip(expecteds.iter()) {
            let expected_p = *expected as *const IterCacheline;
            let actual = match iterator.next() {
                None => panic!("Ran out of scrub areas too soon"),
                Some(actual) => actual,
            };
            let cache_index =
                iter_cache_desc.cache_index(actual as *const u8);
            if actual != expected_p {
                println!("{}: actual {:p} expected {:p} index {}", i, actual, expected_p, cache_index);
                let actual_breakdown = breakdown(&iter_cache_desc,
                        actual as *const IterCacheline, &scrub_areas, &b);
                let expected_breakdown = breakdown(&iter_cache_desc,
                    expected_p, &scrub_areas, &b);
                eprintln!("   actual: b[{}] + {} * cl_size expected: b[{}] + {} * cl_size",
                    actual_breakdown.0, actual_breakdown.1 / cl_size,
                    expected_breakdown.0, expected_breakdown.1 / cl_size);

                print_scrub_areas(&iter_cache_desc, &scrub_areas);
            }
            assert_eq!(actual, expected_p);
        }
        let next = iterator.next();
        assert!(next.is_some());
    }

    // Find the base and offset within the scrub areas
    fn breakdown(iter_cache_desc: &IterCacheDesc,
        expected: *const IterCacheline, scrub_areas: &Vec::<MemArea>,
        b: &Vec::<usize>) -> (usize, usize) {
        let mut found: Option<usize> = None;
        for (j, scrub_area) in (0..scrub_areas.len()).into_iter()
            .zip(scrub_areas.iter()) {
            if ((scrub_area.start as usize) <= expected as usize) &&
                ((expected as usize) < scrub_area.end as usize) {
                found = Some(j);
                break;
            }
        }
        if found.is_none() {
            print_scrub_areas(&iter_cache_desc, &scrub_areas);
            panic!("Unable to find address {:p}", expected);
        }
        let j = found.unwrap();
        let delta = expected as usize - b[j];
        (j, delta)
    }

    #[test] #[ignore]
    fn test_iter_cache_indices() {

        let iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);

        let cl_width = iter_cache_desc.cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = iter_cache_desc.cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        let mut scrub_areas = Vec::<MemArea>::new();
        scrub_areas.push(Mem
            ::new_aligned(4 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas.push(Mem
            ::new_aligned(5 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas[1].start = (scrub_areas[1].start as usize +
            1 * cl_size) as *const u8;
        scrub_areas.push(Mem
            ::new_aligned(6 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas[2].start = (scrub_areas[2].start as usize +
            2 * cl_size) as *const u8;
        scrub_areas.push(Mem
            ::new_aligned(7 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas[3].start = (scrub_areas[3].start as usize +
            3 * cl_size) as *const u8;
        scrub_areas.push(Mem
            ::new_aligned(8 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas[4].start = (scrub_areas[4].start as usize +
            4 * cl_size) as *const u8;
        scrub_areas.push(Mem
            ::new_aligned(9 * cl_size, c_size).unwrap().scrub_area);
        scrub_areas[5].start = (scrub_areas[5].start as usize +
            5 * cl_size) as *const u8;

        let base1 = scrub_areas[0].start as usize;
        let base2 = scrub_areas[1].start as usize;
        let base3 = scrub_areas[2].start as usize;
        let base4 = scrub_areas[3].start as usize;
        let base5 = scrub_areas[4].start as usize;
        let base6 = scrub_areas[5].start as usize;

        let expecteds = [
            base1 + 0 * cl_size,
            base1 + 1 * cl_size, base2 + 0 * cl_size,
            base1 + 2 * cl_size, base2 + 1 * cl_size, base3 + 0 * cl_size,
            base1 + 3 * cl_size, base2 + 2 * cl_size, base3 + 1 * cl_size,
            base4 + 0 * cl_size, base2 + 3 * cl_size, base3 + 2 * cl_size,
            base4 + 1 * cl_size, base5 + 0 * cl_size, base3 + 3 * cl_size,
            base4 + 2 * cl_size, base5 + 1 * cl_size,
            base6 + 0 * cl_size,
        ];

        let mut iterator = CacheIndexIterator::new(&iter_cache_desc,
            &scrub_areas).expect("CacheIndexIterator::new failed");

        for expected in expecteds {
            let expected_p = expected as *const IterCacheline;
            let actual = match iterator.next() {
                None => panic!("Ran out of scrub areas too soon"),
                Some(actual) => actual,
            };
            assert_eq!(actual, expected_p);
        }
        let next = iterator.next();
        assert!(next.is_some());
    }

    #[test] #[ignore]
    fn test_iter_scrub_areas() {
        struct TestDesc<'a> {
            cur_index:  usize,
            expected:   &'a [usize],
        }

        let iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);

        let cl_width = iter_cache_desc.cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = iter_cache_desc.cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        let base1 = 0x10 * c_size + 0 * cl_size;
        let s1 = (base1) as *const u8;
        let e1 = (base1 + 1 * cl_size) as *const u8;
        let base2 = 0x20 * c_size + 1 * cl_size;
        let s2 = (base2) as *const u8;
        let e2 = (base2 + 1 * cl_size) as *const u8;
        let base3 = 0x30 * c_size + 2 * cl_size;
        let s3 = (base3) as *const u8;
        let e3 = (base3 + 4 * c_size + 3 * cl_size) as *const u8;
        let base4 = 0x40 * c_size + 3 * cl_size;
        let s4 = (base4) as *const u8;
        let e4 = (base4 + 2 * cl_size) as *const u8;
        let base5 = 0x50 * c_size + 3 * cl_size;
        let s5 = (base5) as *const u8;
        let e5 = (base5 + 2 * cl_size + 2 * cl_size) as *const u8;

        let phys_scrub_areas = [
            MemArea::new(s1, e1),
            MemArea::new(s2, e2),
            MemArea::new(s3, e3),
            MemArea::new(s4, e4),
            MemArea::new(s5, e5),
        ];

        let expected1 = [base1 + 0 * c_size];
        let expected2 = [base1 + 0 * c_size + 1 * cl_size,
            base2 + 0 * c_size + 0 * cl_size];
        let expected3 = [base2 + 0 * c_size + 1 * cl_size, base3 + 0 * c_size,
            base3 + 1 * c_size, base3 + 2 * c_size, base3 + 3 * c_size];

        let test_desc:  [TestDesc; 3] = [
            TestDesc {cur_index: 0, expected: &expected1},
            TestDesc {cur_index: 1, expected: &expected2},
            TestDesc {cur_index: 2, expected: &expected3},
        ];

        for test_datum in test_desc {
            let iterator = MemAreasIterator::new(&iter_cache_desc,
                &phys_scrub_areas, test_datum.cur_index).
                expect("MemAreasIterator failed");

            // Scan the entire set of MemAreas
            let mut count = 0;
            for (actual, expected) in
                iterator.into_iter().zip(test_datum.expected.iter()) {
                let expected_p = *expected as *const IterCacheline;
                assert_eq!(actual, expected_p);
                count += 1;
            };
            assert_eq!(count, test_datum.expected.len());
        }
    }

    #[test] #[ignore]
    fn test_iter_scrub_area() {
        struct TestDesc<'a> {
            cur_index:  usize,
            scrub_area: &'a MemArea,
            expected:   &'a[usize],
        }
        let iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);

        let cl_width = iter_cache_desc.cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = iter_cache_desc.cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        // These all start two cache lines above a cache-sized boundary
        let delta1 = 1 * cl_size;
        let mut phys_mem1 =
            match Mem::new_aligned(delta1 + 2 * cl_size, c_size) {
            Err(e) => panic!("Failed to allocate phys_mem1: {}", e),
            Ok(phys_mem) => phys_mem,
        };
        phys_mem1.scrub_area.start = (phys_mem1.scrub_area.start as usize +
            delta1) as *const u8;

        let delta2 = 1 * cl_size;
        let mut phys_mem2 =
            match Mem::new_aligned(delta2 + 3 * c_size, c_size) {
            Err(e) => panic!("Failed to allocate phys_mem2: {}", e),
            Ok(phys_mem) => phys_mem,
        };
        phys_mem2.scrub_area.start = (phys_mem2.scrub_area.start as usize +
              delta2) as *const u8;

        let delta3 = 1 * cl_size;
        let mut phys_mem3 =
            match Mem::new_aligned(delta3 + 1 * cl_size, c_size) {
            Err(e) => panic!("Failed to allocate phys_mem3: {}", e),
            Ok(phys_mem) => phys_mem,
        };
        phys_mem3.scrub_area.start = (phys_mem3.scrub_area.start as usize +
              delta3) as *const u8;

        let base1 = phys_mem1.scrub_area.start as usize;
        let base2 = phys_mem2.scrub_area.start as usize;
        let base3 = phys_mem3.scrub_area.start as usize;

        let expected1 = [];
        let expected2 = [base1];
        let expected3 = [base1 + cl_size];
        let expected4 = [base2 + (c_size - delta2) + 0 * c_size + 0 * cl_size,
            base2 + (c_size - delta2) + 1 * c_size + 0 * cl_size];
        let expected5 = [base2 + 0 * c_size + 0 * cl_size,
            base2 + 1 * c_size + 0 * cl_size,
            base2 + 2 * c_size + 0 * cl_size];
        let expected6 = [base2 + 0 * c_size + 1 * cl_size,
            base2 + 1 * c_size + 1 * cl_size,
            base2 + 2 * c_size + 1 * cl_size];
        let expected7 = [base3 + 0 * cl_size];

        let test_desc: [TestDesc; 7] = [
            TestDesc { cur_index: 0, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected1},
            TestDesc { cur_index: 1, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected2},
            TestDesc { cur_index: 2, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected3},
            TestDesc { cur_index: 0, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected4},
            TestDesc { cur_index: 1, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected5},
            TestDesc { cur_index: 2, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected6},
            TestDesc { cur_index: 1, scrub_area: &phys_mem3.scrub_area.clone(),
                expected: &expected7},
        ];


        for test_datum in test_desc {
            let sax = &test_datum.scrub_area;
            let cur_index = test_datum.cur_index;
            let iterator = MemAreaIterator::new(&iter_cache_desc, sax,
                cur_index).expect("MemAreaIterator::new failed");

            // Scan the entire set of MemAreas
            let mut count = 0;
            for (actual, expected) in
                iterator.into_iter().zip(test_datum.expected.iter()) {
                let expected_p = *expected as *const IterCacheline;
                assert_eq!(actual, expected_p);
                count += 1;
            };
            assert_eq!(count, test_datum.expected.len());
        }
    }

    #[allow(dead_code)]
    fn print_scrub_areas(cache_desc: &IterCacheDesc,
        scrub_areas: &[MemArea]) {
        let mut total_bytes: usize = 0;
        let cacheline_width = cache_desc.cacheline_width();

        // Print the tuples in the vector
        println!("Physical addresses:");
        for (i, scrub_area) in (0..scrub_areas.len()).into_iter()
            .zip(scrub_areas.iter()) {
            let delta_cachelines =
                cache_desc.size_in_cachelines(&scrub_area);
            let delta_bytes = delta_cachelines << cacheline_width;
            total_bytes += delta_bytes;
            println!("{}: {:p}-{:p}: {} ({} cache lines)", i,
                scrub_area.start, scrub_area.end, delta_bytes,
                delta_cachelines);
        }

        println!("total size {} bytes ({} cache lines)", total_bytes,
            total_bytes >> cacheline_width);
    }

    #[test] #[ignore]
    fn test_iter() {
        let iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);

        let cl_width = iter_cache_desc.cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = iter_cache_desc.cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        let s1 = (2 * cl_size) as * const u8;
        let e1 = (2 * cl_size + 3 * c_size) as *const u8;
        let s2 = (4 * c_size) as *const u8;
        let e2 = (4 * c_size + cl_size) as *const u8;
        let s3 = (5 * c_size) as *const u8;
        let e3 = (5 * c_size + 2 * cl_size) as *const u8;
        let virt_scrub_areas = [
            MemArea::new(s1, e1),
            MemArea::new(s2, e2),
            MemArea::new(s3, e3),
        ].to_vec();

        let mut total_bytes: usize = 0;
        for scrub_area in &virt_scrub_areas {
            let delta = iter_cache_desc.size_in_cachelines(&scrub_area);
            total_bytes += delta;
        }
        total_bytes *= iter_cache_desc.cacheline_size();

        // Print the tuples in the vector
        let cur_indices = [0];

        for _cur_index in cur_indices {
            let mut iterator = ScrubLinesIterator::new(&iter_cache_desc,
                &virt_scrub_areas).expect("ScrubLinesIterator failed");

            // Scan the entire set of MemAreas
            for _i in 0..total_bytes >> iter_cache_desc.cacheline_width() {
                let next = iterator.next();
                let _p = next.unwrap();
// FIXME: check against actuals
            };
        }
    }

    // Test support function that scrubs a section of memory, then verifies that
    // things were properly referred.
    // sizes - array of sizes of memory areas to scrub
    // n - number of cache lines to scrub
    fn test_scrubber(sizes: &[usize], n: usize) {
        let (test_cache_desc, scrub_areas) =
            setup_test_desc_areas(sizes);

        let mut memory_scrubber = {
            match MemoryScrubber::<TestCacheDesc<TestCachelineBase>, TestCachelineBase>
                ::new(&test_cache_desc, &scrub_areas) {
                Err(e) => panic!("MemoryScrubber::new() failed {}", e),
                Ok(scrubber) => scrubber,
            }
        };

        if let Err(e) = memory_scrubber.scrub(n) {
            panic!("scrub failed: {}", e);
        };

        verify_scrub::<TestCacheDesc, TestCachelineBase>(&memory_scrubber, n);

        // Ensure that allocated_area is used
        for read_info in
            &memory_scrubber.cache_desc.test.borrow_mut().read_infos {
            let allocated_area = &read_info.mem.allocated_area;
            assert_eq!(allocated_area[0], allocated_area[0]);
        }
    }

    // Set up a TestCacheDesc and MemAreas
    fn setup_test_desc_areas<'a, CL: TestCachelineBase<'a, D>, D>(sizes: &[usize]) -> Vec<MemArea> {
        let test_cache_desc = TestCacheDesc::<CL>::new(sizes);

        // Allocate memory areas according to the given sizes
        let mut scrub_areas: Vec<MemArea> = vec!();
        {
            for read_info in &test_cache_desc.test.borrow_mut().read_infos {
                scrub_areas.push(read_info.mem.scrub_area.clone());
            }
        }

        (test_cache_desc, scrub_areas)
    }

    // Verify the proper locations were hit
    // memory_scrubber - MemoryScrubber to use
    // n - bytes scrubbed
    //
    // This essentially reimplements the iterator code but in a more straight-
    // forward way so that the two implements can verify each other.
    fn verify_scrub<'a, CD: TestCacheDescTrait<'a, CL, D>, CL: TestCachelineBase<'a, D>, D: TestCacheData<D> + CachelineData + Index<usize>>(memory_scrubber: &MemoryScrubber<'a, CD, CL, D>,
        n: usize) {
        let cache_desc = memory_scrubber.cache_desc;
        let cacheline_width = cache_desc.cacheline_width();
        let n_in_cachelines = n >> cacheline_width;
        let cache_lines = cache_desc.cache_lines();

        // Count the total number of scrub lines in all of the MemAreas
        let mut scrub_lines = 0;

        for scrub_area in memory_scrubber.iterator.scrub_areas {
            scrub_lines += memory_scrubber.cache_desc
                .size_in_cachelines(scrub_area);
        }

        let n_min_reads = match (n_in_cachelines / scrub_lines)
            .try_into() {
            Err(e) => panic!("Internal Error: n_min_reads conversion failed: {}", e),
            Ok(n_min_reads) => n_min_reads,
        };
        let n_extra_reads = n_in_cachelines % scrub_lines;

        let mut verified = 0;

        // Scan the cache indices
        for cache_index in 0..cache_lines {
            verify_cache_index::<TestCacheDescTrait>(&memory_scrubber.cache_desc, cache_index,
                n_min_reads, n_extra_reads, &mut verified);
        }
    }

    // Verified that all of the areas for this cache line are correct
    // memory_scrubber: Memory scrubber
    // cache_index:     Cache index being verified
    // verified:        Number of cache indices verified so far
//    fn verify_cache_index<CD: TestCacheDesc<'a, CL>, CL: TestCachelineBase>(cache_desc: &CD,
    fn verify_cache_index<CD>(cache_desc: &CD,
        cache_index: usize, n_min_reads: usize, n_extra_reads: usize,
        verified: &mut usize) {
        for read_info in cache_desc.test.borrow_mut().read_infos {
            verify_read_info(cache_desc, &read_info, cache_index,
                n_min_reads, n_extra_reads, verified);
        }
    }

    // Verify that one scrub area is correct
    // memory_scrubber: The MemoryScrubber being verified
    // scrub_area: the area being verified
    // verified: The number of addresses that have already been verified
    // n_min_reads: The minimum number of reads in the vectors in n_reads
    // n_extra_reads: The number of items in the vectors in n_reads which
    //      are greater than n_min_reads by one
    //
    // Returns the number of addresses that were verified in this scrub area
    fn verify_read_info<'a, CL: TestCachelineBase<'a, D>, D>(cache_desc: &'a TestCacheDesc<'a, CL, D>,
        read_info: &ReadInfo, cache_index: usize, n_min_reads: usize,
        n_extra_reads: usize, verified: &mut usize) {
        let cache_lines = cache_desc.cache_lines();
        let size_in_cachelines =
            cache_desc.size_in_cachelines(&read_info.mem.scrub_area);

        let start = read_info.mem.scrub_area.start;
        let start_index =
            cache_desc.first_offset_for_index(start, cache_index);
        let mem = unsafe {
            slice::from_raw_parts(start as *const CL, size_in_cachelines)
        };

        let n_reads = &read_info.n_reads.as_ref().unwrap();
        verify_guard(n_reads, 0);

        // Now verify the contents of the memory to see whether they were
        // touched the expected number of times. The number of hits for a
        // location i in n_reads[] will be at least equal to the number of
        // complete scans of the memory area. Then, the remaining number of
        // items in the scan will be one larger.
        for i in (start_index..size_in_cachelines).step_by(cache_lines) {
            let inc = if verified < &mut n_extra_reads { 1 } else { 0 };
            let n_expected = n_min_reads + inc;
            let n_actual = n_reads[GUARD_LINES + i];
println!("n_extra_reads {} n_min_reads {}", n_extra_reads, n_min_reads);
println!("n_expected {} n_actual {}", n_expected, n_actual);
println!("i {}", i);
            let n_expected: NRead = n_expected.try_into()
                .expect("Numerical conversion failed");
            assert_eq!(n_actual, n_expected);

            let mem_actual = mem[i].data[0];
            let mem_expected =
                if n_min_reads == 0 && *verified >= n_extra_reads { 0 }
                else { TEST_COOKIE };

            assert_eq!(mem_actual, mem_expected);
            for data in &mem[i].data[1..] {
                let mem_actual = *data;
                assert_eq!(mem_actual, 0);
            }

            *verified += 1;
        }

        verify_guard(n_reads, GUARD_LINES + TEST_SANDBOX_SIZE);
    }

    // Verify a guard area before the memory area. This should
    // not have been seen and so should have a zero value
    // n_reads - Array that should have all zero values at the offset for
    //      GUARD_LINES elements
    // offset - Offset in n_reads to check
    fn verify_guard(n_reads: &Vec<NRead>, offset: usize) {

        for i in 0..GUARD_LINES {
            let actual = n_reads[offset + i];
            assert_eq!(actual, 0);
        }
    }
*/
}
