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
//              MemoryScrubber, ScrubArea};
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
//              fn read_cacheline(&mut self,
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
//              ScrubArea {
//                  start: 0xa0000000 as *const u8,
//                  end: 0xbfffffff as *const u8,
//              },
//              ScrubArea {
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
//              let mut my_cache_desc = MyCacheDesc.clone();
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
//              fn read_cacheline(&mut self, cacheline_ptr: *const MyCacheline) {
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

use std::cell::RefCell;
use std::fmt;
use std::iter;
use std::marker::PhantomData;
use std::rc::Rc;
use std::slice;

// Structure used to define an area to be scrubbed
// start - lowest address of the area. Must be a multiple of the cache line size
// end - address of the last byte of the area. Must be one less than a multiple
//      of the cache line size
#[derive(Clone)]
#[repr(C)]
pub struct ScrubArea {
    pub start:              *const u8,
    pub end:                *const u8,
}

impl ScrubArea {
    pub fn new(start: *const u8, end: *const u8) -> ScrubArea {
        ScrubArea { start: start, end: end }
    }
}

pub trait Cacheline {
}

#[derive(Clone)]
#[repr(C)]
pub struct CCacheDesc {
/*
    me: *mut CCacheDesc,
*/
    cacheline_width: usize,
/*
    // Return the number of bits required to hold an index into the bytes of
    // a cache line. So, if you have an eight-byte cache line (unlikely), this
    // would return 3.
    c_cacheline_width: extern "C" fn(me: *const CCacheDesc) -> usize,
*/

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
    c_read_cacheline: extern "C" fn(me: *mut CCacheDesc,
        cacheline_ptr: *const CCacheline),

    // Return the size of a ScrubArea in cachelines
    c_size_in_cachelines: extern "C" fn(me: *const CCacheDesc,
        scrub_area: &ScrubArea) -> usize,

    // Returns the cache index part of the address
    c_cache_index: extern "C" fn(me: *const CCacheDesc,
        p: *const u8) -> usize,
}

impl CCacheDesc {
    pub fn new(cacheline_width: usize,
        c_cacheline_size: extern "C" fn(me: *const CCacheDesc) -> usize,
        c_cache_index_width: extern "C" fn(me: *const CCacheDesc) -> usize,
        c_read_cacheline: extern "C" fn(me: *mut CCacheDesc,
            cacheline_ptr: *const CCacheline),
        c_size_in_cachelines: extern "C" fn(me: *const CCacheDesc,
            scrub_area: &ScrubArea) -> usize,
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

impl CacheDesc<CCacheline> for CCacheDesc {
    fn cacheline_width(&self) -> usize {
        self.cacheline_width
    }

    fn cacheline_size(&self) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        (self.c_cacheline_size)(self_ptr)
    }

    fn cache_index_width(&self) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        (self.c_cache_index_width)(self_ptr)
    }

    fn read_cacheline(&mut self, cacheline_ptr: *const CCacheline) {
        let self_ptr: *mut CCacheDesc = self as *mut _;
        (self.c_read_cacheline)(self_ptr, cacheline_ptr)
    }

    fn size_in_cachelines(&self, scrub_area: &ScrubArea) -> usize {
        let self_ptr: *const CCacheDesc = self as *const _;
        (self.c_size_in_cachelines)(self_ptr, scrub_area)
    }
}

#[repr(C)]
pub struct CAutoScrubDesc {
    me: *mut CAutoScrubDesc,
    c_next: extern "C" fn(me: *mut CAutoScrubDesc) -> usize,
}

impl AutoScrubDesc for CAutoScrubDesc {
    fn next(&mut self) -> usize {
        (self.c_next)(self.me)
    }
}

#[repr(C)]
pub struct AutoScrubResult {
    is_err:  bool,
    error:  Error,
}

#[repr(C)]
pub struct CCacheline {
    data: u8,
}

impl Cacheline for CCacheline {
}

// This is the C interface to autoscrub.
//    scrub_areas_ptr: *const ScrubArea, n_scrub_areas: usize,
//    c_auto_scrub_desc: &'a mut dyn AutoScrubDesc) -> AutoScrubResult {
#[no_mangle]
pub extern "C" fn autoscrub(c_cache_desc: &mut CCacheDesc,
    scrub_areas_ptr: *const ScrubArea, n_scrub_areas: usize,
    c_auto_scrub_desc: &mut CAutoScrubDesc) -> AutoScrubResult {

    let scrub_areas = unsafe {
        // It looks like the slice constructed with from_raw_parts is
        // never freed. This is the behavior we want when we get a pointer
        // from C/C++.
        slice::from_raw_parts(scrub_areas_ptr, n_scrub_areas)
    };

    match AutoScrub::<CCacheDesc, CCacheline>::autoscrub(c_cache_desc,
        &scrub_areas, c_auto_scrub_desc) {
        Err(e) => AutoScrubResult { is_err: true, error: e },
        Ok(_) => AutoScrubResult { is_err: false, error: Error::InternalError },
    }
}

// Data type that can hold any address for manipulation as an integer
type Addr = usize;

// Describe cache parameters and pull in all elements of the cache line.
pub trait CacheDesc<T> {
    // NOTE: You are unlikely to ever need to implement this
    // Return the number of bits required to hold an index into the bytes of
    // a cache line. So, if you have an eight-byte cache line (unlikely), this
    // would return 3.
    fn cacheline_width(&self) -> usize {
        usize::BITS as usize - 1 - std::mem::size_of::<T>()
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
    fn read_cacheline(&mut self, cacheline_ptr: *const T);

    // Return the size of a ScrubArea in cachelines
    fn size_in_cachelines(&self, scrub_area: &ScrubArea) -> usize {
        let start_in_cachelines =
            scrub_area.start as usize >> self.cacheline_width();
        // This will truncate the number of cache lines by one
        let end_in_cachelines =
            scrub_area.end as usize >> self.cacheline_width();
        (end_in_cachelines - start_in_cachelines) + 1
    }

    // Extract the cache index part of the address
    fn cache_index(&self, p: *const u8) -> usize {
        (p as usize >> self.cacheline_width()) &
            ((1 << self.cache_index_width()) - 1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
pub enum Error {
    InternalError,
    UnalignedStart,
    UnalignedEnd,
    UnalignedSize,
    NoScrubAreas,
    EmptyScrubArea,
    ZeroSize,
    IteratorFailed,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait AutoScrubDesc {
    fn next(&mut self) -> usize;
}

struct AutoScrub<'a, T: CacheDesc<U>, U: Cacheline> {
    scrubber:   MemoryScrubber<'a, T, U>,
    desc:       &'a mut dyn AutoScrubDesc,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<U>. So, this
    // works around that problem
    _marker:    PhantomData<U>,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> AutoScrub<'a, T, U> {
    fn new(cache_desc_in: &'a mut T, scrub_areas: &'a [ScrubArea],
        desc: &'a mut dyn AutoScrubDesc) ->
        Result<AutoScrub<'a, T, U>, Error> {
        let scrubber = match MemoryScrubber::new(cache_desc_in, scrub_areas) {
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

    pub fn autoscrub(cache_desc: &'a mut T, scrub_areas: &'a [ScrubArea],
            desc: &'a mut dyn AutoScrubDesc) -> Result<(), Error> {
        let mut autoscrub = Self::new(cache_desc, scrub_areas, desc)?;
        autoscrub.scrub()
    }
}

// Memory scrubber
// cache_desc - Description of the cache
// iterator - MemoryScrubberIterator used to walk through the memory being
//      scrubbed
// cur_index:   Current cache line index
pub struct MemoryScrubber<'a, T: CacheDesc<U>, U: Cacheline> {
    cache_desc:     Rc<RefCell<&'a mut T>>,
    iterator:       ScrubLinesIterator<'a, T, U>,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<U>. So, this
    // works around that problem
    _marker:    PhantomData<U>,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> MemoryScrubber<'a, T, U> {

    // Create a new memory scrubber
    // cache_desc_in - Description of the cache
    // start - Virtual address of memory being scrubbed
    // end - Virtual address of the last byte of memory to be scrubbed
    pub fn new(cache_desc_in: &'a mut T, scrub_areas: &'a [ScrubArea]) ->
        Result<MemoryScrubber<'a, T, U>, Error> {
        let cache_desc = Rc::new(RefCell::new(cache_desc_in));

        if scrub_areas.len() == 0 {
            return Err(Error::NoScrubAreas);
        }

        let cacheline_size = {
            cache_desc.borrow().cacheline_size()
        };

        // Check each ScrubArea for errors
        for scrub_area in scrub_areas {
            // The code will actually handle this just fine, but it's extra
            // effort to no benefit, so it is expected to be a user error.
            if scrub_area.start == scrub_area.end {
                return Err(Error::EmptyScrubArea);
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

        let iterator = ScrubLinesIterator::new(cache_desc.clone(),
            &scrub_areas);

        let scrubber = MemoryScrubber::<'a, T, U> {
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
            self.cache_desc.borrow().cacheline_width()
        };

        let cacheline_size = {
            self.cache_desc.borrow().cacheline_size()
        };

        if (n & (cacheline_size - 1)) != 0 {
            return Err(Error::UnalignedSize);
        }

        // Convert to the number of cachelines to scrub
        let n_scrublines = n >> cacheline_width;

        // At this point, it's pretty much Iterators all the way down.
        for _i in 0..n_scrublines {
            let next = self.iterator.next();
            let p = next.unwrap();
            self.cache_desc.borrow_mut().read_cacheline(p);
        }

        Ok(())
    }
}

// Walks through cache line-sized items. Never reaches an end, that is,
// next() never returns None
pub struct ScrubLinesIterator<'a, T, U> {
    cache_desc:     Rc<RefCell<&'a mut T>>,
    scrub_areas:    &'a [ScrubArea],
    iterator:       CacheIndexIterator<'a, T, U>,
    cur_index:      usize,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> ScrubLinesIterator<'a, T, U> {
    pub fn new(cache_desc: Rc<RefCell<&'a mut T>>,
        scrub_areas: &'a [ScrubArea]) -> ScrubLinesIterator<'a, T, U> {
//println!("ScrubLinesIterator::new: create new CacheIndexIterator");
        let cur_index = 0;
        let iterator = CacheIndexIterator::<T, U>::new(cache_desc.clone(),
            scrub_areas, cur_index);

        ScrubLinesIterator {
            cache_desc:     cache_desc.clone(),
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            cur_index:      cur_index,
        }
    }
}

impl<'a, T: CacheDesc<U>, U: Cacheline> iter::Iterator for
    ScrubLinesIterator<'a, T, U> {
    type Item = *const U;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.iterator.next();

            match next {
                None => {
//println!("ScrubLinesIterator:next: create new CacheIndexIterator cur_index {}", self.cur_index);
                    self.iterator = CacheIndexIterator::<T, U>
                        ::new(self.cache_desc.clone(),
                        &self.scrub_areas, self.cur_index);
                },
                Some((cur_index, p)) => {
                    self.cur_index = cur_index;
//println!("ScrubLinesIterator:next: returning Some({}, {:p})", cur_index, p);
                    return Some(p);
                },
            }
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
pub struct CacheIndexIterator<'a, T, U> {
    cache_desc:     Rc<RefCell<&'a mut T>>,
    scrub_areas:    &'a [ScrubArea],
    iterator:       ScrubAreasIterator<'a, T, U>,
    cur_index:      usize,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> CacheIndexIterator<'a, T, U> {
    pub fn new(cache_desc: Rc<RefCell<&'a mut T>>,
        scrub_areas: &'a [ScrubArea], cur_index: usize) ->
        CacheIndexIterator<'a, T, U> {
//println!("CacheIndexIterator::new: create new ScrubAreasIterator");
        let iterator = ScrubAreasIterator::<T, U>::new(cache_desc.clone(),
            &scrub_areas, cur_index);

        CacheIndexIterator {
            cache_desc:     cache_desc,
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            cur_index:      cur_index,
        }
    }
}

impl<'a, T: CacheDesc<U>, U: Cacheline> iter::Iterator for
    CacheIndexIterator<'a, T, U> {
    type Item = (usize, *const U);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.iterator.next();

            if let Some(p) = next {
//println!("CacheIndexIterator:next: returning Some({}, {:p})", self.cur_index, p);
                return Some((self.cur_index, p));
            }

            let cache_index_width = self.cache_desc.borrow()
                .cache_index_width();
            if self.cur_index == 1 << cache_index_width {
//println!("CacheIndexIterator:next: returning None");
                return None;
            }

            self.cur_index += 1;
//println!("CacheIndexIterator::next: create new ScrubAreasIterator");
            self.iterator = ScrubAreasIterator::<T, U>
                ::new(self.cache_desc.clone(),
                &self.scrub_areas, self.cur_index);
        }
    }
}

// This iterator goes through all defined scrub areas
// cache_desc:  Cache descriptor
// scrub_areas: List of memory areas to be scrubbed
// iterator:    An iterator for a scrubbing a single memory area
// i:           The index into the current ScrubArea
pub struct ScrubAreasIterator<'a, T, U> {
    cache_desc:     Rc<RefCell<&'a mut T>>,
    scrub_areas:    &'a [ScrubArea],
    iterator:       ScrubAreaIterator<'a, T, U>,
    i:              usize,
    cur_index:      usize,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> ScrubAreasIterator<'a, T, U> {
    pub fn new(cache_desc: Rc<RefCell<&'a mut T>>,
        scrub_areas: &'a [ScrubArea], cur_index: usize) ->
        ScrubAreasIterator<'a, T, U> {
//println!("ScrubAreasIterator::new: create new ScrubAreaIterator");
        let iterator =
            ScrubAreaIterator::<T, U>::new(cache_desc.clone(), &scrub_areas[0],
            cur_index);
        ScrubAreasIterator {
            cache_desc:     cache_desc,
            scrub_areas:    scrub_areas,
            iterator:       iterator,
            i:              0,
            cur_index:      cur_index,
        }
    }
}

impl<'a, T: CacheDesc<U>, U: Cacheline> iter::Iterator for
    ScrubAreasIterator<'_, T, U> {
    type Item = *const U;

    // Loop through all scrub areas
    fn next(&mut self) -> Option<Self::Item> {
        assert_ne!(self.scrub_areas.len(), 0);

        // Assuming not zero length
        loop {
            let next = self.iterator.next();

            if let Some(p) = next {
//println!("ScrubAreasIterator::next: return Some({:p})", p);
                return Some(p);
            }

            self.i += 1;
            if self.i == self.scrub_areas.len() {
//println!("ScrubAreasIterator::next: return None(2)");
                return None;
            }

//println!("ScrubAreasIterator::next: i {}, cur_index {} create ScrubAreaIterator", self.i, self.cur_index);
//println!("   ScrubAreasIterator::next:: ScrubArea [{:p}-{:p}]", self.scrub_areas[self.i].start, self.scrub_areas[self.i].end);
//println!("   cacheline_size {}", self.cache_desc.borrow().cacheline_size());

            self.iterator =
                ScrubAreaIterator::<T, U>::new(self.cache_desc.clone(),
                &self.scrub_areas[self.i], self.cur_index);
        }
    }
}

// ScrubAreaIterator to scan a ScrubArea, keeping on a single cache line as
// long as possible. This means that we increment the addresses we can by
// the cache size each time to keep the cache index constant.
//
// cache_desc   Description of the cache
// scrub_area:  Specifies the address of the scrub area
// i:           Number of cache line-sized items we've scanned
// cur_index:   Cache index we are scrubbing
pub struct ScrubAreaIterator<'a, T, U> {
    cache_desc: Rc<RefCell<&'a mut T>>,
    scrub_area: &'a ScrubArea,
    i:          usize,
    cur_index:  usize,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheDesc<U>. So, this
    // works around that problem
    _marker:    PhantomData<U>,
}

impl<'a, T: CacheDesc<U>, U: Cacheline> ScrubAreaIterator<'a, T, U> {
    // Create a new ScrubAreaIterator.
    // cache_desc: Description of the cache
    // scrub_area: Memory over which we Iterate
    //
    // Returns: Ok(ScrubAreaIterator) on success, Err(Error) on failure
    pub fn new(cache_desc: Rc<RefCell<&'a mut T>>,
        scrub_area: &'a ScrubArea, cur_index: usize) ->
        ScrubAreaIterator<'a, T, U> {

        ScrubAreaIterator {
            cache_desc: cache_desc,
            scrub_area: scrub_area,
            i:          0,
            cur_index:  cur_index,
            _marker:    PhantomData,
        }
    }
}

// Return a pointer into the next memory area of cache line size
impl<'a, T: CacheDesc<U>, U: Cacheline> iter::Iterator for
    ScrubAreaIterator<'a, T, U> {
    type Item = *const U;

    fn next(&mut self) -> Option<Self::Item> {
//println!("ScrubAreaIterator::next: entered");
        let cache_lines = self.cache_desc.borrow().cache_lines();
        let start_index = self.cache_desc.borrow()
            .cache_index(self.scrub_area.start);

        // Compute the delta from self.scrub_area to the next highest address
        // whose cache index is the one we are currently scrubbing. Then add
        // to that the value of i times the number of cachelines it takes to
        // get to the next address with the same cache index. So, for all values
        // of i, we get the addresses with the same cache index.
//println!("self.cur_index {:x} start_index {:x}", self.cur_index, start_index);
        let delta = if self.cur_index >= start_index
            {
//println!("cur_index {} after or at start index {}", self.cur_index, start_index);
                self.cur_index - start_index
            } else {
//println!("cur_index {} before start index {}", self.cur_index, start_index);
                (self.cur_index + cache_lines) - start_index 
            };

        let cur_offset = delta + self.i * cache_lines;
//println!("delta {:x} self.i {:x} cur_offset {:x}", delta, self.i, cur_offset);

        // If the offset less than than the size of the scrub_areas, we have
        // data
        let size_in_cachelines = self.cache_desc.borrow()
            .size_in_cachelines(&self.scrub_area);
//println!("cur_offset {:x} size_in_cachelines {:x}", cur_offset, size_in_cachelines);
        if cur_offset < size_in_cachelines {
            self.i += 1;
            let cacheline_width = self.cache_desc.borrow().cacheline_width();
            let p = self.scrub_area.start as usize +
                (cur_offset << cacheline_width);
//println!("ScrubAreaIterator::next: return {:p}", p as *const U);
            return Some(p as *const U)
        }

//println!("ScrubAreaIterator::next: return None");
        None
    }
}

#[cfg(test)]
mod tests {
    use std::cell::{RefCell};
    use std::ptr;
    use std::rc::Rc;
    use std::slice;
    use std::time::Instant;

    use crate::{Addr, AutoScrub, AutoScrubDesc, CacheDesc, Cacheline, Error,
        MemoryScrubber, ScrubArea, ScrubAreaIterator, ScrubAreasIterator,
        ScrubLinesIterator};

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

    #[repr(C)]
    struct BasicCacheline {
        data:   [BasicECCData;
            (1 << BASIC_CACHELINE_WIDTH) / std::mem::size_of::<BasicECCData>()],
    }

    impl Cacheline for BasicCacheline {
    }

    // BasicCacheDesc - Description of the cache for basic tests
    // cache_index_width - Number of bits of the cache index.
    #[derive(Clone, Copy)]
    struct BasicCacheDesc {
        cache_index_width:        usize,
    }

    impl CacheDesc<BasicCacheline> for BasicCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&mut self, cacheline_ptr: *const BasicCacheline) {
            let cacheline = unsafe {
                &*cacheline_ptr
            };
            let cacheline_data = &cacheline.data[0];
            let _dummy = unsafe {
                ptr::read(cacheline_data)
            };
        }
    }

    // Cache descriptor to pass to the memory scrubbing functions.
    static BASIC_CACHE_DESC: BasicCacheDesc = BasicCacheDesc {
        cache_index_width:        BASIC_CACHE_INDEX_WIDTH,
    };

    // Description of memory that is read into by the read_cacheline() function.
    // This keeps the actually allocation together with the pointer into that
    // allocation so that things go out of scope at the same time.
    //
    // mem_area - Vec<u8> of elements that can be read by read_cacheline()
    // start - Cache size-aligned pointer of the first byte to use in mem_area
    // end - Pointer to the last byte
    #[derive(Clone)]
    struct Mem {
        mem_area:   Vec<u8>,
        scrub_area: ScrubArea,
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
        fn new<T>(size: usize) -> Result<Mem, Error> {
            let cacheline_size = std::mem::size_of::<T>();
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
            let mem_area: Vec<u8> = vec![0; alignment_size + size];

            // Now find the first cache line aligned pointer
            let start_addr = (mem_area.as_ptr() as Addr + alignment_size - 1) &
                !(alignment_size - 1);
            let start = start_addr as *const u8;
            let end = (start_addr + size - 1) as *const u8;

            Ok(Mem {
                mem_area:   mem_area,
                scrub_area: ScrubArea { start: start, end: end },
            })
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for Mem {}

    // Data structure for keep track of how many times a given address has
    // been read
    // mem:     Boundaries of the address covered by this structure
    // n_reads: Counters
    #[derive(Clone)]
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

    // Cache characteristics
    // TOUCHING_CACHELINE_WIDTH - number of bits required to index a byte in a
    //  cache line
    // TOUCHING_CACHELINE_SIZE - Number of bytes in a cache line
    // TOUCHING_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    // TOUCHING_CACHE_LINES - number of cache lines
    const TOUCHING_ECCDATA_WIDTH: usize = usize::BITS as usize - 1 -
        std::mem::size_of::<TouchingECCData>() .leading_zeros() as usize;
    const TOUCHING_CACHELINE_WIDTH: usize = 3 + TOUCHING_ECCDATA_WIDTH;
    const TOUCHING_CACHELINE_SIZE: usize = 1 << TOUCHING_CACHELINE_WIDTH;
/* FIXME: restore this
    const TOUCHING_CACHE_INDEX_WIDTH: usize = 10;
*/
    const TOUCHING_CACHE_INDEX_WIDTH: usize = 2;
    const TOUCHING_CACHE_LINES: usize = 1 << TOUCHING_CACHE_INDEX_WIDTH;

    // GUARD_LINES - Number of cache line size items we allocate but don't
    //      touch, to verify we don't touch the wrong place.
    // TOUCHING_CACHE_NUM_TOUCHED - Number of cache footprints we use for
    //      testing
    // TOUCHING_SANDBOX_SIZE - Number of cachelines we actually expect to
    //      touch
    // TOUCHING_COOKIE - Cookie written to all memory we're scrubbing
    const GUARD_LINES: usize = TOUCHING_CACHE_LINES;
    const TOUCHING_CACHE_NUM_TOUCHED: usize = 3;
    const TOUCHING_SANDBOX_SIZE: usize =
        TOUCHING_CACHE_LINES * TOUCHING_CACHE_NUM_TOUCHED;
    const TOUCHING_COOKIE: TouchingECCData = 17;

    // TouchingECCData - The data size used to compute the ECC for basic tests
    type TouchingECCData = u64;
    const TOUCHING_ECC_DATA_SIZE: usize =
        std::mem::size_of::<TouchingECCData>();

    // TouchingCacheline - the data type of a cache line
    #[repr(C)]
    struct TouchingCacheline {
        data:   [TouchingECCData;
            (1 << TOUCHING_CACHELINE_WIDTH) / TOUCHING_ECC_DATA_SIZE],
    }

    impl Cacheline for TouchingCacheline {
    }

    // Type used for the read counter
    type NRead = u8;

    // TouchingCacheDesc - Description of the cache for basic tests
    // cache_index_width - Number of times this cacheline was iit during the
    //      scrub
    // read_infos:          Array of ReadInfo items>
    struct TouchingCacheDesc {
        cache_index_width:  usize,
        read_infos:         Vec<ReadInfo>,
    }

    impl TouchingCacheDesc {
        // Set up a new TouchingCacheDesc
        // sizes: array of sizes of memory areas
        fn new<TouchingCacheline>(sizes: &[usize]) ->
            Rc<RefCell<TouchingCacheDesc>> {
            let touching_cache_desc = Rc::new(RefCell::new(TouchingCacheDesc {
                cache_index_width: TOUCHING_CACHE_INDEX_WIDTH,
                read_infos: Vec::<ReadInfo>::new(),
            }));

            for size in sizes {
                let mem =
                    match Mem::new::<TouchingCacheline>(*size) {
                    Err(e) => panic!("Memory allocation error: {}", e),
                    Ok(mem) => mem,
                };

                let scrub_area_size = touching_cache_desc.borrow()
                    .size_in_cachelines(&mem.scrub_area);
                let n_reads_size = GUARD_LINES + scrub_area_size + GUARD_LINES;
                touching_cache_desc.borrow_mut().read_infos.push(ReadInfo::new(n_reads_size, mem));
            }

            touching_cache_desc
        }

        // Compute the index into the n_read array for this address. This
        // array has GUARD_LINES elements surrounding the actual counts.
        // cacheline_ptr: Pointer to the address
        fn read_index(&mut self, cacheline_ptr: *const TouchingCacheline) ->
            usize {
            let cacheline_addr = cacheline_ptr as Addr;
            let cacheline_size = {
                self.cacheline_size ()
            };

            let read_info = self.find_read_info(cacheline_ptr);
            let scrub_area = read_info.mem.scrub_area.clone();
            let n_n_reads = self.size_in_cachelines(&scrub_area);
            let start_addr = scrub_area.start as Addr;

            let index = (cacheline_addr - start_addr) / cacheline_size;
//println!("index {} n_n_reads {}", index, n_n_reads);
            assert!(index < n_n_reads);
            index
        }

        // Returns a reference to n_reads[], the array of count read counts
        fn get_n_reads<'a>(&'a mut self,
            cacheline_ptr: *const TouchingCacheline) ->
            &'a mut Vec<NRead> {
            let read_info = self.find_read_info(cacheline_ptr);
            read_info.n_reads.as_mut().unwrap()
        }

        // Find a ReadInfo corresponding to a given location in memory
        fn find_read_info<'a>(&'a mut self,
            cacheline_ptr: *const TouchingCacheline) ->
            &'a mut ReadInfo {
            let cacheline_addr = cacheline_ptr as Addr;
            
            for search_read_info in self.read_infos.iter_mut() {
                let scrub_area = &search_read_info.mem.scrub_area;
                let start_addr = scrub_area.start as Addr;
                let end_addr = scrub_area.end as Addr;

                if cacheline_addr >= start_addr &&
                    cacheline_addr <= end_addr {
                    return search_read_info;
                }
            }

            // If we failed, it's because the cache addess wasn't in any of
            // the ScrubAreas.
            panic!("Unable to find address {:x}", cacheline_addr);
        }
    }

    impl CacheDesc<TouchingCacheline> for TouchingCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&mut self, cacheline_ptr: *const TouchingCacheline) {
            // Do the read
            let _dummy = unsafe {
                ptr::read(&(*cacheline_ptr).data[0]);
            };

            // Now we do something *really* bad--write to a read only section
            // of memory. But, this is for testing purposes, so sometimes you
            // just gotta break the law.
            unsafe {
                let cacheline_write_ptr =
                    cacheline_ptr as *mut TouchingCacheline;
                ptr::write(&mut (*cacheline_write_ptr).data[0],
                    TOUCHING_COOKIE);
            }

            // Update the read count
            let index = {
                self.read_index(cacheline_ptr)
            };
            let n_reads = {
                self.get_n_reads(cacheline_ptr)
            };

//println!("read_cacheline: i {} cacheline_ptr {:p}", GUARD_LINES + index, cacheline_ptr);
            n_reads[GUARD_LINES + index] += 1;
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl Sync for TouchingCacheDesc {}

    impl Clone for TouchingCacheDesc {
        fn clone(&self) -> Self {
            let read_infos = self.read_infos.clone();
            TouchingCacheDesc {
                cache_index_width:  self.cache_index_width,
                read_infos:         read_infos,
            }
        }
    }

    // Verify that an error is returned if the starting address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_start() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let mut mem =
            match Mem::new::<BasicCacheline>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        mem.scrub_area.start = unsafe {
            mem.scrub_area.start.offset(1)
        };

        let scrub_areas = [mem.scrub_area];
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedStart);
    }

    // Verify that an error is returned if the ending address is not
    // aligned on a cache line boundary
    #[test]
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

        let scrub_areas = [mem.scrub_area];
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::UnalignedEnd);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_null_areas() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();

        let scrub_areas = [];
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::NoScrubAreas);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let mut mem =
            match Mem::new::<BasicCacheline>(BASIC_MEM_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        mem.scrub_area.end = mem.scrub_area.start;

        let scrub_areas = [mem.scrub_area];
        let memory_scrubber =
            MemoryScrubber::<BasicCacheDesc, BasicCacheline>::new(basic_cache_desc,
            &scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(),
            Error::EmptyScrubArea);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        let basic_cache_desc = &mut BASIC_CACHE_DESC.clone();
        let cacheline_size = basic_cache_desc.cacheline_size();
        let mem =
            match Mem::new::<BasicCacheline>(basic_cache_desc.cacheline_size() *
                basic_cache_desc.cache_lines() * 14) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };

        let scrub_areas = [mem.scrub_area];
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
    #[test]
    fn test_touch_zero() {
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = 0;
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_one() {
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = cacheline_size;
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_many() {
        const MANY: usize = 50;
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = cacheline_size * MANY;
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_all() {
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = cacheline_size * TOUCHING_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_double_all() {
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = 2 * cacheline_size * TOUCHING_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_more_many() {
        const MANY: usize = 72;
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = 5 * cacheline_size * (TOUCHING_SANDBOX_SIZE + MANY);
        test_scrubber(&[cacheline_size * TOUCHING_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_multiple_areas() {
        const MANY: usize = 72;
        let cacheline_size = TOUCHING_CACHELINE_SIZE;
        let first_area = 2 * cacheline_size * (TOUCHING_SANDBOX_SIZE + MANY);
        let second_area = cacheline_size * TOUCHING_SANDBOX_SIZE;
        let third_area = cacheline_size * MANY;
        let scrub_areas = [first_area, second_area, third_area];
        test_scrubber(&scrub_areas, first_area);
    }

    #[test]
    fn test_big() {
        const MEM_AREA_SIZE: usize = 1 * 1024 * 1024 * 1024;

        let mut basic_cache_desc = BASIC_CACHE_DESC.clone();
        let cache_desc = &mut basic_cache_desc;
        let mem =
            match Mem::new::<BasicCacheline>(MEM_AREA_SIZE) {
            Err(e) => panic!("Memory allocation error: {}", e),
            Ok(mem) => mem,
        };
        let scrub_areas = [mem.scrub_area];
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

    #[test]
    fn test_autoscrub() {
        const CACHELINE_SIZE: usize = std::mem::size_of::<TouchingCacheline>();
        const ONE_SIZE: usize = TOUCHING_CACHE_LINES *
            TOUCHING_CACHE_NUM_TOUCHED * CACHELINE_SIZE;
        const SINGLE_SCAN: usize = ONE_SIZE / 2;
        const TOTAL_SCAN: usize = SINGLE_SCAN * 4 + 3 * CACHELINE_SIZE;

        let sizes = [ONE_SIZE, ONE_SIZE, ONE_SIZE];
        let (touching_cache_desc, scrub_areas) =
            setup_touching_desc_areas(&sizes);
        let cache_desc = &mut *touching_cache_desc.borrow_mut() as
            &mut TouchingCacheDesc;
        struct TestAutoScrubDesc {
            count: usize,
            scrub_size: usize,
        }

        impl AutoScrubDesc for TestAutoScrubDesc {
            fn next(&mut self) -> usize {
                let n = if self.count > self.scrub_size { self.scrub_size }
                    else { self.count };
                self.count -= n;
                n
            }
        }

        let mut autoscrub_desc = TestAutoScrubDesc {
            count: TOTAL_SCAN,
            scrub_size: SINGLE_SCAN,
        };

        let mut autoscrub = match AutoScrub::new(cache_desc, &scrub_areas,
            &mut autoscrub_desc) {
            Err(e) => panic!("AutoScrub::new failed: {}", e),
            Ok(autoscrub) => autoscrub,
        };
        match autoscrub.scrub() {
            Err(e) => panic!("autoscrub() failed: {}", e),
            Ok(_) => {},
        };
        verify_scrub(&autoscrub.scrubber, TOTAL_SCAN);
    }

    // Test iterators
    const ITER_CACHE_INDEX_WIDTH: usize = 4;

    struct IterCacheline {
        data: [u16; 8],
    }

    impl Cacheline for IterCacheline {
    }

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

    impl CacheDesc<IterCacheline> for IterCacheDesc {
        fn cache_index_width(&self) -> usize {
            self.cache_index_width
        }

        fn read_cacheline(&mut self, cacheline_ptr: *const IterCacheline) {
            // Assure that IterCacheline::data is read
            unsafe {
                assert_eq!((*cacheline_ptr).data[0], (*cacheline_ptr).data[0]);
            }
        }
    }


    #[test]
    fn test_iter_scrub_areas() {
        struct TestData<'a> {
            cur_index:  usize,
            expected:   &'a [usize],
        }

        let mut iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);
        let cache_desc = Rc::new(RefCell::new(&mut iter_cache_desc));

        let cl_width = cache_desc.borrow().cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = cache_desc.borrow().cache_index_width();
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
            ScrubArea::new(s1, e1),
            ScrubArea::new(s2, e2),
            ScrubArea::new(s3, e3),
            ScrubArea::new(s4, e4),
            ScrubArea::new(s5, e5),
        ];

        let expected1 = [base1 + 0 * c_size];
        let expected2 = [base1 + 0 * c_size + 1 * cl_size,
            base2 + 0 * c_size + 0 * cl_size];
        let expected3 = [base2 + 0 * c_size + 1 * cl_size, base3 + 0 * c_size,
            base3 + 1 * c_size, base3 + 2 * c_size, base3 + 3 * c_size];

        print_scrub_areas(*cache_desc.borrow(), &phys_scrub_areas);
        let test_data:  [TestData; 3] = [
            TestData {cur_index: 0, expected: &expected1},
            TestData {cur_index: 1, expected: &expected2},
            TestData {cur_index: 2, expected: &expected3},
        ];

        for test_datum in test_data {
//println!("--- {}", cnt);
            let iterator = ScrubAreasIterator::new(cache_desc.clone(),
                &phys_scrub_areas, test_datum.cur_index);
//println!("test_iter_scrub_areas: cur_index {}", test_datum.cur_index);
/*
print!("expected [");
let mut sep = "";
for e in test_datum.expected {
    print!("{}{:x}", sep, e);
    sep = ", ";
}
println!("]");
*/

            // Scan the entire set of ScrubAreas
            let mut count = 0;
            for (actual, expected) in
                iterator.into_iter().zip(test_datum.expected.iter()) {
                let expected_p = *expected as *const IterCacheline;
//println!("test_iter_scrub_areas: count {} actual {:p} expected {:p}", count + 1, actual, expected_p);
                assert_eq!(actual, expected_p);
                count += 1;
            };
//println!("test_iter_scrub_areas: final count {}", count);
            assert_eq!(count, test_datum.expected.len());
        }
    }

    #[test]
    fn test_iter_scrub_area() {
        struct TestData<'a> {
            cur_index:  usize,
            scrub_area: &'a ScrubArea,
            expected:   &'a[usize],
        }
        let mut iter_cache_desc = IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);
        let cache_desc = Rc::new(RefCell::new(&mut iter_cache_desc));

        let cl_width = cache_desc.borrow().cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = cache_desc.borrow().cache_index_width();
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
//println!();
//println!("base2 {:x} (c_size - delta2) {}", base2, c_size - delta2);

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

        let test_data: [TestData; 7] = [
            TestData { cur_index: 0, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected1},
            TestData { cur_index: 1, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected2},
            TestData { cur_index: 2, scrub_area: &phys_mem1.scrub_area.clone(),
                expected: &expected3},
            TestData { cur_index: 0, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected4},
            TestData { cur_index: 1, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected5},
            TestData { cur_index: 2, scrub_area: &phys_mem2.scrub_area.clone(),
                expected: &expected6},
            TestData { cur_index: 1, scrub_area: &phys_mem3.scrub_area.clone(),
                expected: &expected7},
        ];


        for test_datum in test_data {
//println!();
            print_scrub_areas(*cache_desc.borrow(),
                &[test_datum.scrub_area.clone()]);
            let sax = &test_datum.scrub_area;
            let cd = cache_desc.clone();
            let cur_index = test_datum.cur_index;
//println!("cur_index {}", cur_index);
/*
print!("start {:p} expected [", sax.start);
let mut sep = "";
for e in test_datum.expected {
    print!("{}{:x}", sep, e);
    sep = ", ";
}
println!("]");
*/
            let iterator = ScrubAreaIterator::new(cd, sax, cur_index);

            // Scan the entire set of ScrubAreas
            let mut count = 0;
            for (actual, expected) in
                iterator.into_iter().zip(test_datum.expected.iter()) {
                let expected_p = *expected as *const IterCacheline;
//println!("actual {:p} expected {:p}", actual, expected_p);
                assert_eq!(actual, expected_p);
                count += 1;
            };
            assert_eq!(count, test_datum.expected.len());
        }
    }

    fn print_scrub_areas(_cache_desc: &IterCacheDesc,
        _phys_scrub_areas: &[ScrubArea]) {
/*
        let mut total_bytes: usize = 0;
        let cacheline_width = cache_desc.cacheline_width();

        // Print the tuples in the vector
        println!("Physical addresses:");
        for phys_scrub_area in phys_scrub_areas {
            let delta_cachelines =
                cache_desc.size_in_cachelines(&phys_scrub_area);
            let delta_bytes = delta_cachelines << cacheline_width;
            total_bytes += delta_bytes;
            println!("{:p}-{:p}: {} ({} cache lines)", phys_scrub_area.start,
                phys_scrub_area.end, delta_bytes, delta_cachelines);
        }

        println!("total size {} bytes ({} cache lines)", total_bytes,
            total_bytes >> cacheline_width);
*/
    }

    #[test] #[ignore]
    fn test_iter() {
        let iter_cache_desc = &mut IterCacheDesc::new(ITER_CACHE_INDEX_WIDTH);
        let cache_desc = Rc::new(RefCell::new(iter_cache_desc));

        let cl_width = cache_desc.borrow().cacheline_width();
        let cl_size = 1 << cl_width;
        let ci_width = cache_desc.borrow().cache_index_width();
        let c_size = cl_size * (1 << ci_width);

        let s1 = (2 * cl_size) as * const u8;
        let e1 = (2 * cl_size + 3 * c_size) as *const u8;
        let s2 = (4 * c_size) as *const u8;
        let e2 = (4 * c_size + cl_size) as *const u8;
        let s3 = (5 * c_size) as *const u8;
        let e3 = (5 * c_size + 2 * cl_size) as *const u8;
        let virt_scrub_areas = [
            ScrubArea::new(s1, e1),
            ScrubArea::new(s2, e2),
            ScrubArea::new(s3, e3),
        ];

        let mut total_bytes: usize = 0;
        for scrub_area in &virt_scrub_areas {
            let delta = cache_desc.borrow().size_in_cachelines(&scrub_area);
            total_bytes += delta;
        }
        total_bytes *= cache_desc.borrow().cacheline_size();

        // Print the tuples in the vector
        println!("Physical addresses:");
        for scrub_area in &virt_scrub_areas {
            let size_in_cachelines = cache_desc.borrow().size_in_cachelines(&scrub_area);
            println!("{:p}-{:p}: {} ({} cache lines)", scrub_area.start,
                scrub_area.end,
                size_in_cachelines << cache_desc.borrow().cacheline_width(),
                size_in_cachelines);
        }
        println!("total size {}", total_bytes);

        let cur_indices = [0];

        for cur_index in cur_indices {
            println!("Scrubbing with index {}", cur_index);
            let mut iterator = ScrubLinesIterator::new(cache_desc.clone(), &virt_scrub_areas);

            // Scan the entire set of ScrubAreas
            for _i in 0..total_bytes >> cache_desc.borrow().cacheline_width() {
                let next = iterator.next();
                let p = next.unwrap();
                println!("p {:p}", p);
            };
        }
    }

    // Test support function that scrubs a section of memory, then verifies that
    // things were properly referred.
    // sizes - array of sizes of memory areas to scrub
    // n - number of cache lines to scrub
    fn test_scrubber(sizes: &[usize], n: usize) {
        let (touching_cache_desc, scrub_areas) =
            setup_touching_desc_areas(sizes);
        let cache_desc = &mut *touching_cache_desc.borrow_mut() as
            &mut TouchingCacheDesc;

        let mut memory_scrubber = {
            match MemoryScrubber::<TouchingCacheDesc, TouchingCacheline>
                ::new(cache_desc, &scrub_areas) {
                Err(e) => panic!("MemoryScrubber::new() failed {}", e),
                Ok(scrubber) => scrubber,
            }
        };

        if let Err(e) = memory_scrubber.scrub(n) {
            panic!("scrub failed: {}", e);
        };

        verify_scrub(&memory_scrubber, n);

        // Ensure that mem_area is used
        for read_info in &memory_scrubber.cache_desc.borrow().read_infos {
            let mem_area = &read_info.mem.mem_area;
            assert_ne!(mem_area[0], mem_area[0]);
        }
    }

    // Set up a TouchingCacheDesc and ScrubAreas
    fn setup_touching_desc_areas (sizes: &[usize]) ->
        (Rc<RefCell<TouchingCacheDesc>>, Vec<ScrubArea>) {
        let touching_cache_desc =
            TouchingCacheDesc::new::<TouchingCacheline>(sizes);

        // Allocate memory areas according to the given sizes
        let mut scrub_areas: Vec<ScrubArea> = vec!();
        {
            for read_info in &touching_cache_desc.borrow_mut().read_infos {
                scrub_areas.push(read_info.mem.scrub_area.clone());
            }
        }

        (touching_cache_desc, scrub_areas)
    }

    // Verify the proper locations were hit
    // memory_scrubber - MemoryScrubber to use
    // n - bytes scrubbed
    //
    // This essentially reimplements the iterator code but in a more straight-
    // forward way so that the two implements can verify each other.
    fn verify_scrub(memory_scrubber: &MemoryScrubber<TouchingCacheDesc, TouchingCacheline>,
        n: usize) {
        let cacheline_width =
            memory_scrubber.cache_desc.borrow().cacheline_width();
        let n_in_cachelines = n >> cacheline_width;

        // Count the total number of scrub lines in all of the ScrubAreas
        let mut scrub_lines = 0;

        for scrub_area in memory_scrubber.iterator.scrub_areas {
            scrub_lines +=
                memory_scrubber.cache_desc.borrow()
                .size_in_cachelines(scrub_area);
        }

        let n_min_reads: NRead = match (n_in_cachelines / scrub_lines)
            .try_into() {
            Err(e) => panic!("Internal Error: n_min_reads conversion failed: {}", e),
            Ok(n_min_reads) => n_min_reads,
        };
        let n_extra_reads = n_in_cachelines % scrub_lines;

        let mut verified = 0;

        for read_info in &memory_scrubber.cache_desc.borrow().read_infos {
//println!(">>>>> verify_read_info for {}", i);
            verify_read_info(&memory_scrubber, &read_info,
                n_min_reads, n_extra_reads, verified);
            verified += memory_scrubber.cache_desc.borrow()
                    .size_in_cachelines(&read_info.mem.scrub_area);
//println!("<<<<<");
        }
    }

    // Verify that one scrub area is correct
    // memory_scrubber: The MemoryScrubber being verified
    // read_info: A ReadInfo being verified
    // n_min_reads: The minimum number of reads in the vectors in n_reads
    // n_extra_reads: The number of items in the vectors in n_reads which
    //      are greater than n_min_reads by one
    // verified: The number of items in the vectors of n_reads that have already
    //       been verified
    fn verify_read_info<'a>(memory_scrubber: &MemoryScrubber<TouchingCacheDesc, TouchingCacheline>,
        read_info: &ReadInfo, n_min_reads: NRead, n_extra_reads: usize,
        mut verified: usize) {
        let cache_desc = &memory_scrubber.cache_desc;
        let cache_lines = {
            cache_desc.borrow().cache_lines()
        };
        let scrub_area = &read_info.mem.scrub_area;
        let size_in_cachelines = cache_desc.borrow()
            .size_in_cachelines(&scrub_area);
        let verified_end = verified + size_in_cachelines;

        let mem_lines = read_info.mem.scrub_area.start as
            *const TouchingCacheline;
        let mem = unsafe {
            slice::from_raw_parts(mem_lines, size_in_cachelines)
        };

        let n_reads = &read_info.n_reads.as_ref().unwrap();
//println!("GUARD_LINES {} n_reads.len() {}", GUARD_LINES, n_reads.len());
        verify_guard(n_reads, 0);

        // Now verify the contents of the memory to see whether they were
        // touched the expected number of times. The number of hits for a
        // location i in n_reads[] will be at least equal to the number of
        // complete scans of the memory area. Then, the remaining number of
        // items in the scan will be one larger.
        'scan_lines: for line in 0..cache_lines {
            for i in (line..size_in_cachelines).step_by(cache_lines) {
                let inc = if verified < n_extra_reads { 1 } else { 0 };
                let n_expected: NRead = n_min_reads + inc;
                let n_actual = n_reads[GUARD_LINES + i];
                assert_eq!(n_actual, n_expected as u8);

//println!("mem[{}].data[0] {:p}", i, mem[i].data.as_ptr() as *const u8);
//println!("n_min_reads {} n_extra_reads {}", n_min_reads, n_extra_reads);
                let mem_actual = mem[i].data[0];
                let mem_expected =
                    if n_min_reads == 0 && verified >= n_extra_reads { 0 }
                    else { TOUCHING_COOKIE };

                assert_eq!(mem_actual, mem_expected);
                for data in &mem[i].data[1..] {
                    let mem_actual = *data;
                    assert_eq!(mem_actual, 0);
                }

//println!("verified {} verified_end {}", verified, verified_end);
                verified += 1;
                if verified == verified_end {
//println!("break");
                    break 'scan_lines;
                }
            }
        }

        verify_guard(n_reads, GUARD_LINES + TOUCHING_SANDBOX_SIZE);
    }

    // Verify a guard area before the memory area. This should
    // not have been seen and so should have a zero value
    // n_reads - Array that should have all zero values at the offset for
    //      GUARD_LINES elements
    // offset - Offset in n_reads to check
    fn verify_guard(n_reads: &Vec<NRead>, offset: usize) {

//println!("GUARD_LINES {} offset {} TOUCHING_SANDBOX_SIZE {}", GUARD_LINES, offset, TOUCHING_SANDBOX_SIZE);
        for i in 0..GUARD_LINES {
            let actual = n_reads[offset + i];
if actual != 0 {
    println!("verify_guard: offset + i {} actual {} expected {}", offset + i, actual, 0);
}
            assert_eq!(actual, 0);
        }
    }
}
