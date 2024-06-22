/*
// FIXME: replace panic!() will something else
// FIXME: Invoke check_cache_params() in CacheBase.
// FIXME: Convert Vec<ScrubArea> to [ScrubArea]
// FIXME: Remove all #[ignore]
// FIXME: add replace_scrub_area()
// FIXME: use <const n> for test data types
// FIXME: verify that the number of ways works to flush cache lines
// FIXME: comment about having to map virtual to physical and to not
//      bypass the cache.
// FIXME: Make  the default
// FIXME: Note that the first pass may not result in a full flush because
//        some data might already be loaded in cache. Still, this is unlikely.
// FIXME: Verify that all PhantomDatas are needed
// FIXME: Verify that check_.*_params() cover all cases.
// FIXME: Incorporate memory mapping, perhaps in a two-level approach where
//        there are a set of mapping areas, which then include scrubbing
//        areas.
// FIXME: In kernel or not? Using /dev/mem opens a potential security
//        vulnerability, Putting in the kernel is extra crap, but also
//        supports the possibility of an ECC self-test interface in /sys.
// FIXME: Drop all trailing underscores
// FIXME: remove this note
//  ReadInfos have a Mem and a Vec<NRead>
//  A Mem has a allocated area and a MemArea
//  MemAreas have start and end VAddr
//  scrub_areas references to a MemArea
//  TestMemoryScrubber wants a reference to an array of MemAreas
//  If I have sizes, I can create a Mem and push that to an array of ReadInfos
//  sizes->Mem
//  Mem->ReadInfos
//  TestMemoryScrubber has an array of MemArea, that is, &[MemArea]
//---
//      let mut mem = Mem::new::<TestCacheline<D, S>, D, S>(MEM_SIZE);
//      let mut scrub_areas = Vec::<MemArea<VAddr>>::new();
//      scrub_areas.push(mem.scrub_area);
// I think I need to drop ReadInfos and instead make a TestMemArea.
//
// This is code for a memory scrubber.
//
// INTRODUCTION
// ============
// What is a memory scrubber and why would you use one?
//
// RAM is often manipulated as a small number, 4-8, bytes. For each of these
// groups of bytes, an error correction code (ECC) is computed and stored with
// the data. When the processor reads data, the ECC is also read. The ECC is
// used to correct errors and write the corrected data back to RAM. Each ECC
// implementation will have a limit on the total number of errors it can
// correct for a particular group of bytes. Since errors accumulate over time,
// data that has not been read in a long time may have so many errors that it
// is no longer possible to determine what the correct data was. A memory
// scrubber is responsible for reading all ECC-protected data at a rate fast
// enough that the number of errors to be corrected remains within the ability
// of the ECC code used to correct it.
//
// This memory scrubber is specifically designed to reduce the performance
// impact of memory scrubbing. Let us take the case of a system with 16 MB
// memory--one large enough to run a minimal Linux system. The processor's
// cache has 64-byte cache lines, 4 ways, and 1024 cache lines.
//
// A simple memory scrubber might sequentially read data one cache line
// apart, resulting in 16 MB / 1024 = 16384 reads. The cache is completely
// evicted after the product of the number of ways and the number of cache
// lines, i.e 4 * 1024 = 4096. So, the entire cache is evicted 4096 times
// for a scrub of all of memory. Tasks running in parallel with the simple
// memory scrubber may derive little benefit from the cache during this period.
//
// The  memory scrubber implemented is cache aware. It does the same number of
// reads as the simple approach, but it only evicts all of cache once. For our
// example, it evicts cache at a rate 1 / 4096 = 0.02% of the simple memory
// scrubber. For more capable systems, the difference becomes even larger.
// The amount of benefit actually achieved is heavily dependent on the extent
// to which the memory scrubber runs in parallel with other tasks. Still, even
// if this is rate, the cache-aware memory scruber is unlikely to create a
// sudden drop in cache effectiveness. If this happens at a critical time, the
// systems could be adversely affected.
//
// NOTE: This is not intended to cover all possible cache
// implementations. Code to cover other variations is welcome.
//
// QUICK START
// ===========
// 1.   Add the lines:
//          use libmemscrub_arch::{CacheBase, Cacheline,
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
// 2.   Define and implement a CacheBase structure for your cache line. This
//      requires implementing the function cache_index_width(), which returns
//      the cache line width determined above, and read_cacheline(), which
//      causes the entire cacheline to be read:
//
//          struct MyCacheBase {
//              cache_index_width: usize,
//          }
//
//          impl CacheBase for MyCacheBase {
//              fn cache_index_width(&self) -> usize { self.cache_index_width }
//              fn read_cacheline(&self,
//                  _cacheline_ptr: *const MyCacheline) {
//                  let cacheline = unsafe { &*_cacheline_ptr };
//                  let cacheline_data = &cacheline.data[0];
//                  let _dummy = unsafe { ptr::read(cacheline_data) };
//              }
//          }
//
//          static MY_CACHE_DESC: MyCacheBase = MyCacheBase {
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
//                  scrub_size: VAddr,
//              }
//
//              impl AutoScrubDesc for MyAutoScrubDesc {
//                  fn next(&mut self) -> VAddr {
//                      self.scrub_size
//                  }
//              }
//
//              let mut my_autoscrub_desc = MyAutoScrubDesc {
//                  scrub_size: my_cache.cacheline_size() * 5000,
//              };
//
//      c.  Invoke autoscrub():
//
//              let my_cache = MyCacheBase.clone();
//              let scrub = AutoScrub.autoscrub(&mut my_cache,
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
//      an implementation of the CacheBase trait. For example purposes, call
//      this MyCacheBase. In most cases, the default functions provide
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
//              fn read_cacheline(&self, _cacheline_ptr: *const MyCacheline) {
//                  // Get a safe reference to the cache line
//                  let cacheline = unsafe {
//                      &*_cacheline_ptr
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
//              new(&MyCacheBase::<MyCacheline> {...}, my_start, my_end) {
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
//  Assume a memory with S words of S bits, with the probability that a single
//  bit will be flipped in time Tf is P. What is probability that at least one
//  word will have more than S bits flipped in the interval T?
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
//  - `S` be the number of bits in a word.
//  - `P` be the probability that a single bit will be flipped in time Tf.
//  - `S` be the maximum number of flipped bits in a word.
//  - `k` be the number of bits flipped in a word (0 <= k <= S).
//
//  The probability that exactly `k` bits are flipped in a single word can be
//  calculated using the binomial distribution formula:
//
//  ```
//  P(k) = C(S, k) * (1 - P)^(S - k) * P^k
//  ```
//
//  Where `C(S, k)` is the binomial coefficient, given by:
//
//  ```
//  C(S, k) = S! / (k! * (S - k)!)
//  ```
//
//  The probability that a single word doesn't have more than `S` bits flipped
//  is the sum of probabilities for `k` from 0 to `S`:
//
//  ```
//  P_single_word = Σ(P(k)) for k = 0 to N
//  ```
//
//  Finally, the probability that at least one word has more than `S` bits
//  flipped in the interval T can be calculated using the complement rule:
//
//  ```
//  P_at_least_one_word = 1 - (1 - P_single_word)^S
//  ```
//
//  Keep in mind that these formulas involve factorials and exponentials, which
//  can lead to large computations for larger values of `S`, `S`, and `S`. You
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
// memory is S * (S / 8).
//
// NOTE: The above assumes that, once a bit is flipped, it stays flipped. The
// probability of a bit being inverted, then inverted again is small enough
// that it can be ignored.
*/

//#[macro_use]
extern crate lazy_static;
extern crate num_traits;

//use core::ops::{Shr, Shl};
//use num_traits::{Unsigned};
use core::fmt;
//use std::iter;
//use std::marker::PhantomData;
//use std::mem;
//use std::ptr;
//use std::slice;

//mod tests;
mod addr;

use addr::{Addr};

// Some basic definition

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(C)]
pub enum Error {
    InternalError,
    UnalignedStart,
    UnalignedEnd,
    UnalignedSize,
    UnalignedValue,
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

pub trait MemAreaBase<A>
where
//    A: Unsigned + Copy + Shl + Shr,
{
    fn start(&self) -> Addr<A>;
    fn end(&self) -> Addr<A>;
}

/*
/// Structure used to define an area to be scrubbed
/// * `start` - lowest virtual address of the area. Must be a multiple of the
///     cache line size
///
/// * end - address of the last byte of the area. Must be one less than a
///     multiple of the cache line size
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct MemArea<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    pub s: Addr<A>,
    pub e: Addr<A>,
}

impl<A> MemArea<A>
where
    A: Unsigned + Copy + Shl + Shr,
 {
    pub fn new(start: Addr::<A>, end: Addr::<A>) -> MemArea<A> {
        MemArea::<A> {
            s: start,
            e: end,
        }
    }
}

impl<A> MemAreaBase<A>
for MemArea<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    fn start(&self) -> Addr<A> {
        self.s
    }
    fn end(&self) -> Addr<A> {
        self.e
    }
}

/// Convert a number of items into the bit width of a value that will
/// hold that number. The number must be a non-zero multiple of two.
///
/// # Type parameters
///
/// * `T` - PrimInt and Debug
///
/// # Attributes
/// * `size` - Type T value whose width is to be computed.
///
/// # Return value
///
/// Returns Result<usize, Error> where the successful value is the bit width
fn value_to_width<T: PrimInt + std::fmt::Debug>(
    size: T,
) -> Result<usize, Error> {
    if size == T::zero() {
        return Err(Error::ZeroSize);
    }

    let leading_zeros = size.leading_zeros() as usize;
    let size_in_bits = mem::size_of::<T>() * 8; // should be T.BITS/size.BITS...
    let width = size_in_bits - (1 as usize) - leading_zeros;
    if size != (T::one() << width) {
        return Err(Error::UnalignedValue);
    }

    Ok(width)
}

// Low level memory scrubber definitions
// =====================================
// The assumptions are as follows:
//
// *    There is a one-to-one mapping of physical and virtual addresses It
//      follows that there are no overlapping mappings. The mapping does not
//      have to be an identify mapping nor should it map anything that should
//      not be scrubbed.
//
// *    Cache lines are comprisied of S items of type D. Type D is an integer
//      value the same size as those the ECC unit processes. Its size is
//      is a power of two and must be greater than one.
//
// *    Each cache line is aligned on an address that is multiple of S and S
//      is a power of two.
//
// *    Reading an item of type D will read the entire cache line if it is not
//      already in the cache.
//
// *    W is a power of two and specifies the number of ways in a cache line.
//      Ways are associatively mapped and, since there is a one-to-one
//      mapping of physical and virtual addresses, reading W addresses with
//      the same cache index but where the address bits higher than the
//      cache index differ will result in and nvalidate with flush operation
//      on all ways for that cache line.
//
// *    There are N cache lines in the cache and N is a power of two.
//
// Names for generic parameters
// ----------------------------

// D    Integer data type for ECC unit processing.
//
// A    Integer data type representing the address
//
// Conventions for naming cache parameters:
// N    Number of cache lines
// S    Number of ECC unit-sized items (generic parameter D, see above)
// W    Number of ways per cache line
//
// This is for a "classic" cache. There is a two-tier implementation where
// traits define the behavior for most caches but which allows for overriding
// methods to implement non-classical behavior.

// This is the basic definition of cache line data. Note that is is never
// instantiated, though traits that use this trait usually are. Specifically,
// means that none these will have "self" parameter.

pub trait CachelineDataBase<D, const S: usize>
where
    D: Unsigned,
{
}

pub trait CachelineBase<D, const S: usize, A>
where
    D: Unsigned,
    A: Unsigned + Copy + From<usize> + Shr + Shl + From<Addr<A>> + fmt::Display + fmt::Debug,
    *mut u8: From<A>,
{
    // Check cache line related parameters.
    //
    // Returns Result<(), Error>
    fn check_cacheline_params() -> Result<(), Error>
    where
        Self: Sized,
    {
        value_to_width::<usize>(std::mem::size_of::<D>())?;
        value_to_width::<usize>(S)?;
        Ok(())
    }

    // Return the number of bits required to hold the index into the cache
    // line
    fn cacheline_width() -> usize {
        value_to_width::<usize>(std::mem::size_of::<D>() * S).unwrap()
    }

    // Return the number of bytes in a cache line
    fn cacheline_size() -> usize
    where
        Self: Sized,
    {
        1 << Self::cacheline_width()
    }

    // Return the size of a MemArea in cache lines
    fn size_in_cachelines<M: MemAreaBase<A>>(scrub_area: &M) -> Addr<A>
    where
        Self: Sized,
//        A: Unsigned + Copy + Shr,
    {
        let cacheline_width: Addr<A> = Self::cacheline_width().into();

        let start = scrub_area.start();
        let start_in_cachelines = start >> cacheline_width;

let x = Addr::<u128>::new(32);
let y = Addr::<u128>::new(4);
println!("x({}) + y({}) = {}", x, y, x + y);
println!("x({}) >> y({}) = {}", x, y, x >> y);
println!("x({}) >> y({}) = {}", start, cacheline_width, start >> cacheline_width);

        // This will truncate the number of cache lines by one
        let end = scrub_area.end();
        let end_in_cachelines = end >> cacheline_width;

        (end_in_cachelines - start_in_cachelines) + 1
    }

    // Read the first element in the cache line indicated by the address
    fn read(p: Addr<A>) -> D
    where
        Self: Sized,
    {
        let ptr: *mut u8 = p.into();
        let a_ref: &[D; S] = unsafe { &*(ptr as *const _) };
        unsafe { ptr::read(&a_ref[0]) }
    }

    // Read the entire cacheline
    fn read_cacheline(p: Addr<A>)
    where
        Self: Sized,
    {
        Self::read(p);
    }
}

/*
// FIXME: is there any way to drop the CL parameter and define it in terms
// of N, W, D, and S?
pub trait CacheBase<
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    // Report on whether any parameter problems are detected
    fn check_cache_params(&self) -> Result<(), Error> {
        value_to_width::<usize>(N)?;
        value_to_width::<usize>(W)?;
        Ok(())
    }

    // Return the number of bits used to index into the cache, i.e. the index
    // of a cache line in the cache. A cache with 1024 lines will have an
    // index using 10 bits.
    fn cache_index_width(&self) -> usize {
        //println!("CacheBase: cache_index_width: entered");
        value_to_width::<usize>(N).unwrap()
    }

    /// Computes the offset, in cache lines, of the next address at or higher
    /// than the given pointer for an address hitting the given cache index.
    ///
    /// NOTE: You are unlikely to ever need to implement this
    ///
    /// # Attributes
    /// * `p` - Address to start at
    /// * `index` - Cache index to search for
    ///
    /// # Return value
    /// The offset to the next address
    fn offset_to_next_index(&self, p: Addr<A>, index: usize) -> usize
    where
        Self: Sized,
    {
        // FIXME: can I just use Self now?
        let start_index = <Self as CacheBase<N, W, D, S, A>>::cache_index(self, p);
        let cache_lines = <Self as CacheBase<N, W, D, S, A>>::cache_lines(self);

        // Compute the offset from the start of the self.scrub_area to the
        // next highest address whose cache index is the one we are currently
        // scrubbing.
        let result = if index >= start_index {
            index - start_index
        } else {
            (index + cache_lines) - start_index
        };
        result as usize
    }

    /// Extract the cache index part of the address
    /// # Parameters
    /// * `self` - Self
    /// * `p` - Address from which the cache index is to be extracted
    /// NOTE: You are unlikely to ever need to implement this
    /// # Return value
    /// The cache line index
    fn cache_index(&self, p: Addr<A>) -> usize
    where
        Self: Sized,
    {
        let cacheline_width = CachelineBase::<D, S, A>::cacheline_width();
        let width = Addr::<A>::new(cacheline_width.into());
        // FIXME: can I just use Self now?
        let cache_index_width =
            <Self as CacheBase<N, W, D, S, A>>::cache_index_width(self);
        (p >> width) & ((1 << cache_index_width) - 1)
    }

    // Return the number of cache lines in the index.
    //
    // NOTE: You are unlikely to ever need to implement this
    fn cache_lines(&self) -> usize {
        // FIXME: can I just use Self now?
        1 << <Self as CacheBase<N, W, D, S, A>>::cache_index_width(
            self,
        )
    }
}

pub trait MemoryScrubberBase<
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    fn cache(&self) -> &Cache<N, W, D, S, A>;
    fn scrub_areas(&self) -> &[MemArea<A>];

    fn check_scrubber_params(
        cache: &Cache<N, W, D, S, A>,
        scrub_areas: &[MemArea<A>],
    ) -> Result<(), Error> {
        // Check the corresponding cache trait
        CacheBase::<N, W, D, S, A>::check_cache_params(cache)?;

        //  Verify scrub area descriptions
        if scrub_areas.len() == 0 {
            return Err(Error::NoMemAreas);
        }

        let cacheline_size = Addr::<A>::new(Cacheline::<D, S>::cacheline_size().into());
        let cacheline_mask = cacheline_size - Addr::<A>::new(1.into());

        // Check each scrub area for errors
        for scrub_area in scrub_areas {
            let start = scrub_area.start();
            let end = scrub_area.end();

            if start >= end {
                return Err(Error::EmptyMemArea);
            }

            if (start & cacheline_mask) != 0 {
                return Err(Error::UnalignedStart);
            }

            if (end & cacheline_mask) != cacheline_size - Addr::<A>::new(1.into()) {
                return Err(Error::UnalignedEnd);
            }
        }
        Ok(())
    }

    /// This is the core of the scrubbing work. We scrub the given number
    /// of bytes out of the total scrubbing areas supplied, starting after
    /// the previous location.
    ///
    /// # Arguments
    ///
    /// * `n` - Number of bytes to scrub
    ///
    /// # Return value
    ///
    /// Returns Result<(), Error>
    fn scrub(&self, n: Addr<A>) -> Result<(), Error>
    where
        Self: Sized,
    {
        let cacheline_width = Cacheline::<D, S>::cacheline_width();
        let cacheline_size = Cacheline::<D, S>::cacheline_size();
        let cacheline_size_mask = Addr::<A>::new((cacheline_size - 1).into());

        if (n & cacheline_size_mask) != 0 {
            return Err(Error::UnalignedSize);
        }

        // Convert to the number of cachelines to scrub
        let n_scrublines = n >> Addr::<A>::new(cacheline_width.into());
        let iterator = ScrubCountIterator::new(
            self.cache(),
            self.scrub_areas(),
            n_scrublines,
        )?;

        // At this point, it's pretty much Iterators all the way down.
        for p in iterator {
            Cacheline::<D, S>::read_cacheline(p);
        }

        Ok(())
    }
}

// Read a given number of cache lines
//
// # Type Parameters
//
// * `C` - CacheBase, cache configuration
//
// * `CL` - CacheBase, cache line configuration
//
// * `CLD` - CacheBase, memory overlayable cache line data
//
// * `N` - Number of cache lines
//
// * `W` - Number of ways per cache line
//
// * `D` - Type of a single cache line item. This is the same size as what the
//         ECC unit works on
//
// * `S` - Number of items of type `D` in a single cache line way
pub struct ScrubCountIterator<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    cache: &'a Cache<N, W, D, S, A>,
    scrub_areas: &'a [MemArea<A>],
    n_scrublines: Addr<A>,
    i: Addr<A>,
    iterator: ScrubAreasIterator<'a, N, W, D, S, A>,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > ScrubCountIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, D, S, A>,
        scrub_areas: &'a [MemArea<A>],
        n_scrublines: Addr<A>,
    ) -> Result<ScrubCountIterator<'a, N, W, D, S, A>, Error>
    {
        let iterator = ScrubAreasIterator::new(cache, scrub_areas)?;

        Ok(ScrubCountIterator {
            cache: cache,
            scrub_areas: scrub_areas,
            n_scrublines: n_scrublines,
            i: Addr::<A>::new(0usize.into()),
            iterator: iterator,
            _marker1: PhantomData,
        })
    }
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > iter::Iterator for ScrubCountIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    type Item = Addr<A>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            //println!("ScrubCountIterator: next()");
            if self.i == self.n_scrublines {
                return None;
            }

            let p = self.iterator.next();

            if p.is_some() {
                self.i += Addr::<A>::new(1usize.into());
                return p;
            }

            self.iterator =
                ScrubAreasIterator::new(self.cache, self.scrub_areas)
                    .expect("ScrubAreasIterator::new failed");
        }
    }
}

// Walks through cache line-sized items. Never reaches an end, that is,
// next() never returns None
pub struct ScrubAreasIterator<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    cache: &'a Cache<N, W, D, S, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: CacheIndexIterator<'a, N, W, D, S, A>,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > ScrubAreasIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, D, S, A>,
        scrub_areas: &'a [MemArea<A>],
    ) -> Result<ScrubAreasIterator<'a, N, W, D, S, A>, Error>
    {
        let iterator =
            CacheIndexIterator::<N, W, D, S, A>::new(
                cache,
                scrub_areas,
            )?;

        Ok(ScrubAreasIterator {
            cache: cache,
            scrub_areas: scrub_areas,
            iterator: iterator,
            _marker1: PhantomData,
        })
    }
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > iter::Iterator for ScrubAreasIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    type Item = Addr<A>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            //println!("ScrubAreasIterator: next()");
            let next = self.iterator.next();
            //println!("ScrubAreasIterator: back from CacheIndexIterator.next()");

            if next.is_some() {
                return next;
            }

            //println!("ScrubAreasIterator: calling CacheIndexIterator::new()");
            self.iterator = match CacheIndexIterator::<
                N,
                W,
                D,
                S,
                A,
            >::new(
                self.cache, self.scrub_areas
            ) {
                Err(e) => panic!("CacheIndexIterator failed: {}", e),
                Ok(iterator) => iterator,
            };
            //println!("ScrubAreasIterator: back from CacheIndexIterator::new()");
        }
    }
}

// This goes through all cache indices. The current cache index is kept at a
// higher level to ensure continuity. That is, the previous cache index is
// passed down to us and incremented here so that we don't restart the indices
// each time this is called.
//
// cache:  Cache descriptor
// scrub_areas: List of memory areas to be scrubbed
// iterator:    An iterator for a scrubbing a single memory area
// cur_index:   The index of the cache line we are scrubbing

pub struct CacheIndexIterator<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>
{
    cache: &'a Cache<N, W, D, S, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: MemAreasIterator<'a, N, W, D, S, A>,
    cur_index: usize,
    // FIXME: needed?
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > CacheIndexIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, D, S, A>,
        scrub_areas: &'a [MemArea<A>],
    ) -> Result<CacheIndexIterator<'a, N, W, D, S, A>, Error>
    {
        let cur_index = 0;
        let iterator = MemAreasIterator::<N, W, D, S, A>::new(
            cache,
            scrub_areas,
            cur_index,
        )?;

        Ok(CacheIndexIterator {
            cache: cache,
            scrub_areas: scrub_areas,
            iterator: iterator,
            cur_index: cur_index,
            _marker1: PhantomData,
        })
    }
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > iter::Iterator for CacheIndexIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    type Item = Addr<A>;

    fn next(&mut self) -> Option<Self::Item> {
        //println!("CacheIndexIterator::next(): entered");
        let cache_index_width = self.cache.cache_index_width();
        //println!("CacheIndexIterator::next(): after calling cache_index_width");
        let cache_lines = 1 << cache_index_width;

        loop {
            //println!("CacheIndexIterator: next()");
            let next = self.iterator.next();

            if let Some(p) = next {
                return Some(p);
            }

            self.cur_index += 1;

            if self.cur_index == cache_lines {
                return None;
            }

            self.iterator =
                match MemAreasIterator::<N, W, D, S, A>::new(
                    self.cache,
                    self.scrub_areas,
                    self.cur_index,
                ) {
                    Err(e) => {
                        panic!("MemAreasIterator::new failed: {}", e)
                    }
                    Ok(iterator) => iterator,
                };
        }
    }
}

// This iterator goes through all defined scrub areas
// cache:  Cache descriptor
// scrub_areas: List of memory areas to be scrubbed
// iterator:    An iterator for a scrubbing a single memory area
// i:           The index into the current MemArea
pub struct MemAreasIterator<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    cache: &'a Cache<N, W, D, S, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: MemAreaIterator<'a, N, W, D, S, A>,
    i: usize,
    cur_index: usize,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > MemAreasIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, D, S, A>,
        scrub_areas: &'a [MemArea<A>],
        cur_index: usize,
    ) -> Result<MemAreasIterator<'a, N, W, D, S, A>, Error>
    {
        // FIXME: restore this
        //        if scrubber.scrub_areas().len() == 0 {
        //            return Err(Error::NoMemAreas);
        //        }

        let iterator = MemAreaIterator::<N, W, D, S, A>::new(
            cache,
            &scrub_areas[0],
            cur_index,
        )?;

        Ok(MemAreasIterator {
            cache: cache,
            scrub_areas: scrub_areas,
            iterator: iterator,
            i: 0,
            cur_index: cur_index,
            _marker1: PhantomData,
        })
    }
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > iter::Iterator for MemAreasIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    type Item = Addr<A>;

    // Loop through all scrub areas
    fn next(&mut self) -> Option<Self::Item> {
        //println!("MemAreasIterator: next()");
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

            self.iterator =
                match MemAreaIterator::<N, W, D, S, A>::new(
                    self.cache,
                    &self.scrub_areas[self.i],
                    self.cur_index,
                ) {
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
// cache   Description of the cache
// scrub_area:  Specifies the address of the scrub area
// i:           Number of cache line-sized items we've scanned in this
//              MemArea
// cur_index:   Cache index we are scrubbing
pub struct MemAreaIterator<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    cache: &'a Cache<N, W, D, S, A>,
    scrub_area: &'a MemArea<A>,
    i: usize,
    cur_index: usize,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > MemAreaIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    // Create a new MemAreaIterator.
    // cache: Description of the cache
    // scrub_area: Memory over which we Iterate
    // i:           Current element in scrub_area
    // cur_index:   Cache index we're looking for
    //
    // Returns: Ok(MemAreaIterator) on success, Err(Error) on failure
    pub fn new(
        cache: &'a Cache<N, W, D, S, A>,
        scrub_area: &'a MemArea<A>,
        cur_index: usize,
    ) -> Result<MemAreaIterator<'a, N, W, D, S, A>, Error>
    {
        if scrub_area.start() == scrub_area.end() {
            return Err(Error::NoMemAreas);
        }

        Ok(MemAreaIterator {
            cache: cache,
            scrub_area: scrub_area,
            i: 0,
            cur_index: cur_index,
            _marker1: PhantomData,
        })
    }
}

// Return a pointer into the next memory area of cache line size
impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > iter::Iterator for MemAreaIterator<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    // FIXME: Need to supply an address type as a generic parameter, not assume Addr<A>
    type Item = Addr<A>;

    fn next(&mut self) -> Option<Self::Item> {
        //println!("MemAreasIterator: next()");
        let cache_index_width = self.cache.cache_index_width();

        let first_offset = self
            .cache
            .offset_to_next_index(self.scrub_area.start(), self.cur_index);

        // Add multiples of the number of cache lines in the cache to get to
        // the offset with the same cache index.
        let cur_offset =
            Addr::<A>::new((first_offset + (self.i << cache_index_width)).into());

        // If this offset is greater than or equal to the number of cache
        // lines in the current scrub area, we're done.
        // FIXME: shouldn't I be able to use something like:
        // let size_in_cachelines = self.scrubber.cache()
        //    .size_in_cachelines(&self.scrub_area);
        let size_in_cachelines =
            Cacheline::<D, S>::size_in_cachelines::<MemArea<A>>(&self.scrub_area);
        if cur_offset >= size_in_cachelines {
            return None;
        }

        let cacheline_width = Addr::<A>::new(Cacheline::<D, S>::cacheline_width().into());
        self.i += 1;
        let cur_offset_in_bytes = cur_offset << cacheline_width;
        let p = self.scrub_area.start() + cur_offset_in_bytes;
        return Some(p);
    }
}

// Memory scrubber implementations
// ===============================

#[repr(C)]
pub struct CachelineData<D, const S: usize>
where
    D: Unsigned,
{
    // The actual cache line data as an array of S items of type D.
    // FIXME: This is an inconsistency in Rust. S should be capable of being
    // as large as memory, but it seems like usize might be less. Not good,
    // really
    data: [D; S],
}

impl<D, const S: usize> CachelineDataBase<D, S> for CachelineData<D, S> where
    D: Unsigned,
{
}

pub struct Cacheline<D, const S: usize>
where
    D: Unsigned,
{
    _marker2: PhantomData<D>,
}

impl<D, const S: usize> Cacheline<D, S>
where
    D: Unsigned,
{
    /*
        // FIXME: I don't know why these need to be implemented here. Shouldn't they
        // already be implemented as trait defaults?
        fn check_cacheline_params() -> Result<(), Error> where Self: Sized {
            <Self as CachelineBase<D, S, A>>
                ::check_cacheline_params()?;
            Ok(())
        }
    */

    /*
        fn new() -> Cacheline<D, S>{
            Cacheline::<D, S> {
                _marker1:   PhantomData,
                _marker2:   PhantomData,
            }
        }
    */
}

impl<D, const S: usize, A> CachelineBase<D, S, A> for Cacheline<D, S>
where
    D: Unsigned,
    A: Unsigned + AddrBase<A>,
{
}

pub struct Cache<
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: Unsigned + AddrBase<A>,
{
    cacheline: Cacheline<D, S>,
    _marker1: PhantomData<D>,
    _marker2: PhantomData<A>, // FIXME: remove this
}

impl<'a, const N: usize, const W: usize, D, const S: usize, A>
    Cache<N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    /*
        fn new() -> Self {
    //        let cacheline = <CL>::new();
            let cacheline = Cacheline::<D, S>::new();

            Cache::<N, W, D, S> {
                cacheline:  cacheline,
                _marker1:   PhantomData,
            }
        }
    */

    /*
        fn check_cache_params(&self) -> Result<(), Error> {
            // FIXME: should this be referencing a "classic" derivative of
            // CacheBase?
            // Invoke check_cache_params() in CacheBase.
            //let self_cache_base = self as &dyn CacheBase<dyn CachelineBase>;
            //self_cache_base.check_cache_params()?;
    //        let self_cache_base =
    //            self as &dyn CacheBase<Cacheline<D, S>, CLD>;
    //            self as &dyn CacheBase<Cacheline<D, S>, CLD>;
            self.check_cache_params()?;
            Ok(())
        }
    */
}

// FIXME: I think this is the root of all of the Cache implementations,
// but it's not yet clear. I'm feeling pretty good about this, however.
impl<const N: usize, const W: usize, D, const S: usize, A>
    CacheBase<N, W, D, S, A> for Cache<N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
    /*
    // FIXME: revise this function
        // Report on whether any parameter problems are detected
        fn check_cache_params(&self) -> Result<(), Error> {
            <CL as CachelineBase<D, S, A>>::check_cacheline_params()?;
            Ok(())
        }

    // FIXME: these are in the right order
        // NOTE: You are unlikely to ever need to implement this
        // Extract the cache index part of the address
        fn cache_index_width(&self) -> usize where Self: Sized {
    //println!("Cache::CacheBase1: cache_index_width: entered");
            CacheBase::<CL>::cache_index_width(self)
        }

        // NOTE: You are unlikely to ever need to implement this
        // Computes the offset, in cache lines, of the next address at or higher
        // than the given pointer for an address hitting the given cache index.
        //
        // p:       Address to start at
        // index:   Cache index to search for
        fn offset_to_next_index(&self, p: Addr<A>, index: usize) -> usize {
            let self_cache_base = self as &dyn CacheBase<Cacheline<D, S>>;
            let start_index = self_cache_base.cache_index(p);
            let cache_lines = self_cache_base.cache_lines();

            // Compute the offset from the start of the self.scrub_area to the
            // next highest address whose cache index is the one we are currently
            // scrubbing.
            let result = if index >= start_index { index - start_index }
            else { (index + cache_lines) - start_index };
            result as usize
        }

        // NOTE: You are unlikely to ever need to implement this
        // Extract the cache index part of the address
        fn cache_index(&self, p: Addr<A>) -> usize {
            let cacheline_width =
                <Cacheline<D, S> as CachelineBase<D, S, A>>
                ::cacheline_width();
            let self_cache_base = self as &dyn CacheBase<Cacheline<D, S>>;
            let cache_index_width = self_cache_base.cache_index_width();
            (p as usize >> cacheline_width) & ((1 << cache_index_width) - 1)
        }

        // NOTE: You are unlikely to ever need to implement this
        // Report the number of cache lines in the cache
        fn cache_lines(&self) -> usize {
            N
        }
    */
}

/// # Attributes
///
/// * `my_cache` - Cache description
///
/// * 'my_scrub_areas` - List of MemAreas to be scrubbed
pub struct MemoryScrubber<
    'a,
    const N: usize,
    const W: usize,
    D,
    const S: usize,
    A,
> where
    D: Unsigned,
    A: AddrBase<A>,
{
    my_cache: &'a Cache<N, W, D, S, A>,
    my_scrub_areas: &'a [MemArea<A>],
    _marker4: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > MemoryScrubber<'a, N, W, D, S, A>
where
    D: Unsigned,
    A: AddrBase<A>,
{
}

impl<
        'a,
        const N: usize,
        const W: usize,
        D,
        const S: usize,
        A,
    > MemoryScrubberBase<N, W, D, S, A>
    for MemoryScrubber<'a, N, W, D, S, A>
where
    D: Unsigned + 'a,
    A: AddrBase<A>,
{
    fn cache(&self) -> &Cache<N, W, D, S, A> {
        self.my_cache
    }

    fn scrub_areas(&self) -> &[MemArea<A>] {
        self.my_scrub_areas
    }
}

// ===============================================================

/*
// C interfaces pieces

#[derive(Clone)]
#[repr(C)]
pub struct CCacheBase {
    cacheline_width: usize,
    cache_index_width: usize,

    // NOTE: You are unlikely to ever need to implement this
    // Return the number of bytes in the cache line. A cache line width of 4
    // bits will have a cache line size of 16 bytes.
    c_cacheline_size: extern "C" fn(me: *const CCacheBase) -> usize,

    // This function is given a pointer to a cache line-aligned address with
    // as many bytes as are in a cache line. The implementation should do
    // whatever is necessary to ensure all bytes are read in order to trigger
    // a fault if any bits have an unexpected value. So long as the number
    // of bad bits is small enough (ECC-dependent), corrected data should
    // be written back to that location, preventing the accumulation of so many
    // bad bits that the correct value cannot be determined.
    c_read_cacheline: extern "C" fn(me: *const CCacheBase,
        _cacheline_ptr: *const dyn CachelineBase),

    // Return the size of a MemArea in cachelines
    c_size_in_cachelines: extern "C" fn(me: *const CCacheBase,
        scrub_area: &MemArea) -> usize,

    // Returns the cache index part of the address
    c_cache_index: extern "C" fn(me: *const CCacheBase,
        p: *const u8) -> usize,
}

impl CCacheBase {
    pub fn new(cacheline_width: usize,
        cache_index_width: usize,
        c_cacheline_size: extern "C" fn(me: *const CCacheBase) -> usize,
        c_read_cacheline: extern "C" fn(me: *const CCacheBase,
            _cacheline_ptr: *const dyn CachelineBase),
        c_size_in_cachelines: extern "C" fn(me: *const CCacheBase,
            scrub_area: &MemArea) -> usize,
        c_cache_index: extern "C" fn(me: *const CCacheBase,
            p: *const u8) -> usize) -> CCacheBase {
        CCacheBase {
            cacheline_width: cacheline_width,
            cache_index_width: cache_index_width,
            c_cacheline_size: c_cacheline_size,
            c_read_cacheline: c_read_cacheline,
            c_size_in_cachelines: c_size_in_cachelines,
            c_cache_index: c_cache_index,
        }
    }
}

impl CacheBase for CCacheBase {
/*
    fn cacheline_width(&self) -> usize {
        self.cacheline_width
    }

    fn cacheline_size(&self) -> usize {
        let self_ptr: *const CCacheBase = self as *const _;
        1 << (self.c_cacheline_size)(self_ptr)
    }

    fn cache_index_width(&self) -> usize {
        self.cache_index_width
    }

    fn read_cacheline(&self, _cacheline_ptr: Addr<A>) {
        let self_ptr: *const CCacheBase = self as *const _;
        let _cacheline_ptr = _cacheline_ptr as *const dyn CachelineBase;
        (self.c_read_cacheline)(self_ptr, _cacheline_ptr)
    }

    fn size_in_cachelines(&self, scrub_area: &MemArea) -> Addr<A> {
        let self_ptr: *const CCacheBase = self as *const _;
        (self.c_size_in_cachelines)(self_ptr, scrub_area)
    }
*/
}

impl<'a, CL: Cacheline<'a, D>, const N: usize, D: CachelineData>
CacheBase<'a, N, D>
for CCacheBase {
}

impl<'a, D> Cacheline<'a, D> for CCacheBase {
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

impl<'a, C: CacheBase<CL>, CL: CachelineBase<'a>>
AutoScrubDesc<CD, CL>
for CAutoScrubDesc<'a, CD, CL> {
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
pub extern "C" fn autoscrub<'a, const N: usize, CLD>(c_cache: &'a mut CCacheBase,
    scrub_areas_ptr: *const MemArea, n_scrub_areas: usize,
    c_auto_scrub_desc: &'a mut CAutoScrubDesc<CCacheBase, CCacheline>) ->
        CAutoScrubResult
    where
{
    let scrub_result: Result<(), Error>;

    {
        let scrub_areas_slice = unsafe {
            // It looks like the slice constructed with from_raw_parts is
            // never freed. This is the behavior we want when we get a pointer
            // from C/C++.
            slice::from_raw_parts(scrub_areas_ptr, n_scrub_areas)
        };

        scrub_result = AutoScrub::<'a, CCacheBase, CCacheline, N, CLD>
            ::autoscrub(c_cache, &scrub_areas_slice, c_auto_scrub_desc);
    }

    match scrub_result {
        Err(e) => CAutoScrubResult { is_err: true, error: e },
        Ok(_) => CAutoScrubResult { is_err: false, error: Error::InternalError },
    }
}
*/

pub trait AutoScrubDesc<CL, A>
where
    A: Unsigned,
{
    fn next(&mut self) -> Addr<A>;
}

/*
struct AutoScrub<'a, const N: usize, const W: usize, D,
    const S: usize>
where
    D:      Unsigned,
{
    scrubber:   MemoryScrubber<'a, N, W, D, S>,
    desc:       &'a mut dyn AutoScrubDesc<CL>,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheBase<CL>. So, this
    // works around that problem
    _marker1:    PhantomData<D>,
}

impl<'a, const N: usize, const W: usize, D: Unsigned, const S: usize>
AutoScrub<'a, N, W, D, S>
where
{
/*
    fn new(cache_in: &'a C, scrub_areas: &'a [MemArea],
        desc: &'a mut dyn AutoScrubDesc<CL>) ->
        Result<AutoScrub<'a, N, W, D, S>, Error> {

        let scrubber = match MemoryScrubber::<'a, N, W, D, S>
            ::new(cache_in, scrub_areas) {
            Err(e) => return Err(e),
            Ok(scrubber) => scrubber,
        };

        Ok(AutoScrub {
            scrubber:   scrubber,
            desc:       desc,
            _marker1:   PhantomData,
            _marker2:   PhantomData,
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

    pub fn autoscrub(cache: &'a mut C, scrub_areas: &'a [MemArea],
            desc: &'a mut dyn AutoScrubDesc<CL>) -> Result<(), Error> {
        let mut autoscrub = Self::new(cache, scrub_areas, desc)?;
        autoscrub.scrub()
    }
*/
}

impl<'a, const N: usize, const W: usize, D: Unsigned, const S: usize>
CachelineBase<D, S, A>
for AutoScrub<'a, N, W, D, S>
where
{
}

impl<'a, const N: usize, const W: usize, D: Unsigned, const S: usize>
CacheBase<N, W, D, S>
for AutoScrub<'a, N, W, D, S>
where
{
}
*/
*/
*/
