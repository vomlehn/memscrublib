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
// FIXME: Use core:: instead of std:: where possible
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
//      let mut mem = Mem::new::<TestCacheline<S, D>, S, D>(MEM_SIZE);
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

//#[macro_use]
extern crate lazy_static;
extern crate num_traits;

use core::fmt;
use core::mem;
use core::ptr;
use num_traits::PrimInt;
use std::convert::From;
use std::iter;
use std::marker::PhantomData;
//use std::slice;

mod addr;

use addr::{Addr, AddrImplTrait, DataImplTrait};

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
{
    fn start(&self) -> Addr<A>;
    fn end(&self) -> Addr<A>;
}

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
    A: AddrImplTrait<A>,
{
    pub s: Addr<A>,
    pub e: Addr<A>,
}

impl<A> MemArea<A>
where
    A: AddrImplTrait<A>,
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
    A: AddrImplTrait<A>,
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

    let leading_zeros = size.leading_zeros();
    let size_in_bits = mem::size_of::<T>() * 8; // FIXME: should be T.BITS/size.BITS...
    let width = (size_in_bits - 1) - (leading_zeros as usize);
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

pub trait CachelineDataBase<const S: usize, D>
{
}

pub trait CachelineBase<const S: usize, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
    Addr<A>: Into<*mut D>
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
        A: AddrImplTrait<A>,
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

        (end_in_cachelines - start_in_cachelines) + 1.into()
    }

    // Read the first element in the cache line indicated by the address
    fn read(p: Addr<A>) -> D
    where
        Self: Sized,
    {
        let ptr: *mut D = p.into();
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

// FIXME: is there any way to drop the CL parameter and define it in terms
// of N, W, D, and S?
pub trait CacheBase<
    const N: usize,
    const W: usize,
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
    usize: From<A>,
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
        A: AddrImplTrait<A>,
    {
        // FIXME: can I just use Self now?
        let start_index = <Self as CacheBase<N, W, S, D, A>>::cache_index(self, p);
        let cache_lines = <Self as CacheBase<N, W, S, D, A>>::cache_lines(self);

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
        A: AddrImplTrait<A>,
    {
        let cacheline_width: Addr<A> =
            <Cacheline<S, D> as CachelineBase::<S, D, A>>::cacheline_width().into();
        // FIXME: can I just use Self now?
        let cache_index_width =
            <Self as CacheBase<N, W, S, D, A>>::cache_index_width(self);
        let shifted_index: usize = (p >> cacheline_width).into();
        shifted_index & ((1 << cache_index_width) - 1)
    }

    // Return the number of cache lines in the index.
    //
    // NOTE: You are unlikely to ever need to implement this
    fn cache_lines(&self) -> usize {
        // FIXME: can I just use Self now?
        1 << <Self as CacheBase<N, W, S, D, A>>::cache_index_width(
            self,
        )
    }
}

pub trait MemoryScrubberBase<
    const N: usize,
    const W: usize,
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    fn cache(&self) -> &Cache<N, W, S, D, A>;
    fn scrub_areas(&self) -> &[MemArea<A>];

    fn check_scrubber_params(
        cache: &Cache<N, W, S, D, A>,
        scrub_areas: &[MemArea<A>],
    ) -> Result<(), Error> {
        // Check the corresponding cache trait
        CacheBase::<N, W, S, D, A>::check_cache_params(cache)?;

        //  Verify scrub area descriptions
        if scrub_areas.len() == 0 {
            return Err(Error::NoMemAreas);
        }

        let cacheline_size: Addr<A> = Cacheline::<S, D>::cacheline_size().into();
        let cacheline_mask: Addr<A> = (cacheline_size - 1.into()).into();

        // Check each scrub area for errors
        for scrub_area in scrub_areas {
            let start = scrub_area.start();
            let end = scrub_area.end();

            if start >= end {
                return Err(Error::EmptyMemArea);
            }

            if (start & cacheline_mask) != 0.into() {
                return Err(Error::UnalignedStart);
            }

            if (end & cacheline_mask) != cacheline_size - 1.into() {
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
        let cacheline_width = Cacheline::<S, D>::cacheline_width();
        let cacheline_size = Cacheline::<S, D>::cacheline_size();
        let cacheline_size_mask = (cacheline_size - 1).into();

        if (n & cacheline_size_mask) != 0.into() {
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
            Cacheline::<S, D>::read_cacheline(p);
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    cache: &'a Cache<N, W, S, D, A>,
    scrub_areas: &'a [MemArea<A>],
    n_scrublines: Addr<A>,
    i: Addr<A>,
    iterator: ScrubAreasIterator<'a, N, W, S, D, A>,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > ScrubCountIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, S, D, A>,
        scrub_areas: &'a [MemArea<A>],
        n_scrublines: Addr<A>,
    ) -> Result<ScrubCountIterator<'a, N, W, S, D, A>, Error>
    {
        let iterator = ScrubAreasIterator::new(cache, scrub_areas)?;

        Ok(ScrubCountIterator {
            cache: cache,
            scrub_areas: scrub_areas,
            n_scrublines: n_scrublines,
            i: 0usize.into(),
            iterator: iterator,
            _marker1: PhantomData,
        })
    }
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > iter::Iterator for ScrubCountIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D> + PartialEq,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
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
                self.i += 1usize.into();
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    cache: &'a Cache<N, W, S, D, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: CacheIndexIterator<'a, N, W, S, D, A>,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > ScrubAreasIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, S, D, A>,
        scrub_areas: &'a [MemArea<A>],
    ) -> Result<ScrubAreasIterator<'a, N, W, S, D, A>, Error>
    {
        let iterator =
            CacheIndexIterator::<N, W, S, D, A>::new(
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
        const S: usize,
        D,
        A,
    > iter::Iterator for ScrubAreasIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
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
                S,
                D,
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    cache: &'a Cache<N, W, S, D, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: MemAreasIterator<'a, N, W, S, D, A>,
    cur_index: usize,
    // FIXME: needed?
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > CacheIndexIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, S, D, A>,
        scrub_areas: &'a [MemArea<A>],
    ) -> Result<CacheIndexIterator<'a, N, W, S, D, A>, Error>
    {
        let cur_index = 0;
        let iterator = MemAreasIterator::<N, W, S, D, A>::new(
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
        const S: usize,
        D,
        A,
    > iter::Iterator for CacheIndexIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
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
                match MemAreasIterator::<N, W, S, D, A>::new(
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    cache: &'a Cache<N, W, S, D, A>,
    scrub_areas: &'a [MemArea<A>],
    iterator: MemAreaIterator<'a, N, W, S, D, A>,
    i: usize,
    cur_index: usize,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > MemAreasIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    pub fn new(
        cache: &'a Cache<N, W, S, D, A>,
        scrub_areas: &'a [MemArea<A>],
        cur_index: usize,
    ) -> Result<MemAreasIterator<'a, N, W, S, D, A>, Error>
    {
        // FIXME: restore this
        //        if scrubber.scrub_areas().len() == 0 {
        //            return Err(Error::NoMemAreas);
        //        }

        let iterator = MemAreaIterator::<N, W, S, D, A>::new(
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
        const S: usize,
        D,
        A,
    > iter::Iterator for MemAreasIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
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
                match MemAreaIterator::<N, W, S, D, A>::new(
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    cache: &'a Cache<N, W, S, D, A>,
    scrub_area: &'a MemArea<A>,
    i: usize,
    cur_index: usize,
    _marker1: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > MemAreaIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    // Create a new MemAreaIterator.
    // cache: Description of the cache
    // scrub_area: Memory over which we Iterate
    // i:           Current element in scrub_area
    // cur_index:   Cache index we're looking for
    //
    // Returns: Ok(MemAreaIterator) on success, Err(Error) on failure
    pub fn new(
        cache: &'a Cache<N, W, S, D, A>,
        scrub_area: &'a MemArea<A>,
        cur_index: usize,
    ) -> Result<MemAreaIterator<'a, N, W, S, D, A>, Error>
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
        const S: usize,
        D,
        A,
    > iter::Iterator for MemAreaIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
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
            Cacheline::<S, D>::size_in_cachelines::<MemArea<A>>(&self.scrub_area);
        if cur_offset >= size_in_cachelines {
            return None;
        }

        let cacheline_width = Addr::<A>::new(Cacheline::<S, D>::cacheline_width().into());
        self.i += 1;
        let cur_offset_in_bytes = cur_offset << cacheline_width;
        let p = self.scrub_area.start() + cur_offset_in_bytes;
        return Some(p);
    }
}

// Memory scrubber implementations
// ===============================

#[repr(C)]
pub struct CachelineData<const S: usize, D>
where
    D: DataImplTrait<D>,
{
    // The actual cache line data as an array of S items of type D.
    // FIXME: This is an inconsistency in Rust. S should be capable of being
    // as large as memory, but it seems like usize might be less. Not good,
    // really
    data: [D; S],
}

impl<const S: usize, D> CachelineDataBase<S, D> for CachelineData<S, D> where
    D: DataImplTrait<D>,
{
}

pub struct Cacheline<const S: usize, D>
where
    D: DataImplTrait<D>,
{
    _marker1: PhantomData<D>,
}

impl<const S: usize, D> Cacheline<S, D>
where
    D: DataImplTrait<D>,
{
    /*
        // FIXME: I don't know why these need to be implemented here. Shouldn't they
        // already be implemented as trait defaults?
        fn check_cacheline_params() -> Result<(), Error> where Self: Sized {
            <Self as CachelineBase<S, D, A>>
                ::check_cacheline_params()?;
            Ok(())
        }
    */

    fn new() -> Cacheline<S, D>{
        Cacheline::<S, D> {
            _marker1:   PhantomData,
        }
    }
}

impl<const S: usize, D, A> CachelineBase<S, D, A> for Cacheline<S, D>
where
    D: DataImplTrait<D>,
    *mut D: From<A>,
    usize: From<A>,
    A: AddrImplTrait<A>,
    Addr<A>: Into<*mut D>,
{
}

#[derive(Clone, Copy)]
pub struct Cache<
    const N: usize,
    const W: usize,
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
//    cacheline: Cacheline<S, D>,
    _marker1: PhantomData<D>,
    _marker2: PhantomData<A>, // FIXME: remove this
}

impl<'a, const N: usize, const W: usize, const S: usize, D, A>
    Cache<N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    fn new() -> Self {
//        let cacheline = <CL>::new();
//        let cacheline = Cacheline::<S, D>::new();

        Cache::<N, W, S, D, A> {
//            cacheline:  cacheline,
            _marker1:   PhantomData,
            _marker2:   PhantomData,
        }
    }

    /*
        fn check_cache_params(&self) -> Result<(), Error> {
            // FIXME: should this be referencing a "classic" derivative of
            // CacheBase?
            // Invoke check_cache_params() in CacheBase.
            //let self_cache_base = self as &dyn CacheBase<dyn CachelineBase>;
            //self_cache_base.check_cache_params()?;
    //        let self_cache_base =
    //            self as &dyn CacheBase<Cacheline<S, D>, CLD>;
    //            self as &dyn CacheBase<Cacheline<S, D>, CLD>;
            self.check_cache_params()?;
            Ok(())
        }
    */
}

// FIXME: I think this is the root of all of the Cache implementations,
// but it's not yet clear. I'm feeling pretty good about this, however.
impl<const N: usize, const W: usize, const S: usize, D, A>
    CacheBase<N, W, S, D, A> for Cache<N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    /*
    // FIXME: revise this function
        // Report on whether any parameter problems are detected
        fn check_cache_params(&self) -> Result<(), Error> {
            <CL as CachelineBase<S, D, A>>::check_cacheline_params()?;
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
            let self_cache_base = self as &dyn CacheBase<Cacheline<S, D>>;
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
                <Cacheline<S, D> as CachelineBase<S, D, A>>
                ::cacheline_width();
            let self_cache_base = self as &dyn CacheBase<Cacheline<S, D>>;
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
    const S: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    my_cache: &'a Cache<N, W, S, D, A>,
    my_scrub_areas: &'a [MemArea<A>],
    _marker4: PhantomData<D>,
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > MemoryScrubber<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
}

impl<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    > MemoryScrubberBase<N, W, S, D, A>
    for MemoryScrubber<'a, N, W, S, D, A>
where
    D: DataImplTrait<D> + 'a,
    A: AddrImplTrait<A>,
    *mut D: From<A>,
    usize: From<A>,
{
    fn cache(&self) -> &Cache<N, W, S, D, A> {
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
        p: *const D) -> usize,
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
            p: *const D) -> usize) -> CCacheBase {
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
    A: AddrImplTrait<A>,
{
    fn next(&mut self) -> Addr<A>;
}

/*
struct AutoScrub<'a, const N: usize, const W: usize, D,
    const S: usize>
where
    D:      Unsigned,
    A: AddrImplTrait<A>,
{
    scrubber:   MemoryScrubber<'a, N, W, S, D>,
    desc:       &'a mut dyn AutoScrubDesc<CL>,
    // FIXME: Remove when possible. Right now, the compiler doesn't appear
    // to know that U is actually used when it's in CacheBase<CL>. So, this
    // works around that problem
    _marker1:    PhantomData<D>,
}

impl<'a, const N: usize, const W: usize, D: DataImplTrait<D>, const S: usize>
AutoScrub<'a, N, W, S, D>
where
    A: AddrImplTrait<A>,
{
/*
    fn new(cache_in: &'a C, scrub_areas: &'a [MemArea],
        desc: &'a mut dyn AutoScrubDesc<CL>) ->
        Result<AutoScrub<'a, N, W, S, D>, Error> {

        let scrubber = match MemoryScrubber::<'a, N, W, S, D>
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

impl<'a, const N: usize, const W: usize, D: DataImplTrait<D>, const S: usize>
CachelineBase<S, D, A>
for AutoScrub<'a, N, W, S, D>
where
    A: AddrImplTrait<A>,
{
}

impl<'a, const N: usize, const W: usize, D: DataImplTrait<D>, const S: usize>
CacheBase<N, W, S, D>
for AutoScrub<'a, N, W, S, D>
where
    A: AddrImplTrait<A>,
{
}
*/

#[cfg(test)]
mod tests {
//    use num_traits::Num;

    //    use std::borrow::BorrowMut;
    use std::fmt;
    use std::marker::PhantomData;
    use std::ops::Index;
    //    use std::ptr;
    //use std::slice;
    //use std::time::Instant;

    use crate::*;

    type VAddrType = u64;
    type VAddr = Addr<VAddrType>;

    // Cache characteristics for basic tests

    // BASIC_CACHELINE_WIDTH - number of bits required to index a byte in a
    //      cache line
    // BASIC_CACHE_INDEX_WIDTH - number of bits used as a cache line index in
    //      the cache
    // BASIC_MEM_SIZE - Cache size, in bytes
    const BASIC_ECCDATA_WIDTH: usize = usize::BITS as usize
        - 1
        - std::mem::size_of::<BasicECCData>().leading_zeros() as usize;
    const BASIC_CACHELINE_WIDTH: usize = 6 + BASIC_ECCDATA_WIDTH;
    const BASIC_CACHE_INDEX_WIDTH: usize = 10;
    const BASIC_MEM_SIZE: VAddr = VAddr {
        addr: (1 << (BASIC_CACHELINE_WIDTH + BASIC_CACHE_INDEX_WIDTH)) as VAddrType,
    };

    // ECCData - The data size used to compute the ECC for basic tests
    // BasicCacheline - the data type of a cache line
    type BasicECCData = u64;

    // Cache characteristics
    // TEST_CACHE_INDEX_WIDTH - number of bits used as a cache line index
    //  in the cache
    // TEST_CACHE_LINES - number of cache lines
    /* FIXME: restore this
        const TEST_CACHE_INDEX_WIDTH: usize = 10;
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
    const TEST_CACHE_NUM_TOUCHED: usize = 3;
    const TEST_SANDBOX_SIZE: usize =
        TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED;
    const TEST_COOKIE: usize = 17;

    // Verify an error is returned if the number of cache lines, the size of
    // the basic data, or the number of cache line items is not a power of two
    // OK_N:     Number of cache lines
    // OK_W:     Number of ways in each cache line
    // OkD:      Type of the smallest unit written to memory
    // OK_S:     Number of OkD items in a cache line
    const OK_N: usize = 1024;
    const OK_W: usize = 16;
    type OkD = u32;
    const OK_S: usize = 8;

    // Type used for the read counter
    type NRead = OkD;

    // Traits for testing
    // ==================
    trait TestCachelineDataBase<const S: usize, D>:
        CachelineDataBase<S, D> + Index<usize>
    where
        D: DataImplTrait<D>,
    {
    }

    trait TestCachelineBase<const S: usize, D, A>:
        TestCachelineBase_<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    trait TestCachelineBase_<const S: usize, D, A>: CachelineBase<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    // TestCacheBase - Description of the cache for basic tests
    // cache_index_width - Number of times this cacheline was iit during the
    //      scrub
    // read_infos:          Array of ReadInfo items>
    trait TestCacheBase<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A
    >: CacheBase<N, W, S, D, A>
    where
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn new() -> Self
        where
            Self: Sized;
//        fn cacheline(&self) -> *const TestCacheline<S, D>;
    }

    trait TestMemoryScrubberBase<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    >: MemoryScrubberBase<N, W, S, D, A>
    where
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn check_scrubber_params(cache: &dyn TestCacheBase<'a, N, W, S, D, A>,
            scrub_areas: &[MemArea<A>]) ->
            Result<(), Error> where Self: Sized {
        /*
            println!("In TestMemoryScrubberBase");
                    <TestMemoryScrubber<C, CL, CLD, N, W, S, D> as
                            MemoryScrubberBase<C, CL, CLD, N, W, S, D>>
                        ::check_scrubber_params(cache, scrub_areas)?;
            println!("In TestMemoryScrubberBase: no problem");
*/
            Ok(())
        }
    }

    // Types for testing
    // =================
    struct TestCache<
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    >
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        cache: Cache<N, W, S, D, A>,
        cacheline: TestCacheline<S, D, A>,
        // FIXME: Remove when possible. Right now, the compiler doesn't appear
        // to know that U is actually used when it's in CacheBase<CL>. So, this
        // works around that problem
        _marker1: PhantomData<D>,
        _marker2: PhantomData<A>,
    }

    // /*
    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn new() -> Self {
            let cache = Cache::<N, W, S, D, A>::new();
            let cacheline = TestCacheline::new();

            let test_cache = Self {
                cache: cache,
                cacheline: cacheline,
                _marker1: PhantomData,
                _marker2: PhantomData,
            };

            test_cache
        }
    }

    /*
        impl<'a, CL, CLD, const N: usize, const W: usize, const S: usize, D>
        Test<'a, CLD, N, W, S, D>
        for TestCache<CL, CLD, N, W, S, D>
        where
            CL: TestCachelineBase<S, D>,
            CLD: TestCachelineDataBase<S, D>,
            D: DataImplTrait<D>,
        {
            fn test(&mut self, _cacheline_ptr: *const CLD) {
    panic!("TestCache::Test unimplemented");
            }
        }
    */

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > CacheBase<N, W, S, D, A> for TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > TestCacheBase<'a, N, W, S, D, A>
        for TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn new() -> Self {
            let cache = Cache::<N, W, S, D, A>::new();
            let cacheline = TestCacheline::new();

            let test_cache = Self {
                cache: cache,
                cacheline: cacheline,
                _marker1: PhantomData,
                _marker2: PhantomData,
            };

            test_cache
        }
//        fn cacheline(&self) -> *const TestCacheline<S, D> {
//            &self.cacheline
//        }
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > fmt::Debug for TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self.cacheline)
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl<
            'a,
            const N: usize,
            const W: usize,
            D,
            const S: usize,
            A,
        > Sync for TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > Clone for TestCache<N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn clone(&self) -> Self {
            TestCache {
                cache: self.cache.clone(),
                cacheline: self.cacheline.clone(),
                _marker1: PhantomData,
                _marker2: PhantomData,
            }
        }
    }
/*
    // */
    #[derive(Clone, Copy)]
    #[repr(C)]
    struct TestCachelineData<const S: usize, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        data: [D; S],
        _marker1: PhantomData<A>,
    }

    impl<const S: usize, D, A> CachelineDataBase<S, D> for TestCachelineData<S, D, A> where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    // FIXME: this seems to be where the problem starts
    impl<const S: usize, D, A> TestCachelineDataBase<S, D>
        for TestCachelineData<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<const S: usize, D, A> Index<usize> for TestCachelineData<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        type Output = D;

        fn index(&self, i: usize) -> &Self::Output {
            &self.data[i]
        }
    }

    struct TestCacheline<const S: usize, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        _marker1: PhantomData<D>,
        _marker2: PhantomData<A>,
    }

    impl<const S: usize, D, A> TestCacheline<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn new() -> Self {
            TestCacheline {
                _marker1: PhantomData,
                _marker2: PhantomData,
            }
        }
    }

    impl<const S: usize, D, A> CachelineBase<S, D, A> for TestCacheline<S, D, A> where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<const S: usize, D, A> TestCachelineBase<S, D, A> for TestCacheline<S, D, A> where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<const S: usize, D, A> TestCachelineBase_<S, D, A> for TestCacheline<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        /*
                fn new() -> Self where Self: Sized {
                    TestCacheline {
                    }
                }
        */
    }

    impl<const S: usize, D, A> Clone for TestCacheline<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn clone(&self) -> TestCacheline<S, D, A> {
            TestCacheline {
                _marker1: PhantomData,
                _marker2: PhantomData,
            }
        }
    }

    impl<const S: usize, D, A> fmt::Debug for TestCacheline<S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", "TestCacheline...")
        }
    }

    struct TestMemoryScrubber<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
        A,
    >
    where
        D: DataImplTrait<D> + 'a,
        *mut D: From<A>,
        A: AddrImplTrait<A>,
        usize: From<A>,
    {
        cache:  TestCache::<N, W, S, D, A>,
        my_scrub_areas: &'a [MemArea<A>],
        read_infos: Vec<ReadInfo<D, A>>,
        _marker1: PhantomData<D>,
        _marker2: PhantomData<A>,
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
    > TestMemoryScrubber<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn new(
            scrub_areas: &'a [MemArea<A>]
        ) -> Result<TestMemoryScrubber<'a, N, W, S, D, A>, Error>
        {
            let test_cache = TestCache::<N, W, S, D, A>::new();

            <TestMemoryScrubber<'a, N, W, S, D, A> as TestMemoryScrubberBase::<N, W, S, D, A>>::check_scrubber_params(
                &(&test_cache as &Cache<N, W, S, D, A>),
                scrub_areas,
            )?;

            Ok(TestMemoryScrubber::<N, W, S, D, A> {
                cache: test_cache,
                my_scrub_areas: scrub_areas,
                read_infos: vec!(),
                _marker1: PhantomData,
                _marker2: PhantomData,
            })
        }
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
    > TestMemoryScrubberBase<'a, N, W, S, D, A>
    for TestMemoryScrubber<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
    > MemoryScrubberBase<N, W, S, D, A>
    for TestMemoryScrubber<'a, N, W, S, D, A>
    where
        // Not sure why I have to add CachelineBase here
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn cache(&self) -> &Cache<N, W, S, D, A> {
            println!("TestMemoryScrubber:MemoryScrubberBase1: entered");
            let c: &Cache<N, W, S, D, A>;
            c = &self.cache;
            c
        }

        fn scrub_areas(&self) -> &[MemArea<A>] {
             &self.my_scrub_areas
        }
    }

    impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
        > VerifyScrubber
    for TestMemoryScrubber<'a, N, W, S, D, A>
    where
        // Not sure why I have to add CachelineBase here
        D: DataImplTrait<D> + 'a,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        *mut u32: From<A>,
        usize: From<A>,
    {
        fn verify(&mut self) {
            // FIXME: what does this code actually do?
//let x: D = self.cache;
            let cacheline_ptr: * const TestCacheline<S, D, A> = &self.cache.cacheline;

            let test_cacheline_data = TestCachelineData::<OK_S, OkD, A>{
                data: [0.into(); OK_S],
                _marker1: PhantomData,
            };
            let mut data: [TestCachelineData<OK_S, OkD, A>; 10] =
                [test_cacheline_data; 10];
            let cacheline_ptr: * const TestCachelineData<OK_S, OkD, A> =
                data.as_ptr();
            let read_infos = &self.read_infos;

            // Ensure that allocated_area is used
            for read_info in read_infos {
                let allocated_area = &read_info.mem.allocated_area;
// FIXME: what is this next statement supposed to be?
                assert_eq!(allocated_area[0], allocated_area[0]);
            }
        }
    }

    trait VerifyScrubber {
        fn verify(&mut self);
    }

    struct Tester<'a, const N: usize, const W: usize, const S: usize, D, A>
    where
        // Not sure why I have to add CachelineBase and TestCachelineBase_ here
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        cache: &'a dyn TestCacheBase<'a, N, W, S, D, A>,
        // FIXME: can this be a reference to a slice?
        read_infos: Vec<ReadInfo<D, A>>,
    }

/*
    impl<'a, const N: usize, const W: usize, const S: usize, D, A>
    Tester<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        pub fn new(
            cache: &'a TestCache<N, W, S, D, A>,
            sizes: &'a [usize],
        ) -> Tester<'a, N, W, S, D, A> {
            let mut read_infos = Vec::<ReadInfo>::new();

            // Allocate memory and associated data structures
            for size in sizes {
                let mem = Mem::new::<S, D>(*size as VAddr);

                let n_reads_size = GUARD_LINES + scrub_area_size + GUARD_LINES;
                read_infos.push(ReadInfo::new(n_reads_size, mem));
            }
            Tester {
                cache: cache,
                read_infos: read_infos,
                _marker1: PhantomData,
            }
        }

        pub fn test(&mut self, _cacheline_ptr: *const CL) {
            self.set_cookie(_cacheline_ptr);
            self.increment_n_reads(_cacheline_ptr);
        }

        fn set_cookie(&mut self, _cacheline_ptr: *const CL) {
            unimplemented!();
        }

        fn increment_n_reads(&mut self, _cacheline_ptr: *const CL) {
            // Update the read count
            let index = {
                //                self.read_index(self.cache, _cacheline_ptr)
                self.read_index(_cacheline_ptr)
            };
            let n_reads = { self.get_n_reads(_cacheline_ptr) };

            n_reads[GUARD_LINES + index] += 1;
            n_reads[GUARD_LINES + index] += 1;
                self.read_index(_cacheline_ptr)
            };
            let n_reads = { self.get_n_reads(_cacheline_ptr) };

            n_reads[GUARD_LINES + index] += 1;
            println!("n_reads[{}] became {}", index, n_reads[GUARD_LINES + index]);
        }

        // Compute the index into the n_read array for this address. This
        // array has GUARD_LINES elements surrounding the actual counts.
        // _cacheline_ptr: Pointer to the address
        fn read_index(
            &mut self,
            //            cache: &'a dyn TestCacheBase<N, W, S, D>,
        ) -> usize {
            let cacheline_addr = _cacheline_ptr as VAddr;

            let read_info = self.find_read_info(_cacheline_ptr);
            let scrub_area = read_info.mem.scrub_area.clone();
            let start_addr = scrub_area.start() as VAddr;

            let index = (cacheline_addr - start_addr) / cacheline_size;
            assert!(index < n_n_reads);
            index
        }

        // Returns a reference to n_reads[], the array of count read counts
        fn get_n_reads(&mut self, _cacheline_ptr: *const CL) -> &mut Vec<NRead> {
            let read_info = self.find_read_info(_cacheline_ptr);
            read_info.n_reads.as_mut().unwrap()
        }

        // Find a ReadInfo corresponding to a given location in memory
        fn find_read_info(&mut self, _cacheline_ptr: *const CL) -> &mut ReadInfo {
            let cacheline_addr = _cacheline_ptr as VAddr;

            for search_read_info in &mut self.read_infos.iter_mut() {
                let scrub_area = &search_read_info.mem.scrub_area;
                let start_addr = scrub_area.start() as VAddr;
                let end_addr = scrub_area.end() as VAddr;

                if cacheline_addr >= start_addr && cacheline_addr <= end_addr {
                    return search_read_info;
                }
            }

            // If we failed, it's because the cache addess wasn't in any of
            // the MemAreas.
            panic!("Unable to find address {:x}", cacheline_addr);
        }
    }

    impl<'a, const N: usize, const W: usize, const S: usize, D, A> Clone
        for Tester<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn clone(&self) -> Tester<'a, N, W, S, D> {
            let read_infos = self.read_infos.clone();
            Tester {
                cache: self.cache,
                read_infos: read_infos,
                _marker1: PhantomData,
            }
        }
    }

    impl<'a, const N: usize, const W: usize, const S: usize, D, A> fmt::Debug
        for Tester<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }
*/

    // FIXME: can these be combined?

    unsafe impl<A> Sync for MemArea<A>
    where
        A: AddrImplTrait<A>
    {}

    // FIXME: should this be moved?
    unsafe impl<
            'a,
            const N: usize,
            const W: usize,
            const S: usize,
            D,
            A,
    > Sync for MemAreaIterator<'a, N, W, S, D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
        *mut D: From<A>,
        usize: From<A>,
    {
    }

    // Description of memory that is read into by the read_cacheline()
    // function. This keeps the actual allocation together with the pointer
    // into that allocation so that things go out of scope at the same time.
    //
    // allocated_area - Vec<D> of elements that can be read by
    //      read_cacheline()
    // start - Cache size-aligned pointer of the first byte to use in allocated_area
    // end - Pointer to the last byte
    #[derive(Clone, Debug)]
    struct Mem<D, A>
    where
        A: AddrImplTrait<A>,
    {
        allocated_area: Vec<D>,
        scrub_area: MemArea<A>,
    }

    impl<D, A> Mem<D, A>
    where
        D: DataImplTrait<D>,
        A: AddrImplTrait<A>,
    {
        // Allocates a memory area on a cache line boundary
        //
        //
        // Returns: a Mem with a Vec<D>. The size of the Vec<D> is
        // opaque but the p element in the Mem has at least size bytes
        // starting at a cache line aligned section of memory. The size
        // element is the size used to call this function.
        //        fn new<const S: usize, D>(size: Addr<A>) -> Mem
        fn new<const S: usize>(size: Addr<A>) -> Mem<D, A>
        where
            D: DataImplTrait<D>,
            A: AddrImplTrait<A>,
            *mut D: From<A>,
            usize: From<A>,
        {
            let cacheline_size = Cacheline::<S, D>::cacheline_size();
            Self::new_aligned(size, cacheline_size)
        }

        fn new_aligned(size: Addr<A>, alignment_size: usize) -> Mem<D, A> {
            if size == 0.into() {
                panic!("Allocation failure: {:?}", Error::ZeroSize);
            }

            let _dummy = value_to_width(alignment_size).expect(
                "new_alignment: alignment_size must be power of two",
            );

            // Allocate memory, which includes a cache size-sided area before
            // what we are touching. These areas should not be touched by
            // scrubbing.
            let allocated_size = alignment_size + size.into();
            let allocated_area: Vec<D> = vec![0.into(); allocated_size];
            let alignment_size_addr = alignment_size.into();

            // Now find the first cache line aligned pointer
            let start_addr = (allocated_area.as_ptr().into()
                + alignment_size_addr
                - 1)
                & !(alignment_size_addr - 1.into());
            let end_addr = start_addr + size - 1.into();

            Mem::<D, A> {
                allocated_area: allocated_area,
                scrub_area: MemArea::<A> {
                    s: start_addr,
                    e: end_addr,
                },
            }
        }
    }

    // This clues the compiler in that I know what I'm doing by having a
    // *const pointer in the struct
    unsafe impl<D, A> Sync for Mem<D, A>
    where
        A: AddrImplTrait<A>,
    {}

    // Data structure a memory allocation along with counters for how many
    // times cache line-sized memory areas has been read.
    // mem:     Boundaries of the address covered by this structure
    // n_reads: Counters, one per cache line-sized area.
    #[derive(Clone, Debug)]
    struct ReadInfo<D, A>
    where
        A: AddrImplTrait<A>,
    {
        mem: Mem<D, A>,
        n_reads: Option<Vec<NRead>>,
    }

    impl<D, A> ReadInfo<D, A>
    where
        A: AddrImplTrait<A>,
    {
        // Allocate a vector for counters
        // size:    Size in cache lines
        // mem:     Associated memory
        fn new(size: usize, mem: Mem<D, A>) -> ReadInfo<D, A> {
            let n_reads = vec![0; size];
            ReadInfo::<D, A> {
                mem: mem,
                n_reads: Some(n_reads),
            }
        }
    }

/*
    // Verify that the number of cache lines is a power of two
    #[test]
    fn test_unaligned_parameters() {
        test_unaligned_parameter(OK_N, "N - number of cache lines");
        test_unaligned_parameter(OK_W, "N - number of cache ways");
        test_unaligned_parameter(
            std::mem::size_of::<OkD>(),
            "size_of<D> - size of cache line element",
        );
        test_unaligned_parameter(
            OK_S,
            "S - number of cache line elements per cache line",
        );
    }

    fn test_unaligned_parameter(value: usize, label: &str) {
        let width = value_to_width::<usize>(value);
        if width.is_err() {
            panic!("Value must be a power of two: {}", label);
        }
    }

    fn test_scrubber_deleteme<
        'a,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
    >(
        param_name: &str,
    ) where
        D: DataImplTrait<D>,
    {
        // Want to allocate a few times the cache size for this
        const CACHE_SIZE: usize = OK_N * std::mem::size_of::<OkD>() * OK_S;
        const MEM_SIZE: VAddr = (3 * CACHE_SIZE) as VAddr;

        // Allocate a memory area using known good parameters
        let mut mem = Mem::new::<TestCacheline<S, D>, S, D>(MEM_SIZE);
        let mut scrub_areas = Vec::<MemArea<A>>::new();
        scrub_areas.push(mem.scrub_area);

        println!(
            "Test params <{}, {}, {}, {}>",
            N,
            W,
            std::mem::size_of::<D>(),
            S
        );
        let t1: TestCacheline<S, D>;
        let t2: TestCachelineData<S, D>;
        let t3: TestCacheline<S, D>;
        let t4: TestCache<
            TestCacheline<S, D>,
            TestCachelineData<S, D>,
            N,
            W,
            D,
            S,
            MemArea<A>,
        >;
        let t5: TestMemoryScrubber<
            TestCache<
                TestCacheline<S, D>,
                TestCachelineData<S, D>,
                N,
                W,
                D,
                S,
                MemArea<A>,
            >,
            TestCacheline<S, D>,
            TestCachelineData<S, D>,
            N,
            W,
            D,
            S,
            MemArea<A>,
        >;

        let cl_size = TestCacheline::<S, D>::cacheline_size();
        let one_size = TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED * cl_size
            as VAddr;
        let sizes = [one_size, one_size, one_size];
        // What should I be doing with test_cache?
        let (test_cache, scrub_areas) = setup_test_desc_areas::<
            TestCache<
                'a,
                TestCacheline<S, D>,
                TestCachelineData<S, D>,
                N,
                W,
                D,
                S,
                MemArea<A>,
            >,
            TestCacheline<S, D>,
            TestCachelineData<S, D>,
            N,
            W,
            D,
            S,
            MemArea<A>,
        >(&sizes);

        let t6 = TestMemoryScrubber::<
            TestCache<
                TestCacheline<S, D>,
                TestCachelineData<S, D>,
                N,
                W,
                D,
                S,
                MemArea<A>,
            >,
            TestCacheline<S, D>,
            TestCachelineData<S, D>,
            N,
            W,
            D,
            S,
            MemArea<A>,
        >::new;
        let memory_scrubber = t6(&scrub_areas);
    }

    // Check that a problem with a given Mem scrub area is properly detected
    //
    // mem: the Mem with bad scrub scrub area
    // err: the expected Error value
    fn check_scrub_area_error(mem: Mem, err: Error) {
        let mut scrub_areas = Vec::<MemArea<A>>::new();
        scrub_areas.push(mem.scrub_area);
        let memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);

        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(), err);
    }

    // Verify that an error is returned if the starting address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_start() {
        let mut mem = Mem::new::<TestCacheline<OK_S, OkD>, OK_S, OkD>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.start() += 1;
        check_scrub_area_error(mem, Error::UnalignedStart);
    }

    // Verify that an error is returned if the ending address is not
    // aligned on a cache line boundary
    #[test]
    fn test_unaligned_end() {
        let mut mem = Mem::new::<TestCacheline<OK_S, OkD>, OK_S, OkD>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.end += 1;
        check_scrub_area_error(mem, Error::UnalignedEnd);
    }

    // Verify that an error is returned if there are no areas defined
    #[test]
    fn test_null_areas() {
        let scrub_areas = Vec::<MemArea<A>>::new();
        let memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);
        assert!(memory_scrubber.is_err());
        assert_eq!(memory_scrubber.err().unwrap(), Error::NoMemAreas);
    }

    // Verify that an error is returned if the size is zero.
    #[test]
    fn test_zero_size() {
        let mut mem = Mem::new::<TestCacheline<OK_S, OkD>, OK_S, OkD>(
            BASIC_MEM_SIZE,
        );
        mem.scrub_area.end = mem.scrub_area.start();
        check_scrub_area_error(mem, Error::EmptyMemArea);
    }

    // Verify that a small scrub with good parameters can be done.
    #[test]
    fn test_aligned() {
        const cacheline_data: TestCachelineData<OK_S, OkD> =
            TestCachelineData::<OK_S, OkD> { data: [0; OK_S] };

        const cacheline: TestCacheline<OK_S, OkD> =
            TestCacheline::<OK_S, OkD> {
                _marker1: PhantomData,
            };

        let cache: TestCache<
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        > = TestCache::<
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new();

        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let cache_lines = CacheBase::cache_lines(&cache) as A;
        let alloc_size = cacheline_size * cache_lines * 14;
        let mut mem =
            Mem::new::<TestCacheline<OK_S, OkD>, OK_S, OkD>(alloc_size);
        let mut scrub_areas = Vec::<MemArea<A>>::new();
        scrub_areas.push(mem.scrub_area);
        let mut memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas);
        assert!(memory_scrubber.is_ok());
        let scrubber = memory_scrubber.unwrap();
        let res = scrubber.scrub(cacheline_size * 10);

        if let Err(e) = res {
            panic!("scrub failed: {}", e);
        }
    }

    // Verify that all specified locations are scrubbed and locations outside
    // the requested are are not touched.
    #[test]
    fn test_touch_zero() {
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = 0;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_one() {
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = cacheline_size;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_many() {
        const MANY: usize = 50;
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = cacheline_size * MANY;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_all() {
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_double_all() {
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = 2 * cacheline_size * TEST_SANDBOX_SIZE;
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_more_many() {
        const MANY: usize = 72;
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = 5 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        test_scrubber(&[cacheline_size * TEST_SANDBOX_SIZE], first_area);
    }

    #[test]
    fn test_touch_multiple_areas() {
        const MANY: usize = 72;
        let cacheline_size = TestCacheline::<OK_S, OkD>::cacheline_size();
        let first_area = 2 * cacheline_size * (TEST_SANDBOX_SIZE + MANY);
        let second_area = cacheline_size * TEST_SANDBOX_SIZE;
        let third_area = cacheline_size * MANY;
        let scrub_areas = [first_area, second_area, third_area];
        test_scrubber(&scrub_areas, first_area);
    }

    /*
        #[test]
        fn test_big() {
            const MEM_AREA_SIZE: usize = 1 * 1024 * 1024 * 1024;

            let cl: TestCachelineBase /* 32 */;
            //let mut basic_cache = BASIC_CACHE_DESC.clone();
            //let cache = &mut basic_cache;
            let cache = TestCache::<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                 OK_N, OK_W, OK_S, OkD>::new();
            let mem = Mem::new::<TestCacheline>(MEM_AREA_SIZE);
            let mut scrub_areas = Vec::<MemArea<A>>::new();
            scrub_areas.push(mem.scrub_area);
            let mut scrubber = TestMemoryScrubber::<
                TestCache<
                    TestCacheline<OK_S, OkD>,
                    TestCachelineData<OK_S, OkD>,
                    OK_N, OK_W, OK_S, OkD
                >,
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N, OK_W, OK_S, OkD
            >::new(&scrub_areas).unwrap();

    //        let mut scrubber =
    //            match MemoryScrubber::<TestCacheBase, TestCacheline>::
    //            new(cache, &scrub_areas) {
    //            Err(e) => panic!("Could not create MemoryScrubber: {}",
    //                e),
    //            Ok(scrubber) => scrubber,
    //        };

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
            let cl: TestCachelineBase /* 32 */;
            let one_size = TEST_CACHE_LINES * TEST_CACHE_NUM_TOUCHED * cl.size()
                as A;
            let single_scan = one_size / 2;
            let total_scan = single_scan * 4 + 3 * cl.size();

            struct TestAutoScrubDesc {
                count:      Addr<A>,
                scrub_size: Addr<A>,
            }

            impl<'a, CD: TestCacheBaseTrait<'a, CL, D>, CL: TestCachelineBase<'a, D>,
                D: TestCacheData<D> + Index<usize>>
                AutoScrubDesc<CD, CL>
                for TestAutoScrubDesc {
                fn next(&mut self) -> Addr<A> {
                    let n = if self.count > self.scrub_size { self.scrub_size }
                        else { self.count };
                    self.count -= n;
                    n
                }
            }

            let sizes = [one_size, one_size, one_size];
            let (test_cache, scrub_areas) =
                setup_test_desc_areas::<TestCachelineBase /* 32 */>(&sizes);

            let mut autoscrub_desc = TestAutoScrubDesc {
                count: total_scan,
                scrub_size: single_scan,
            };

            let mut autoscrub = match AutoScrub::new(&test_cache, &scrub_areas,
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

        struct IterCacheline<const S: usize, D>
        where
            D:  Num,
        {
            data: [D; S],
        }

        impl<const S: usize, D>
        TestCachelineBase<S, D>
        for IterCacheline<S, D>
        where
            D: DataImplTrait<D>,
        {
        }

        #[derive(Clone)]
        struct IterCacheBase {
            cache_index_width: usize,
        }

        impl IterCacheBase {
            fn new(cache_index_width: usize) -> IterCacheBase {
                IterCacheBase {
                    cache_index_width: cache_index_width,
                }
            }
        }

        impl<'a, CL, CLD, const N: usize, const W: usize, const S: usize, D>
        CacheBase<CL, CLD, N, W, S, D>
        for IterCacheBase
        where
            CL: IterCacheline<'a, S, D>,
            D:  Num,
        {
        }

        impl<CL, CLD, const N: usize, const W: usize, const S: usize, D>
        CacheBase<CL, CLD, N, W, S, D>
        for IterCacheBase {
    /*
            fn cacheline_width(&self) -> usize {
                unimplemented!();
            }
    */

            fn cache_index_width(&self) -> usize {
                self.cache_index_width
            }

    /*
            fn read_cacheline<'a>(&self,
                _cacheline_ptr: *const dyn CachelineBase<'a>) {
    // FIXME: add more checks
                // Assure that IterCacheline::data is read
                unsafe {
                    assert_eq!((*_cacheline_ptr).data[0], (*_cacheline_ptr).data[0]);
                }
            }
    */
        }

        #[test] #[ignore]
        fn test_iter_scrublines() {
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            // Create areas that increasingly, then decreasingly, overlap in their
            // cache indices.
            let mut scrub_areas = Vec::<MemArea<A>>::new();
            for i in 0..5 {
                let offset = i * cl_size;
                let size = offset + 3 * cl_size;
                let mut mem = Mem::new_aligned(size, c_size);
                mem.scrub_area.start() =
                    (mem.scrub_area.start() as usize + offset) as *const D;
                scrub_areas.push(mem.scrub_area);
            }

            let mut b = Vec::<usize>::new();
            for scrub_area in &scrub_areas {
                b.push(scrub_area.start() as usize);
            }

            // We have to have enough cache lines available to avoid wrapping
            // when we go through the memory areas
            assert!(iter_cache.cache_lines() > scrub_areas.len());

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

            let mut iterator = ScrubAreasIterator::new(&iter_cache,
                &scrub_areas).expect("ScrubAreasIterator failed");

            for (i, expected) in (0..expecteds.len()).into_iter()
                .zip(expecteds.iter()) {
                let expected_p = *expected as *const IterCacheline;
                let actual = match iterator.next() {
                    None => panic!("Ran out of scrub areas too soon"),
                    Some(actual) => actual,
                };
                let cache_index =
                    iter_cache.cache_index(actual as *const D);
                if actual != expected_p {
                    println!("{}: actual {:p} expected {:p} index {}", i, actual, expected_p, cache_index);
                    let actual_breakdown = breakdown(&iter_cache,
                            actual as *const IterCacheline, &scrub_areas, &b);
                    let expected_breakdown = breakdown(&iter_cache,
                        expected_p, &scrub_areas, &b);
                    eprintln!("   actual: b[{}] + {} * cl_size expected: b[{}] + {} * cl_size",
                        actual_breakdown.0, actual_breakdown.1 / cl_size,
                        expected_breakdown.0, expected_breakdown.1 / cl_size);

                    print_scrub_areas(&iter_cache, &scrub_areas);
                }
                assert_eq!(actual, expected_p);
            }
            let next = iterator.next();
            assert!(next.is_some());
        }

        // Find the base and offset within the scrub areas
        fn breakdown(iter_cache: &IterCacheBase,
            expected: *const IterCacheline<OK_S, OkD>, scrub_areas: &Vec::<MemArea<A>>,
            b: &Vec::<usize>) -> (usize, usize) {
            let mut found: Option<usize> = None;
            for (j, scrub_area) in (0..scrub_areas.len()).into_iter()
                .zip(scrub_areas.iter()) {
                if ((scrub_area.start() as usize) <= expected as usize) &&
                    ((expected as usize) < scrub_area.end as usize) {
                    found = Some(j);
                    break;
                }
            }
            if found.is_none() {
                print_scrub_areas(&iter_cache, &scrub_areas);
                panic!("Unable to find address {:p}", expected);
            }
            let j = found.unwrap();
            let delta = expected as usize - b[j];
            (j, delta)
        }

        #[test] #[ignore]
        fn test_iter_cache_indices() {

            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            let mut scrub_areas = Vec::<MemArea<A>>::new();
            scrub_areas.push(Mem
                ::new_aligned(4 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas.push(Mem
                ::new_aligned(5 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[1].start() = (scrub_areas[1].start as usize +
                1 * cl_size) as *const D;
            scrub_areas.push(Mem
                ::new_aligned(6 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[2].start() = (scrub_areas[2].start as usize +
                2 * cl_size) as *const D;
            scrub_areas.push(Mem
                ::new_aligned(7 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[3].start() = (scrub_areas[3].start as usize +
                3 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(8 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[4].start() = (scrub_areas[4].start as usize +
                4 * cl_size) as *const u8;
            scrub_areas.push(Mem
                ::new_aligned(9 * cl_size, c_size).unwrap().scrub_area);
            scrub_areas[5].start() = (scrub_areas[5].start as usize +
                5 * cl_size) as *const u8;

            let base1 = scrub_areas[0].start() as usize;
            let base2 = scrub_areas[1].start() as usize;
            let base3 = scrub_areas[2].start() as usize;
            let base4 = scrub_areas[3].start() as usize;
            let base5 = scrub_areas[4].start() as usize;
            let base6 = scrub_areas[5].start() as usize;

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

            let mut iterator = CacheIndexIterator::new(&iter_cache,
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

            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
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
                let iterator = MemAreasIterator::new(&iter_cache,
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
                scrub_area: &'a MemArea<A>,
                expected:   &'a[usize],
            }
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
            let c_size = cl_size * (1 << ci_width);

            // These all start two cache lines above a cache-sized boundary
            let delta1 = 1 * cl_size;
            let mut phys_mem1 = Mem::new_aligned(delta1 + 2 * cl_size, c_size);
            phys_mem1.scrub_area.start() = (phys_mem1.scrub_area.start as usize +
                delta1) as *const u8;

            let delta2 = 1 * cl_size;
            let mut phys_mem2 = Mem::new_aligned(delta2 + 3 * c_size, c_size);
            phys_mem2.scrub_area.start() = (phys_mem2.scrub_area.start as usize +
                  delta2) as *const u8;

            let delta3 = 1 * cl_size;
            let mut phys_mem3 = Mem::new_aligned(delta3 + 1 * cl_size, c_size);
            phys_mem3.scrub_area.start() = (phys_mem3.scrub_area.start as usize +
                  delta3) as *const u8;

            let base1 = phys_mem1.scrub_area.start() as usize;
            let base2 = phys_mem2.scrub_area.start() as usize;
            let base3 = phys_mem3.scrub_area.start() as usize;

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
                let iterator = MemAreaIterator::new(&iter_cache, sax,
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
        fn print_scrub_areas(cache: &IterCacheBase,
            scrub_areas: &[MemArea]) {
            let mut total_bytes: usize = 0;
            let cacheline_width = cache.cacheline_width();

            // Print the tuples in the vector
            println!("Physical addresses:");
            for (i, scrub_area) in (0..scrub_areas.len()).into_iter()
                .zip(scrub_areas.iter()) {
                let delta_cachelines =
                    cache.size_in_cachelines(&scrub_area);
                let delta_bytes = delta_cachelines << cacheline_width;
                total_bytes += delta_bytes;
                println!("{}: {:p}-{:p}: {} ({} cache lines)", i,
                    scrub_area.start(), scrub_area.end, delta_bytes,
                    delta_cachelines);
            }

            println!("total size {} bytes ({} cache lines)", total_bytes,
                total_bytes >> cacheline_width);
        }

        #[test] #[ignore]
        fn test_iter() {
            let iter_cache = IterCacheBase::new(ITER_CACHE_INDEX_WIDTH);

            let cl_width = iter_cache.cacheline_width();
            let cl_size = 1 << cl_width;
            let ci_width = iter_cache.cache_index_width();
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
                let delta = iter_cache.size_in_cachelines(&scrub_area);
                total_bytes += delta;
            }
            total_bytes *= iter_cache.cacheline_size();

            // Print the tuples in the vector
            let cur_indices = [0];

            for _cur_index in cur_indices {
                let mut iterator = ScrubAreasIterator::new(&iter_cache,
                    &virt_scrub_areas).expect("ScrubAreasIterator failed");

                // Scan the entire set of MemAreas
                for _i in 0..total_bytes >> iter_cache.cacheline_width() {
                    let next = iterator.next();
                    let _p = next.unwrap();
    // FIXME: check against actuals
                };
            }
        }
    */

    // Test support function that scrubs a section of memory, then verifies
    // that things were properly touch or not touched.
    // sizes - array of sizes of memory areas to scrub
    // n - number of cache lines to scrub
    fn test_scrubber(sizes: &[usize], n: usize) {
        println!("In test_scrubber");
        let (test_cache, scrub_areas) = setup_test_desc_areas::<
            TestCache<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >(sizes);

        let mut memory_scrubber = TestMemoryScrubber::<
            TestCache<
                TestCacheline<OK_S, OkD>,
                TestCachelineData<OK_S, OkD>,
                OK_N,
                OK_W,
                OkD,
                OK_S,
            >,
            TestCacheline<OK_S, OkD>,
            TestCachelineData<OK_S, OkD>,
            OK_N,
            OK_W,
            OkD,
            OK_S,
        >::new(&scrub_areas)
        .unwrap();

        if let Err(e) = memory_scrubber.scrub(n) {
            panic!("scrub failed: {}", e);
        };

        memory_scrubber.verify();
    }

    // Set up a TestCacheBase and MemAreas
    fn setup_test_desc_areas<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
    >(
        sizes: &[usize],
    ) -> (TestCache<CL, CLD, N, W, S, D>, Vec<MemArea<A>>)
    where
        C: TestCacheBase<'a, CL, CLD, N, W, S, D>,
        CL: TestCachelineBase<S, D>,
        CLD: TestCachelineDataBase<S, D>,
        D: DataImplTrait<D> + 'a,
    {
        // FIXME: this had the argument sizes, why?
        let test_cache = TestCache::<CL, CLD, N, W, S, D>::new(sizes);

        // Allocate memory areas according to the given sizes
        let mut scrub_areas: Vec<MemArea<A>> = vec![];
        {
            let cacheline_ptr: *const CLD;

            for read_info in &test_cache.read_infos {
                scrub_areas.push(read_info.mem.scrub_area.clone());
            }
        }

        (test_cache, scrub_areas)
    }

    /// Verify the proper locations were hit
    ///
    /// * `scrubber` - MemoryScrubber to use
    ///
    /// * `n` - Bytes scrubbed
    ///
    /// This essentially reimplements the iterator code but in a more straight-
    /// forward way so that the two implementations can verify each other.
    fn verify_scrub<
        'a,
        C,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
    >(
        scrubber: &TestMemoryScrubber<'a, C, CL, CLD, N, W, S, D>,
        n: usize,
    ) where
        C: TestCacheBase<'a, CL, CLD, N, W, S, D>,
        CL: TestCachelineBase<S, D>,
        CLD: TestCachelineDataBase<S, D>,
        D: DataImplTrait<D> + 'a,
    {
        let cache = scrubber.cache();
        let cacheline_width = CL::cacheline_width();
        let n_in_cachelines = n >> cacheline_width;
        let cache_lines = cache.cache_lines();

        // Count the total number of scrub lines in all of the MemAreas
        let mut scrub_lines = 0;

        unimplemented!();
        /*
                for scrub_area in scrubber.iterator.scrub_areas {
                    scrub_lines += C::size_in_cachelines(scrub_area);
                }
        */

        unimplemented!();
        /*
                let n_min_reads = match (n_in_cachelines / scrub_lines)
                    .try_into() {
                    Err(e) => panic!("Internal Error: n_min_reads conversion failed: {}", e),
                    Ok(n_min_reads) => n_min_reads,
                };
        */
        let n_extra_reads = n_in_cachelines % scrub_lines;

        let mut verified = 0;

        /* FIXEME: restore this
                // Scan the cache indices
                for cache_index in 0..cache_lines {
                    verify_cache_index::<TestCacheBaseTrait>(&scrubber.cache(),
                        cache_index, n_min_reads, n_extra_reads, &mut verified);
                }
        */
    }

    // Verified that all of the areas for this cache line are correct
    // memory_scrubber: Memory scrubber
    // cache_index:     Cache index being verified
    // verified:        Number of cache indices verified so far
    //    fn verify_cache_index<CD: TestCacheBase<'a, CL>, CL: TestCachelineBase>(cache: &C,
    fn verify_cache_index<C>(
        cache: &C,
        cache_index: usize,
        n_min_reads: usize,
        n_extra_reads: usize,
        verified: &mut usize,
    ) {
        /*
        unimplemented!();
                for read_info in cache.test.borrow_mut().read_infos {
                    verify_read_info(cache, &read_info, cache_index,
                        n_min_reads, n_extra_reads, verified);
                }
        */
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
    fn verify_read_info<
        'a,
        CL,
        CLD,
        const N: usize,
        const W: usize,
        const S: usize,
        D,
    >(
        cache: &dyn TestCacheBase<'a, CL, CLD, N, W, S, D>,
        read_info: &ReadInfo,
        cache_index: usize,
        n_min_reads: usize,
        n_extra_reads: usize,
        verified: &mut usize,
    ) where
        CL: TestCachelineBase<S, D>,
        CLD: TestCachelineDataBase<S, D>,
        D: DataImplTrait<D>,
    {
        let cache_lines = cache.cache_lines();
        /*
                let size_in_cachelines =
                    cache.size_in_cachelines(&read_info.mem.scrub_area);
        */
        let size_in_cachelines = 1;
        unimplemented!();

        let start = read_info.mem.scrub_area.start();
        unimplemented!();
        let start_index = 0;
        /*
                let start_index =
                    cache.offset_to_next_index(start, cache_index);
        */
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
            println!(
                "n_extra_reads {} n_min_reads {}",
                n_extra_reads, n_min_reads
            );
            println!("n_expected {} n_actual {}", n_expected, n_actual);
            println!("i {}", i);
            let n_expected: NRead = n_expected
                .try_into()
                .expect("Numerical conversion failed");
            assert_eq!(n_actual, n_expected);

            unimplemented!();
            let mem_actual = 0;
            /*
                        let mem_actual = mem[i].data[0];
            */
            let mem_expected =
                if n_min_reads == 0 && *verified >= n_extra_reads {
                    0
                } else {
                    TEST_COOKIE
                };

            assert_eq!(mem_actual, mem_expected);
            unimplemented!();
            /*
                        for data in &mem[i].data[1..] {
                            let mem_actual = *data;
                            assert_eq!(mem_actual, 0);
                        }
            */

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
