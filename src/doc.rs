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

