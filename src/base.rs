// Base traits that can be used to construct non-standard memory scrubbersRun once through the scrubber loop
extern crate num_traits;

use core::fmt;
use core::mem;
//use core::ops::{Add};
use core::ptr;
//use num_traits::{PrimInt, Unsigned};
use num_traits::PrimInt;
use std::convert::From;
use std::iter;
use std::marker::PhantomData;
//use std::slice;

/*
mod crate::addr;
mod crate::base;
mod crate::data;
*/
/*
use crate::addr::{Addr, AddrImplTrait};
use crate::data::DataImplTrait;
*/
use crate::addr::*;
use crate::data::*;

// Error definitions. Because this is core software, it returns errors instread
// of panicing wherever possible. These are the errors it can return.

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

/// # Names for generic parameters
///
/// | Name | Description |
/// |---|---|
/// | D | Integer data type for ECC unit processing. That is, the ECC |
/// |   | unit processes as many bytes at a time as are in this data type. |
/// | A |  Integer data type representing the address. If this is usize, it |
/// |   | should be correct for virtual addresses. Physical addresses may be |
/// |   | larger. |
///
/// # Conventions for naming cache parameters:
///
/// | Name | Description |
/// | N | Number of cache lines |
/// | S | Number of bytes in a cache line |
/// | W | Number of ways per cache line |

pub trait AutoScrubDesc<
    const N: usize,
    const S: usize,
    const W: usize,
    D,
    A,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
{
    fn next(&mut self) -> Addr<A>;
}

// This is for a generic cache. There is a two-tier implementation where
// traits define the behavior for most caches but which allows for overriding
// methods to implement non-classical behavior.

pub trait MemoryScrubberBase<
    'a,
    const N: usize,
    const W: usize,
    const S: usize,
    D,
    A,
    I,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    I: ScrubAreasIteratorBase<'a, N, W, S, D, A>,
    usize: From<A>,
{
    /// Returns a reference to a CacheBase trait
    fn cache(&self) -> &dyn CacheBase<N, W, S, D, A>;

    /// Return cacheline width in number of D items
    fn cacheline_width(&self) -> usize {
        S / mem::size_of::<D>()
    }

    /// Return cacheline size in number of bytes
    fn cacheline_size(&self) -> usize {
        S
    }

    /// Return a references to an array of MemAreas.
    fn scrub_areas(&self) -> &[MemArea<A>];

    /// Check that the parameters for the cache make sense
    ///
    /// # Arguments:
    /// * `cache` - Reference to trait CacheBase to check
    ///
    /// * `scrub_areas` - Reference to an array of MemAreas being scrubbed
    ///
    /// # Returns:
    /// Either OK(()) or an Error
    fn check_scrubber_params(
        cache: &dyn CacheBase<N, W, S, D, A>,
        scrub_areas: &[MemArea<A>],
    ) -> Result<(), Error> {
        // Check the corresponding cache trait
        CacheBase::<N, W, S, D, A>::check_cache_params(cache)?;

        //  Verify scrub area descriptions
        if scrub_areas.len() == 0 {
            return Err(Error::NoMemAreas);
        }

        let cacheline_size = S;
        let cacheline_mask: Addr<A> = (cacheline_size - 1).into();

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

            if (end & cacheline_mask) != cacheline_mask {
                return Err(Error::UnalignedEnd);
            }
        }

        Ok(())
    }

    /// This is the core of the scrubbing work. We scrub the given number
    /// of bytes out of the total scrubbing areas supplied, starting after
    /// the previous location.
    ///
    /// # Arguments:
    ///
    /// * `n` - Number of bytes to scrub
    ///
    /// # Returns:
    /// Ok(()) or Error
    fn scrub(&self, n: Addr<A>) -> Result<(), Error>
    where
        Self: Sized,
        usize: From<A>,
    {
        let cacheline_size = S;
        let cacheline_size_mask = (cacheline_size - 1).into();
        let cacheline_width = self.cacheline_width();

        // Verify that the number of bytes to scrub is an even multiple of the
        // cacheline size
        if (n & cacheline_size_mask) != 0.into() {
            return Err(Error::UnalignedSize);
        }

        let n_scrublines = n >> Addr::<A>(cacheline_width.into());
// FIXME: Add ? operator
        let iterator =
            ScrubCountIterator::new(self.cache(), self.scrub_areas(), n_scrublines);

        // At this point, it's pretty much Iterators all the way down.
        for p in iterator {
            // FIXME: implement read_cacheline(p);
            eprintln!("Implemented read_cacheline()");
            //            Cacheline::<S, D>::read_cacheline(p);
        }

        Ok(())
    }
}

// Trait to iterate through cache lines. The next() function never returns
// None; it's up to the caller to implement any limit on number of cache
// lines iterated
pub trait ScrubAreasIteratorBase<
    'a,
    const N: usize,
    const W: usize,
    const S: usize,
    D,
    A,
>: iter::Iterator
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
{
    fn new(cache: &dyn CacheBase<N, W, S, D, A>, scrub_areas: &[MemArea<A>], n: Addr<A>) -> Self;
}

// Iterator for a given number of cache lines
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
    I,
> where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    I: ScrubAreasIteratorBase<'a, N, W, S, D, A>,
{
    cache: &'a dyn CacheBase<N, W, S, D, A>,
    scrub_areas: &'a [MemArea<A>],
    n_scrublines: Addr<A>,
    i: Addr<A>,
    iterator: I,
    _marker1: PhantomData<D>,
}

/*
impl<'a, const N: usize, const W: usize, const S: usize, D, A, I>
    ScrubCountIterator<'a, N, W, S, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    I: ScrubAreasIteratorBase<'a, N, W, S, D, A>,
{
}
*/

impl<'a, const N: usize, const W: usize, const S: usize, D, A, I>
    ScrubCountIterator<'a, N, W, S, D, A, I>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    I: ScrubAreasIteratorBase<'a, N, W, S, D, A>,
{
    fn new(
        cache: &'a dyn CacheBase<N, W, S, D, A>,
        scrub_areas: &'a [MemArea<A>],
        n_scrublines: Addr<A>,
    ) -> Result<ScrubCountIterator<'a, N, W, S, D, A, I>, Error> {
        let iterator = I::new(cache, scrub_areas, n_scrublines)?;

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

impl<'a, const N: usize, const W: usize, const S: usize, D, A, I>
    iter::Iterator for ScrubCountIterator<'a, N, W, S, D, A, I>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    I: ScrubAreasIteratorBase<'a, N, W, S, D, A>,
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

// This is the basic definition of cache line data. Note that is is never
// instantiated, though traits that use this trait usually are. Specifically,
// means that none these will have "self" parameter.
pub trait CachelineDataBase<const S: usize, D> {}

pub trait CachelineBase<const S: usize, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
{
    // Check cache line related parameters.
    //
    // #Returns:
    // Ok(()) or Error
    fn check_cacheline_params() -> Result<(), Error>
    where
        Self: Sized,
    {
        bit_width::<usize>(std::mem::size_of::<D>())?;
        bit_width::<usize>(S)?;
        Ok(())
    }

    // Return the number of bits required to hold the index into the cache
    // line
    fn cacheline_width() -> usize {
        let data_width = std::mem::size_of::<D>();
        
        bit_width::<usize>(data_width * S).unwrap()
    }

    // Return the number of bytes in a cache line
    fn cacheline_size() -> usize
    where
        Self: Sized,
    {
        1 << Self::cacheline_width()
    }

    // Return the size of a MemArea in cache lines
    fn size_in_cachelines(scrub_area: &MemArea<A>) -> Addr<A>
    where
        Self: Sized,
        A: AddrImplTrait<A>,
    {
        let cacheline_width: Addr<A> = Self::cacheline_width().into();

        let start = scrub_area.start();
        let start_in_cachelines = start >> cacheline_width;

        //let x = Addr(32);

        //let x = Addr<u128>(32);

        //let x: Addr::<u128>;
        //let x = Addr::<u128>::(32);
        let x = Addr::<u128>(32);
        let y = Addr::<u128>(4);
        //let y = Addr::<u128>::new(4);
        println!("x({}) + y({}) = {}", x, y, x + y);
        println!("x({}) >> y({}) = {}", x, y, x >> y);
        println!(
            "x({}) >> y({}) = {}",
            start,
            cacheline_width,
            start >> cacheline_width
        );

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
pub trait CacheBase<const N: usize, const W: usize, const S: usize, D, A>
where
    D: DataImplTrait<D>,
    A: AddrImplTrait<A>,
    usize: From<A>,
{
    // Report on whether any parameter problems are detected
    fn check_cache_params(&self) -> Result<(), Error> {
        bit_width::<usize>(N)?;
        bit_width::<usize>(W)?;
        Ok(())
    }

    // Return the number of bits used to index into the cache, i.e. the index
    // of a cache line in the cache. A cache with 1024 lines will have an
    // index using 10 bits.
    fn cache_index_width(&self) -> usize {
        //println!("CacheBase: cache_index_width: entered");
        bit_width::<usize>(N).unwrap()
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
        let start_index =
            <Self as CacheBase<N, W, S, D, A>>::cache_index(self, p);
        let cache_lines =
            <Self as CacheBase<N, W, S, D, A>>::cache_lines(self);

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
        let cacheline_width_usize =
            CachelineBase::<S, D, A>::cacheline_width();
        let cacheline_width: Addr<A> = cacheline_width_usize.into();
        // FIXME: can I just use Self now?
        let cache_index_width =
            <Self as CacheBase<N, W, S, D, A>>::cache_index_width(self);
        let shifted_index = p >> cacheline_width;
        //        let shifted_index_usize: usize = shifted_index.into();
        let shifted_index_usize: usize = usize::from(shifted_index.into());
        shifted_index_usize & ((1 << cache_index_width) - 1)
    }

    // Return the number of cache lines in the index.
    //
    // NOTE: You are unlikely to ever need to implement this
    fn cache_lines(&self) -> usize {
        // FIXME: can I just use Self now?
        1 << <Self as CacheBase<N, W, S, D, A>>::cache_index_width(self)
    }
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
    pub fn new(start: Addr<A>, end: Addr<A>) -> MemArea<A> {
        MemArea::<A> { s: start, e: end }
    }

    /// Returns the address of the first byte contained in the memory area
    ///
    /// # Returns:
    /// Addr<A> which is the address of the first byte
    fn start(&self) -> Addr<A> {
        self.s
    }

    /// Returns the address of the last byte contained in the memory area. Note
    /// that this is inclusive
    ///
    /// # Returns:
    /// Addr<A> which is the address of the last byte
    fn end(&self) -> Addr<A> {
        self.e
    }
}

/// Compute the minimum number of bits required to hold a given value.
/// The number must be a non-zero multiple of two.
///
/// # Type parameters
///
/// * `T` - PrimInt and Debug
///
/// # Attributes
/// * `size` - Type T value whose width is to be computed.
///
/// # Return:
/// * `Error` - If the value is not a multiple of two.
///
/// * `usize` - Number of bits required to hold the given value
pub fn bit_width<T: PrimInt + std::fmt::Debug>(
    size: T,
) -> Result<usize, Error> {
    if size == T::zero() {
        return Err(Error::ZeroSize);
    }

    let leading_zeros = size.leading_zeros(); // Type is u32
    let bits_per_byte = 8; // FIXME: should be T.BITS/size.BITS...
    let size_in_bytes = mem::size_of::<T>(); // Type is usize
    let size_in_bits = size_in_bytes * (bits_per_byte as usize);
    let width = (size_in_bits - 1) - (leading_zeros as usize);
    if size != (T::one() << width) {
        return Err(Error::UnalignedValue);
    }

    Ok(width)
}
