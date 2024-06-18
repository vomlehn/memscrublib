// Definitions to handle the type used to implement addresses

extern crate num_traits;

use core::ops::{Add, AddAssign, Div, Mul, Rem, Sub, SubAssign};
//use core::ops::{BitAnd, Shl, Shr};
/*
use core::num::ParseIntError;
*/
use core::fmt;
use num_traits::{Num, One, Zero};
use std::convert::{From};
use std::cmp::{PartialOrd};

// This is the trait required for the type used in implementing a particular
// Addr<T> struct. It applies both to the Addr<T> and to the T since we need
// to do the same things to both.
pub trait AddrTraits: Num + Copy +
    From<usize> + From<u128> + From<u64> + From<u32> +
    Into<*mut u8> +
    Add + AddAssign + Div + Mul + Rem + Sub + SubAssign +
//    BitAnd + Shl + Shr +
    Num + One + Zero +
    PartialOrd +
    fmt::LowerHex
    {
}

/*
pub struct AddrImpl<A>
where
    A: AddrTraits,
{
    a:  A,
}

impl<A> AddrImpl<A>
where
    A: AddrTraits
{
    pub fn new(a: A) -> AddrImpl<A> {
        AddrImpl::<A> {
            a:  a,
        }
    }
}
*/
