// Define things for the struct Data<A>, which allows arbitrary addresses to be
// computed. To do so, Data<A> implements num_traits::Unsigned and it's possible to
// add two Data<A> values together. To do this, Data<A> has an element of type
// A which also implements Unsigned. This allows specifying an underlying integer
// type to use for performing the operations.
//
// FIXME: Make Data be ECCData?

use core::fmt;
use core::num::ParseIntError;
use core::ops::{Add, AddAssign, Div, Mul, Rem, Sub, SubAssign};
use core::ops::{BitAnd, Shl, Shr};
use num_traits::{Num, One, Unsigned, Zero};
//use std::convert::From;

// FIXME: rename ECCDataTrait?
pub trait DataImplTrait<D>:
    Unsigned + Copy + Into<*mut D> + fmt::Debug
{
}

// FIXME: remove allow(dead_code)
#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Data<D>(D)
where
    D: DataImplTrait<D>;

impl<D>
From<Data<D>>
for *mut D
where
    D: DataImplTrait<D>,
{
    fn from(value: Data<D>) -> *mut D {
        let p: *mut D;
        p = value.0.into();
        p
    }
}

impl<D> DataImplTrait<u32> for Data<D>
where
    D: DataImplTrait<D>,
    *mut u32: From<D>,
{
}

impl<D> fmt::Display for Data<D>
where
    D: DataImplTrait<D> + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<D> fmt::LowerHex for Data<D>
where
    D: DataImplTrait<D> + fmt::LowerHex,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

// From num_traits

impl<D> Unsigned for Data<D> where D: DataImplTrait<D> {}

impl<D> Num for Data<D>
where
    D: DataImplTrait<D>,
{
    type FromStrRadixErr = ParseIntError;
    fn from_str_radix(
        _: &str,
        _: u32,
    ) -> Result<Self, Self::FromStrRadixErr> {
        // FIXME: impl<D>ement this
        unimplemented!();
    }
}

impl<D> One for Data<D>
where
    D: DataImplTrait<D>,
{
    fn one() -> Self::Output {
        Data(D::one())
    }
}

impl<D> Zero for Data<D>
where
    D: DataImplTrait<D>,
{
    fn zero() -> Self::Output {
        Data(D::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

/*
impl<D> From<Data<usize>> for usize
where
    D: DataImplTrait<D>,
{
    fn from(value: Data<usize>) -> usize {
        value.0.into()
    }
}
*/

impl<D> From<usize> for Data<D>
where
    D: DataImplTrait<D> + From<usize>,
{
    fn from(value: usize) -> Self {
        value.into() // Convert usize to Data
    }
}

impl<D> From<u128> for Data<D>
where
    D: DataImplTrait<D> + From<u128>,
{
    fn from(value: u128) -> Self {
        value.into() // Convert u128 to Data
    }
}

impl<D> From<u64> for Data<D>
where
    D: DataImplTrait<D> + From<u64>,
{
    fn from(value: u64) -> Self {
        value.into() // Convert u64 to Data
    }
}

impl<D> From<u32> for Data<D>
where
    D: DataImplTrait<D> + From<u32>,
{
    fn from(value: u32) -> Self {
        value.into() // Convert u32 to Data
    }
}

/*
impl<D> From<usize>
for *mut usize
where
    D: DataImplTrait<D>,
{
}

impl<D> DataImplTrait<D><usize>
for usize
where
    D: DataImplTrait<D>,
{
}

impl<D> DataImplTrait<D><u128>
for u128
where
    D: DataImplTrait<D>,
{
}

impl<D> DataImplTrait<D><u64>
for u64
where
    D: DataImplTrait<D>,
{
}

impl<D> DataImplTrait<D><u32>
for u32
where
    D: DataImplTrait<D>,
{
}

impl<D> Into<*mut u32>
for u32
where
    D: DataImplTrait<D>,
{
    fn into(self) -> *mut u32 {
        self.into()
    }
}
*/

/*
impl<D><D> From<DataImplTrait<D><D>> for u32
where
    D: DataImplTrait<D>,
where
    D: Unsigned,
    usize: From<D>,
{
    fn from(value: Addr<D>) -> Self {
        value.addr.into()
    }
}
*/

// Arithmetic from core::ops

impl<D> Add for Data<D>
where
    D: DataImplTrait<D>,
{
    type Output = Data<D>;

    fn add(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 + rhs.0)
    }
}

impl<D> AddAssign for Data<D>
where
    D: DataImplTrait<D> + AddAssign,
{
    fn add_assign(&mut self, rhs: Data<D>) {
        (*self).0 += rhs.0
    }
}

impl<D> Div for Data<D>
where
    D: DataImplTrait<D>,
{
    type Output = Data<D>;

    fn div(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 / rhs.0)
    }
}

impl<D> Mul for Data<D>
where
    D: DataImplTrait<D>,
{
    type Output = Data<D>;

    fn mul(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 * rhs.0)
    }
}

impl<D> Rem for Data<D>
where
    D: DataImplTrait<D>,
{
    type Output = Data<D>;

    fn rem(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 / rhs.0)
    }
}

impl<D> Sub for Data<D>
where
    D: DataImplTrait<D>,
{
    type Output = Data<D>;

    fn sub(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 - rhs.0)
    }
}

impl<D> SubAssign for Data<D>
where
    D: DataImplTrait<D> + SubAssign,
{
    fn sub_assign(&mut self, rhs: Data<D>) {
        (*self).0 -= rhs.0
    }
}

// Bit operations from core::ops

impl<D> BitAnd for Data<D>
where
    D: DataImplTrait<D> + BitAnd<Output = D>,
{
    type Output = Data<D>;

    fn bitand(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 & rhs.0)
    }
}

impl<D> Shl for Data<D>
where
    D: DataImplTrait<D> + Shl<Output = D>,
{
    type Output = Data<D>;

    fn shl(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 << rhs.0)
    }
}

impl<D> Shr for Data<D>
where
    D: DataImplTrait<D> + Shr<Output = D>,
{
    type Output = Data<D>;

    fn shr(self, rhs: Data<D>) -> Self::Output {
        Data(self.0 >> rhs.0)
    }
}
