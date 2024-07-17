// Define things for the struct Addr<A>, which allows arbitrary addresses to be
// computed. To do so, Addr<A> implements num_traits::Unsigned and it's possible to
// add two Addr<A> values together. To do this, Addr<A> has an element of type
// A which also implements Unsigned. This allows specifying an underlying integer
// type to use for performing the operations.

use core::fmt;
use core::num::ParseIntError;
use core::ops::{Add, AddAssign, Div, Mul, Rem, Sub, SubAssign};
use core::ops::{BitAnd, Shl, Shr};
use num_traits::{Num, One, Unsigned, Zero};
use std::convert::From;

pub trait AddrImplTrait<A>:
    Unsigned
    + Copy
    + Clone
    + Shl<Output = A>
    + Shr<Output = A>
    + BitAnd<Output = A>
    + AddAssign
    + SubAssign
    + PartialOrd
    + From<Addr<A>>
    + From<usize>
    + fmt::Display
{
}

// Addr definitions

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Addr<A>(pub A);

impl<A> fmt::Display for Addr<A>
where
    A: Unsigned + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<A> fmt::LowerHex for Addr<A>
where
    A: Unsigned + fmt::LowerHex,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}

/*
impl<A, D> Into<*mut D> for Addr<A>
where
    *mut D: From<A>,
{
    fn into(self) -> *mut D {
        self.0.into()
    }
}
*/

impl<A> From<Addr<A>> for usize
where
    A: Unsigned,
    usize: From<A>,
{
    fn from(value: Addr<A>) -> Self {
        value.0.into()
    }
}

impl<A> From<usize> for Addr<A>
where
    A: Unsigned + From<usize>,
{
    fn from(value: usize) -> Self {
        Addr(value.into()) // Convert usize to Addr
    }
}

impl<A> From<u128> for Addr<A>
where
    A: Unsigned + From<u128>,
{
    fn from(value: u128) -> Self {
        Addr(value.into()) // Convert u128 to Addr
    }
}

impl<A> From<u64> for Addr<A>
where
    A: Unsigned + From<u64>,
{
    fn from(value: u64) -> Self {
        Addr(value.into()) // Convert u64 to Addr
    }
}

impl<A> From<u32> for Addr<A>
where
    A: Unsigned + From<u32>,
{
    fn from(value: u32) -> Self {
        Addr(value.into()) // Convert u32 to Addr
    }
}

impl<A, D> From<Addr<A>>
for *mut D
where
    A: Unsigned + From<Addr<A>>,
{
    fn from(value: Addr<A>) -> Self {
        value.into()
    }
}

/*
impl <A> From<A> for usize
where A: Unsigned + From<usize>
{
    fn from(value: A) -> Self {
        value.into()
    }
}

impl<A> Into<usize> for A
where
    A: Unsigned + Into<usize>,
{
    fn into(self) -> Self {
        self.into()
    }
}
*/

// From num_traits

impl<A> Unsigned for Addr<A> where A: Unsigned + Copy {}

impl<A> Num for Addr<A>
where
    A: Unsigned + Copy,
{
    type FromStrRadixErr = ParseIntError;
    fn from_str_radix(
        _: &str,
        _: u32,
    ) -> Result<Self, Self::FromStrRadixErr> {
        // FIXME: implement this
        unimplemented!();
    }
}

impl<A> One for Addr<A>
where
    A: Unsigned + Copy,
{
    fn one() -> Self::Output {
        Addr(A::one())
    }
}

impl<A> Zero for Addr<A>
where
    A: Unsigned + Copy,
{
    fn zero() -> Self::Output {
        Addr(A::zero())
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

// Arithmetic from core::ops

impl<A> Add for Addr<A>
where
    A: Unsigned + Add,
{
    type Output = Addr<A>;

    fn add(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 + rhs.0)
    }
}

impl<A> AddAssign for Addr<A>
where
    A: Unsigned + AddAssign + SubAssign,
{
    fn add_assign(&mut self, rhs: Addr<A>) {
        self.0 += rhs.0
    }
}

impl<A> Div for Addr<A>
where
    A: Unsigned + Div,
{
    type Output = Addr<A>;

    fn div(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 / rhs.0)
    }
}

impl<A> Mul for Addr<A>
where
    A: Unsigned + Mul,
{
    type Output = Addr<A>;

    fn mul(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 * rhs.0)
    }
}

impl<A> Rem for Addr<A>
where
    A: Unsigned + Copy,
{
    type Output = Addr<A>;

    fn rem(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 / rhs.0)
    }
}

impl<A> Sub for Addr<A>
where
    A: Unsigned + Sub,
{
    type Output = Addr<A>;

    fn sub(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 - rhs.0)
    }
}

impl<A> SubAssign for Addr<A>
where
    A: Unsigned + SubAssign,
{
    fn sub_assign(&mut self, rhs: Addr<A>) {
        self.0 -= rhs.0
    }
}

// Bit operations from core::ops

impl<A> BitAnd for Addr<A>
where
    A: Unsigned + Copy + BitAnd<Output = A>,
{
    type Output = Addr<A>;

    fn bitand(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 & rhs.0)
    }
}

impl<A> Shl for Addr<A>
where
    A: Unsigned + Shl<Output = A> + Shr,
{
    type Output = Addr<A>;

    fn shl(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 << rhs.0)
    }
}

impl<A> Shr for Addr<A>
where
    A: Unsigned + Shl + Shr<Output = A>,
{
    type Output = Addr<A>;

    fn shr(self, rhs: Addr<A>) -> Self::Output {
        Addr(self.0 >> rhs.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    type BaseVType = u64;
    type VAddr = Addr<BaseVType>;
    type BasePType = u128;
    type PAddr = Addr<BasePType>;

    #[test]
    fn test_create() {
        let _dummy: VAddr;
        let x: Addr<u128> = Addr::<u128>(0);
        println!("x = {:?}", x);
        let y: VAddr = Addr::<BaseVType>(1);
        println!("y = {:?}", y);
        let z: VAddr = Addr::<BaseVType>(2u64);
        println!("z = {:?}", z);
        let z: VAddr = Addr::<BaseVType>(2);
        println!("z = {:?}", z);
        let a: usize = 12;
        let b = Addr::<BaseVType>(a as BaseVType);
        println!("a {} b {}", a, b);
        let l: usize = 17;
        let l = l as BaseVType;
        let m = Addr::<BaseVType>(l);
        println!("l {} m {}", l, m);
    }

    #[test]
    fn test_assign() {
        let x: u32 = 1;
        let y = Addr::<BaseVType>(x as u64);
        println!("x {} y {}", x, y);
        assert_eq!(x as u64, y.0);
    }

    #[test]
    fn test_add() {
        let x: PAddr = Addr::<BasePType>(2);
        let y: PAddr = Addr::<BasePType>(4);
        print!("x({}) + y({}) = ", x, y);
        let z = x + y;
        println!("z({})", z);
        assert_eq!(z, Addr::<BasePType>(6));
    }

    #[test]
    fn test_sub() {
        let x: PAddr = Addr::<BasePType>(4);
        let y: PAddr = Addr::<BasePType>(2);
        print!("x({}) + y({}) = ", x, y);
        let z = x - y;
        println!("z({})", z);
        assert_eq!(z, Addr::<BasePType>(2));
    }

    #[test]
    fn test_mul() {
        let x: PAddr = Addr::<BasePType>(3);
        let y: PAddr = Addr::<BasePType>(7);
        print!("x({}) + y({}) = ", x, y);
        let z = x * y;
        println!("z({})", z);
        assert_eq!(z, Addr::<BasePType>(21));
    }

    #[test]
    fn test_div() {
        let x: PAddr = Addr::<BasePType>(17);
        let y: PAddr = Addr::<BasePType>(4);
        print!("x({}) + y({}) = ", x, y);
        let z = x / y;
        println!("z({})", z);
        assert_eq!(z, Addr::<BasePType>(4));
    }

    #[test]
    fn test_add_assign() {
        let mut x: PAddr = Addr::<BasePType>(17);
        let y: PAddr = Addr::<BasePType>(4);
        print!("x({}) + y({}) = ", x, y);
        x += y;
        println!("x({})", x);
        assert_eq!(x, Addr::<BasePType>(21));
    }

    #[test]
    fn test_sub_assign() {
        let mut x: PAddr = Addr::<BasePType>(17);
        let y: PAddr = Addr::<BasePType>(4);
        print!("x({}) + y({}) = ", x, y);
        x -= y;
        println!("x({})", x);
        assert_eq!(x, Addr::<BasePType>(13));
    }

    #[test]
    fn test_shl() {
        let x: PAddr = Addr::<BasePType>(0x8 | 0x2 | 0x1);
        let y: PAddr = Addr::<BasePType>(3);
        print!("x(0x{:x}) << y({}) = ", x, y);
        let z = x << y;
        println!("z(0x{:x})", z);
        assert_eq!(
            z,
            Addr::<BasePType>((0x8 << 3) | (0x2 << 3) | (0x1 << 3))
        );
    }

    #[test]
    fn test_shr() {
        let x: PAddr = Addr::<BasePType>(0x8 | 0x2 | 0x1);
        let y: PAddr = Addr::<BasePType>(1);
        print!("x(0x{:x}) >> y(0x{}) = ", x, y);
        let z = x >> y;
        println!("z(0x{:x})", z);
        assert_eq!(
            z,
            Addr::<BasePType>((0x8 >> 1) | (0x2 >> 1) | (0x1 >> 0))
        );

        let a = Addr::<BasePType>(0x5);
        let b = Addr::<BasePType>(1);
        print!("a (0x{:x}) >> b (0x{:x}) = ", a, b);
        let c = a << b;
        println!("c (0x{:x})", c);
        assert_eq!(c, Addr::<BasePType>(0xa));

        let l = Addr::<BasePType>(2);
        let m = Addr::<BasePType>(1);
        print!("l (0x{:x}) >> m (0x{:x}) = ", l, m);
        let n = l >> m;
        println!("n (0x{:x})", n);
    }

    #[test]
    fn test_underlying_num() {
        type XAddr = Addr<u64>;
        type BaseVType = u64;
        type YAddr = Addr<BaseVType>;

        #[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
        struct MemArea<A>
        where
            A: Unsigned + Copy + Shl + Shr,
        {
            s: Addr<A>,
            e: Addr<A>,
        }

        impl<A> MemArea<A>
        where
            A: Unsigned + Copy + Shl + Shr,
        {
            pub fn new(start: Addr<A>, end: Addr<A>) -> MemArea<A> {
                MemArea::<A> { s: start, e: end }
            }
        }

        let l: usize = 17;
        let m = Addr::<BaseVType>(l as BaseVType);
        println!("l {} m {}", l, m);

        let x0 = MemArea::<u64>::new(Addr(24), Addr(71));
        let x1: usize = 21;
        let x2 = Addr::<BaseVType>(x1 as BaseVType);
        let x3: XAddr = x2;
        print!("x0.s ({:x}) >> x3 ({:x}) = ", x0.s, x3);
        let x4 = x0.s >> x3;
        println!("x4 ({:x})", x4);
    }
}
