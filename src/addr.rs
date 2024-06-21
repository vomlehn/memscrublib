// Define things for the struct Addr<A>, which allows arbitrary addresses to be
// computed. To do so, Addr<A> implements num_traits::Unsigned and it's possible to
// add two Addr<A> values together. To do this, Addr<A> has an element of type
// A which also implements Unsigned. This allows specifying an underlying integer
// type to use for performing the operations.

extern crate num_traits;

use core::ops::{Add, AddAssign, Div, Mul, Rem, Sub, SubAssign};
use core::ops::{BitAnd, Shl, Shr};
use core::num::ParseIntError;
use core::fmt;
use num_traits::{Unsigned, One, Zero};

// Type used for implementing generic address operations. It would be possible
// for the implementation type to always be usize if we were always working
// on virtual addresses since those should be no larger than usize. However, we
// may have a physical memory larger than virtual memory and want to be able
// to scrub that as well. Thus, we use a generic type for the address
// implementation type.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    a: A,
}

impl<A> Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    pub fn new(new_a: A) -> Self {
        Addr::<A> {
            a: new_a,
        }
    }

    // Rest of the existing implementation for Addr<A> methods...
    pub fn xxx(new_a: A) -> Self {
        let x = Addr::<A> { a: new_a, };
        let y = Addr::<A> { a: new_a, };
        let z = x - y;
        let z = x >> y;

        let a = Addr::<u128>::new(16);
        let b = Addr::<u128>::new(1);
        let c = a >> b;

        let z = a >> x;
        z
    }
}

// Type conversion functions

impl<A> From<usize> for Addr<A>
where
    A: Unsigned + From<usize> + Copy + Shl + Shr,
{
    fn from(value: usize) -> Self {
        Addr { a: value.into() } // Convert usize to A
    }
}

impl<A> From<u128> for
Addr<A>
where
    A: Unsigned + From<u128> + Copy + Shl + Shr,
{
    fn from(value: u128) -> Self {
        Addr { a: value.into() } // Convert usize to A
    }
}

impl<A> From<u64> for Addr<A>
where
    A: Unsigned + From<u64> + Copy + Shl + Shr,
{
    fn from(value: u64) -> Self {
        Addr { a: value.into() } // Convert usize to A
    }
}

impl<A> From<u32> for Addr<A>
where
    A: Unsigned + From<u32> + Copy + Shl + Shr,
{
    fn from(value: u32) -> Self {
        Addr { a: value.into() } // Convert usize to A
    }
}

// FIXME: If <A> is a physical address, we can implement mapping for the physical
// address to the virtual address here, though I'm not really sure this is the
// right place to do this.
impl <A> From<Addr<A>>
for *mut u8
where
    A: Unsigned + From<Addr<A>> + Copy + Shl + Shr,
    *mut u8: From<A>,
{
    fn from(value: Addr<A>) -> Self {
        value.a.into()
//        value.a as *mut u8
    }
}

/*
impl <A> Into<*mut u8>
for Addr<A>
where
    A: Unsigned + From<*mut u8> + Copy,
    *mut u8: From<A>,
{
    fn into(self) -> *mut u8 {
        self.a.into()
    }
}
*/

// Input and output functions

/*
impl<A> Unsigned
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    type FromStrRadixErr = ParseIntError;
    fn from_str_radix(_: &str, _: u32)->Result<Self, <Self as Unsigned>::FromStrRadixErr> {
        unimplemented!();
    }
}
*/

impl<A: Shl + Shr + fmt::Display + fmt::Debug> fmt::Display
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.a)
    }
}

impl<A> fmt::LowerHex
for Addr<A>
where
    A: Unsigned + fmt::LowerHex + Copy + Shl + Shr,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.a)
    }
}

// Constants

impl<A: Zero + Shl + Shr> Zero
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    fn zero() -> Self {
        Addr::<A> {
            a: A::zero(),
        }
    }
    fn is_zero(&self) -> bool {
        self.a == A::zero()
    }
}

impl<A: One<Output = A>> One
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    fn one() -> Addr<A> {
        Addr::<A> {
            a: A::one(),
        }
    }
    fn is_one(&self) -> bool {
        self.a == A::one()
    }
}

// Mathematical operations

impl<A> Add
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr + Add<Output = A>,
{
    type Output = Addr<A>;
    fn add(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a + rhs.a,
        }
    }
}

impl<A> AddAssign<Addr<A>>
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr + AddAssign,
{
    fn add_assign(&mut self, rhs: Self) {
        self.a += rhs.a; // Use the AddAssign trait on the inner value (A)
    }
}

impl<A> Sub
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr + Sub<Output = A>,
{
    type Output = Addr<A>;
    fn sub(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a - rhs.a,
        }
    }
}

impl<A: SubAssign> SubAssign<Addr<A>>
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{
    fn sub_assign(&mut self, rhs: Self) {
        self.a -= rhs.a; // Use the AddAssign trait on the inner value (A)
    }
}

impl<A: Mul<Output = A>> Mul
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{ type Output = Addr<A>;
    fn mul(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a * rhs.a,
        }
    }
}

impl<A: Div<Output = A>> Div
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{ type Output = Addr<A>;
    fn div(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a / rhs.a,
        }
    }
}

impl<A: Rem<Output = A>> Rem
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{ type Output = Addr<A>;
    fn rem(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a % rhs.a,
        }
    }
}

// Bitwise operations
impl<A: BitAnd<Output = A> + Shl + Shr> BitAnd
for Addr<A>
where
    A: Unsigned + Copy + Shl + Shr,
{ type Output = Addr<A>;
    fn bitand(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a & rhs.a,
        }
    }
}

impl<A> Shl
for Addr<A>
where
    A: Unsigned + Copy + Shr + Shl<Output = A>,
{ type Output = Addr<A>;
    fn shl(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a << rhs.a,
        }
    }
}

impl<A> Shr
for Addr<A>
where
    A: Unsigned + Copy + Shr<Output = A> + Shl,
{
    type Output = Addr<A>;
    fn shr(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a + rhs.a,
        }
    }
}

/*
impl<A> Shr
for Addr<A>
where
    A: Unsigned + Copy + Shr<Output = A> + Shl,
{
    type Output = Addr<A>;

    fn shr(self, rhs: Self) -> Self::Output {
        Addr {
            a: self.a >> rhs.a,
        }
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    type BaseVType = u64;
    type VAddr = Addr<BaseVType>;
    type BasePType = u128;
    type PAddr = Addr<BasePType>;

    #[test]
    fn test_create() {
        let x: Addr<u128> = Addr {a: 0};
        println!("x = {:?}", x);
        let y: VAddr = VAddr {a: 1};
        println!("y = {:?}", y);
        let z: VAddr = VAddr::new(2);
        println!("z = {:?}", z);
        let a: usize = 12;
        let b = VAddr::new(a as BaseVType);
        println!("a {} b {}", a, b);
        let l: usize = 17;
        let l = l as BaseVType;
        let m = VAddr::new(l);
        println!("l {} m {}", l, m);
    }

    #[test]
    fn test_assign() {
        let x: u32 = 1;
        let y = VAddr::new(x as u64);
        println!("x {} y {}", x, y);
        assert_eq!(x as u64, y.a);
    }

    #[test]
    fn test_add() {
        let x: PAddr = PAddr::new(2);
        let y: PAddr = PAddr::new(4);
        print!("x({}) + y({}) = ", x, y);
        let z = x + y;
        println!("z({})", z);
        assert_eq!(z, PAddr::new(6));
    }

    #[test]
    fn test_sub() {
        let x: PAddr = PAddr::new(4);
        let y: PAddr = PAddr::new(2);
        print!("x({}) + y({}) = ", x, y);
        let z = x - y;
        println!("z({})", z);
        assert_eq!(z, PAddr::new(2));
    }

    #[test]
    fn test_mul() {
        let x: PAddr = PAddr::new(3);
        let y: PAddr = PAddr::new(7);
        print!("x({}) + y({}) = ", x, y);
        let z = x * y;
        println!("z({})", z);
        assert_eq!(z, PAddr::new(21));
    }

    #[test]
    fn test_div() {
        let x: PAddr = PAddr::new(17);
        let y: PAddr = PAddr::new(4);
        print!("x({}) + y({}) = ", x, y);
        let z = x / y;
        println!("z({})", z);
        assert_eq!(z, PAddr::new(4));
    }

    #[test]
    fn test_add_assign() {
        let mut x: PAddr = PAddr::new(17);
        let y: PAddr = PAddr::new(4);
        print!("x({}) + y({}) = ", x, y);
        x += y;
        println!("x({})", x);
        assert_eq!(x, PAddr::new(21));
    }

    #[test]
    fn test_sub_assign() {
        let mut x: PAddr = PAddr::new(17);
        let y: PAddr = PAddr::new(4);
        print!("x({}) + y({}) = ", x, y);
        x -= y;
        println!("x({})", x);
        assert_eq!(x, PAddr::new(13));
    }

    #[test]
    fn test_shl() {
        let x: PAddr = Addr::<BasePType>::new(0x8 | 0x2 | 0x1);
        let y: PAddr = PAddr::new(3);
        print!("x(0x{:x}) << y({}) = ", x, y);
        let z = x << y;
        println!("z(0x{:x})", z);
        assert_eq!(z, PAddr::new((0x8 << 3) | (0x2 << 3) | (0x1 << 3)));
    }

    #[test]
    fn test_shr() {
        let x: PAddr = PAddr::new(0x8 | 0x2 | 0x1);
        let y: PAddr = PAddr::new(1);
        print!("x(0x{:x}) >> y(0x{}) = ", x, y);
        let z = x >> y;
        println!("z(0x{:x})", z);
        assert_eq!(z, PAddr::new((0x8 >> 1) | (0x2 >> 1) | (0x1 >> 0)));

        let a = PAddr::new(0x5);
        let b = PAddr::new(1);
        print!("a (0x{:x}) >> b (0x{:x}) = ", a, b);
        let c = a << b;
        println!("c (0x{:x})", c);
        assert_eq!(c, PAddr::new(0xa));

        let l = PAddr::new(2);
        let m = PAddr::new(1);
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
            pub fn new(start: Addr::<A>, end: Addr::<A>) -> MemArea<A> {
                MemArea::<A> {
                    s: start,
                    e: end,
                }
            }
        }

        let l: usize = 17;
        let m = YAddr::new(l as BaseVType);
        println!("l {} m {}", l, m);

        let x0 = MemArea::<u64>::new(XAddr::new(24), XAddr::new(71));
        let x1: usize = 21;
        let x2 = Addr::new(x1 as BaseVType);
        let x3: XAddr = x2;
        print!("x0.s ({:x}) >> x3 ({:x}) = ", x0.s, x3);
        let x4 = x0.s >> x3;
        println!("x4 ({:x})", x4);
        
    }
}
