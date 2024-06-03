// Struct to allow arbitrary address info to be manipulated

extern crate num_traits;

use core::ops::{Add, AddAssign, Div, Mul, Rem, Shl, Shr, Sub, SubAssign};
use core::num::ParseIntError;
use num_traits::{Num, One, Zero};
use core::fmt;

pub trait AddrBase<A>: Num
where
    A: Num,
{
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Addr<A>
where
    A: Num,
 {
    a: A,
}

impl<A> Addr<A>
where
    A: Num,
{
    pub fn new<T: Num>(new_a: T) -> Addr<T> {
        Addr::<T> {
            a: new_a,
        }
    }
}

impl<A> AddrBase<A>
for Addr<A>
where
    A: Num,
{
}

/*
pub fn assign_num<T: Num>(x: T) -> Addr<T> {
    Addr::<T> {
        a: x,
    }
}
*/

impl<A> Num
for Addr<A>
where
    A: Num,
{
    type FromStrRadixErr = ParseIntError;
    fn from_str_radix(_: &str, _: u32)->Result<Self, <Self as Num>::FromStrRadixErr> {
        unimplemented!();
    }
}

impl<A: fmt::Display + fmt::Debug> fmt::Display
for Addr<A>
where
    A: Num,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.a)
    }
}

impl<A> fmt::LowerHex
for Addr<A>
where
    A: Num + fmt::LowerHex,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.a)
    }
}

impl<A: Zero> Zero
for Addr<A>
where
    A: Num,
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
    A: Num,
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

impl<A: Add<Output = A>> Add
for Addr<A>
where
    A: Num,
{ type Output = Addr<A>;
    fn add(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a + rhs.a,
        }
    }
}

impl<A: AddAssign> AddAssign<Addr<A>>
for Addr<A>
where
    A: Num,
{
    fn add_assign(&mut self, rhs: Self) {
        self.a += rhs.a; // Use the AddAssign trait on the inner value (A)
    }
}

impl<A: Sub<Output = A>> Sub
for Addr<A>
where
    A: Num,
{ type Output = Addr<A>;
    fn sub(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a - rhs.a,
        }
    }
}

impl<A: SubAssign> SubAssign<Addr<A>>
for Addr<A>
where
    A: Num,
{
    fn sub_assign(&mut self, rhs: Self) {
        self.a -= rhs.a; // Use the AddAssign trait on the inner value (A)
    }
}

impl<A: Mul<Output = A>> Mul
for Addr<A>
where
    A: Num,
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
    A: Num,
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
    A: Num,
{ type Output = Addr<A>;
    fn rem(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a % rhs.a,
        }
    }
}

impl<A: Shl<Output = A>> Shl
for Addr<A>
where
    A: Num,
{ type Output = Addr<A>;
    fn shl(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a << rhs.a,
        }
    }
}

impl<A: Shr<Output = A>> Shr
for Addr<A>
where
    A: Num,
{ type Output = Addr<A>;
    fn shr(self, rhs: Addr<A>) -> Self::Output {
        Addr {
            a: self.a >> rhs.a,
        }
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
    fn test_shr() {
        let x: PAddr = PAddr::new(0x8 | 0x2 | 0x1);
        let y: PAddr = PAddr::new(1);
        print!("x(0x{:x}) >> y({}) = ", x, y);
        let z = x >> y;
        println!("z(0x{:x})", z);
        assert_eq!(z, PAddr::new((0x8 >> 1) | (0x2 >> 1) | (0x1 >> 0)));

        let a = PAddr::new(0x5);
        let b = PAddr::new(1);
        println!("a ({}) << b ({}) = ", a, b);
        let c = a << b;
        print!("c ({})", c);
        assert_eq!(c, PAddr::new(0xa));
    }

    #[test]
    fn test_shl() {
        let x: PAddr = PAddr::new(0x8 | 0x2 | 0x1);
        let y: PAddr = PAddr::new(3);
        print!("x(0x{:x}) << y({}) = ", x, y);
        let z = x << y;
        println!("z(0x{:x})", z);
        assert_eq!(z, PAddr::new((0x8 << 3) | (0x2 << 3) | (0x1 << 3)));
    }
}
