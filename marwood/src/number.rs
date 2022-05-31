use num::bigint::BigInt;
use num::traits::FloatConst;
use num::{
    BigRational, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Rational64, Signed,
};
use num::{Num, Rational32, ToPrimitive};
use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Binary, Formatter, LowerHex, Octal};
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Rem, Sub};
use std::rc::Rc;

/// Exactness
///
/// Exactness represents preferred exactness when parsing
/// a textual representation of a number.
///
/// * Exact - Exactness is preferred in cases where the result
///   would have been inexact. This usually means trying to
///   convert a float to its rational counterpart.
///
/// * Inexact - Inexactness is preferred in cases where
///   an exact form is given, even if the exact form was an
///   integer.
///
/// * Unspecified - Exactness is not specified. Prefer to
///   keep inexact values inexact, and exact exact.
#[derive(Debug)]
pub enum Exactness {
    Exact,
    Inexact,
    Unspecified,
}

/// Number
///
/// Number implements scheme's numerical tower, converting
/// between numerical types transparently.
///
/// It follows a set of loose rules if possible:
///
/// * Exactness if preferred
/// * Inexactness is infectious
///
/// Rationals are provided by the Rational32 type, allowing
/// for a rational composed of a 32 bit numerator and denominator.
/// If a rational exceeds these limits, the resulting operation
/// falls back to "inexact" (float).
#[derive(Clone, Debug)]
pub enum Number {
    Fixnum(i64),
    Float(f64),
    BigInt(Rc<BigInt>),
    Rational(Rational32),
}

impl Number {
    pub fn new_bigint<T: Into<BigInt>>(num: T) -> Number {
        Number::BigInt(Rc::new(num.into()))
    }

    /// Parse
    ///
    /// Parse the text given the desired exactness and radix.
    pub fn parse_with_exactness(text: &str, exactness: Exactness, radix: u32) -> Option<Number> {
        match exactness {
            Exactness::Unspecified => Self::parse(text, radix),
            Exactness::Exact => Self::parse(text, radix).map(|num| match num.to_exact() {
                Some(num) => num,
                None => num,
            }),
            Exactness::Inexact => Self::parse(text, radix)
                .map(|num| num.to_inexact())
                .map(|it| it.unwrap()),
        }
    }

    pub fn parse(text: &str, radix: u32) -> Option<Number> {
        if let Ok(num) = i64::from_str_radix(text, radix) {
            Some(Number::from(num))
        } else if let Ok(num) = BigInt::from_str_radix(text, radix) {
            Some(Number::from(num))
        } else if let Some(num) = Self::parse_rational(text, radix) {
            Some(num)
        } else if let Ok(num) = f64::from_str_radix(text, radix) {
            Some(Number::from(num))
        } else {
            None
        }
    }

    /// Parse Rational
    ///
    /// Parse the text according to the given radix. If a 32 bit rational
    /// cannot be constructed, then attempt to construct a BigRational,
    /// converting the resulting BigRational into the most appropriate
    /// Number type.
    pub fn parse_rational(text: &str, radix: u32) -> Option<Number> {
        match Rational32::from_str_radix(text, radix) {
            Ok(num) => {
                if num.is_integer() {
                    Some(Number::from(num.to_i64().unwrap()))
                } else {
                    Some(num.into())
                }
            }
            Err(_) => match BigRational::from_str_radix(text, radix) {
                Ok(num) => {
                    if num.is_integer() {
                        match num.to_i64() {
                            Some(num) => Some(num.into()),
                            None => Some(num.to_integer().into()),
                        }
                    } else {
                        Some(num.to_f64().unwrap_or(f64::NAN).into())
                    }
                }
                Err(_) => None,
            },
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
        match self {
            Number::Fixnum(num) if *num >= 0 => num.to_usize(),
            Number::BigInt(num)
                if **num >= BigInt::from(0) && **num <= BigInt::from(usize::MAX) =>
            {
                Some(num.to_usize().unwrap())
            }
            Number::Rational(num) if num.is_integer() => num.to_usize(),
            _ => None,
        }
    }

    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Number::Fixnum(num) => Some(*num),
            Number::BigInt(num) => num.to_i64(),
            Number::Rational(num) if num.is_integer() => num.to_i64(),
            Number::Float(num) if self.is_integer() => num.to_i64(),
            _ => None,
        }
    }

    pub fn to_u64(&self) -> Option<u64> {
        match self {
            Number::Fixnum(num) if *num >= 0 => Some(*num as u64),
            Number::BigInt(num) if **num >= BigInt::from(0) && **num <= BigInt::from(u64::MAX) => {
                Some(num.to_u64().unwrap())
            }
            Number::Rational(num) if num.is_integer() => num.to_u64(),
            Number::Float(num) if self.is_integer() => num.to_u64(),
            _ => None,
        }
    }

    pub fn to_u32(&self) -> Option<u32> {
        match self {
            Number::Fixnum(num) => num.to_u32(),
            Number::BigInt(num) if **num >= BigInt::from(0) && **num <= BigInt::from(u32::MAX) => {
                Some(num.to_u32().unwrap())
            }
            Number::Rational(num) if num.is_integer() => num.to_u32(),
            Number::Float(num) if self.is_integer() => num.to_u32(),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            Number::Fixnum(num) => num.to_f64(),
            Number::BigInt(num) => num.to_f64(),
            Number::Rational(num) => num.to_f64(),
            Number::Float(num) => Some(*num),
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Number::Fixnum(_) => true,
            Number::Float(num) => num.floor() == *num,
            Number::BigInt(_) => true,
            Number::Rational(num) => num.is_integer(),
        }
    }

    pub fn is_complex(&self) -> bool {
        true
    }

    pub fn is_real(&self) -> bool {
        true
    }

    pub fn is_rational(&self) -> bool {
        match self {
            Number::Fixnum(_) => true,
            Number::Float(_) => false,
            Number::BigInt(_) => true,
            Number::Rational(_) => true,
        }
    }

    pub fn is_zero(&self) -> bool {
        self == &Number::from(0)
    }

    pub fn to_inexact(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => Some((*num as f64).into()),
            Number::Float(num) => Some((*num).into()),
            Number::BigInt(num) => Some(num.to_f64().unwrap().into()),
            Number::Rational(num) => Some(num.to_f64().unwrap().into()),
        }
    }

    pub fn to_exact(&self) -> Option<Number> {
        match self {
            Number::Float(num) => {
                if self.is_integer() {
                    match num.to_i64() {
                        Some(integer) => Some(Number::Fixnum(integer)),
                        None => num.to_i128().map(|num| BigInt::from(num).into()),
                    }
                } else {
                    match Rational32::from_f64(*num) {
                        Some(num) => Some(num.into()),
                        None => Some((*num).into()),
                    }
                }
            }
            Number::BigInt(_) | Number::Rational(_) | Number::Fixnum(_) => Some(self.clone()),
        }
    }

    pub fn numerator(&self) -> Number {
        match self {
            Number::Fixnum(_) => self.clone(),
            Number::Float(num) => match BigRational::from_f64(*num) {
                Some(rational) => Number::from(rational.numer().clone()),
                None => self.clone(),
            },
            Number::BigInt(_) => self.clone(),
            Number::Rational(num) => (*num.numer() as i64).into(),
        }
    }

    pub fn denominator(&self) -> Number {
        match self {
            Number::Fixnum(_) => 1.into(),
            Number::Float(num) => match BigRational::from_f64(*num) {
                Some(rational) => Number::from(rational.denom().clone()),
                None => self.clone(),
            },
            Number::BigInt(_) => 1.into(),
            Number::Rational(num) => (*num.denom() as i64).into(),
        }
    }

    pub fn abs(&self) -> Number {
        match self {
            Number::Fixnum(num) => num.unsigned_abs().into(),
            Number::Float(num) => num.abs().into(),
            Number::BigInt(num) => num.abs().into(),
            Number::Rational(num) => num.abs().into(),
        }
    }

    pub fn modulo(&self, rhs: &Number) -> Option<Number> {
        match self % rhs {
            Some(num) => &(&num + rhs) % rhs,
            None => None,
        }
    }

    pub fn round(&self) -> Number {
        match self {
            Number::Fixnum(_) => self.clone(),
            Number::Float(num) => num.round().into(),
            Number::BigInt(_) => self.clone(),
            Number::Rational(num) => num.round().into(),
        }
    }

    pub fn floor(&self) -> Number {
        match self {
            Number::Fixnum(_) => self.clone(),
            Number::Float(num) => num.floor().into(),
            Number::BigInt(_) => self.clone(),
            Number::Rational(num) => num.floor().into(),
        }
    }

    pub fn ceil(&self) -> Number {
        match self {
            Number::Fixnum(_) => self.clone(),
            Number::Float(num) => num.ceil().into(),
            Number::BigInt(_) => self.clone(),
            Number::Rational(num) => num.ceil().into(),
        }
    }

    pub fn truncate(&self) -> Number {
        match self {
            Number::Fixnum(_) => self.clone(),
            Number::Float(num) => num.trunc().into(),
            Number::BigInt(_) => self.clone(),
            Number::Rational(num) => num.trunc().into(),
        }
    }

    pub fn pow(&self, exp: u32) -> Number {
        match self {
            Number::Fixnum(num) => match num.checked_pow(exp) {
                Some(num) => num.into(),
                None => BigInt::from(*num).pow(exp).into(),
            },
            Number::Float(num) => num.powf(exp as f64).into(),
            Number::BigInt(lhs) => lhs.pow(exp).into(),
            Number::Rational(num) => {
                if exp.to_i32().is_some() {
                    num.pow(exp as i32).into()
                } else {
                    num.to_f64().unwrap_or(f64::NAN).powf(exp as f64).into()
                }
            }
        }
    }
}

impl Eq for Number {}
impl PartialEq for Number {
    fn eq(&self, rhs: &Self) -> bool {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs == rhs,
                Number::BigInt(rhs) => BigInt::from(*lhs) == **rhs,
                Number::Float(rhs) => *lhs as f64 == *rhs,
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        Rational32::from_integer(*lhs as i32) == *rhs
                    } else {
                        false
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => **lhs == BigInt::from(*rhs),
                Number::BigInt(rhs) => lhs == rhs,
                Number::Float(rhs) => lhs.to_f64().unwrap() == *rhs,
                Number::Rational(rhs) => match lhs.to_i32() {
                    Some(lhs) => Rational32::from_integer(lhs) == *rhs,
                    None => false,
                },
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => *lhs == *rhs as f64,
                Number::Float(rhs) => lhs == rhs,
                Number::BigInt(rhs) => *lhs == rhs.to_f64().unwrap(),
                Number::Rational(rhs) => match rhs.to_f64() {
                    Some(rhs) => *lhs == rhs,
                    None => false,
                },
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        Rational32::from_integer(*rhs as i32) == *lhs
                    } else {
                        false
                    }
                }
                Number::Float(rhs) => match lhs.to_f64() {
                    Some(lhs) => lhs == *rhs,
                    None => false,
                },
                Number::BigInt(rhs) => match rhs.to_i32() {
                    Some(rhs) => *lhs == Rational32::from_integer(rhs),
                    None => false,
                },
                Number::Rational(rhs) => lhs == rhs,
            },
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs.partial_cmp(rhs),
                Number::BigInt(rhs) => BigInt::from(*lhs).partial_cmp(&**rhs),
                Number::Float(rhs) => (*lhs as f64).partial_cmp(rhs),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        Rational32::from_integer(*lhs as i32).partial_cmp(rhs)
                    } else {
                        Some(Ordering::Greater)
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (**lhs).partial_cmp(&BigInt::from(*rhs)),
                Number::BigInt(rhs) => (**lhs).partial_cmp(&**rhs),
                Number::Float(rhs) => (**lhs).to_f64().unwrap().partial_cmp(rhs),
                Number::Rational(rhs) => match lhs.to_i32() {
                    Some(lhs) => Rational32::from_integer(lhs).partial_cmp(rhs),
                    None => Some(Ordering::Greater),
                },
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Number::Float(rhs) => lhs.partial_cmp(rhs),
                Number::BigInt(rhs) => lhs.partial_cmp(&(**rhs).to_f64().unwrap()),
                Number::Rational(rhs) => lhs.partial_cmp(&rhs.to_f64().unwrap()),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        lhs.partial_cmp(&Rational32::from_integer(*rhs as i32))
                    } else {
                        Some(Ordering::Less)
                    }
                }
                Number::Float(rhs) => lhs.to_f64().unwrap().partial_cmp(rhs),
                Number::BigInt(rhs) => match rhs.to_i32() {
                    Some(rhs) => lhs.partial_cmp(&Rational32::from_integer(rhs)),
                    None => Some(Ordering::Less),
                },
                Number::Rational(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

impl AddAssign for Number {
    fn add_assign(&mut self, rhs: Self) {
        let result = &*self + &rhs;
        *self = result;
    }
}

impl MulAssign for Number {
    fn mul_assign(&mut self, rhs: Self) {
        let result = &*self * &rhs;
        *self = result;
    }
}

impl DivAssign for Number {
    fn div_assign(&mut self, rhs: Self) {
        let result = &*self / &rhs;
        *self = result;
    }
}

impl Add for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        (&self).add(&rhs)
    }
}

impl Add for &Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => match lhs.checked_add(rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) + rhs).into(),
                },
                Number::BigInt(rhs) => (&**rhs + lhs).into(),
                Number::Float(rhs) => (*lhs as f64 + rhs).into(),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        let lhs_rational = Rational32::from_integer(*lhs as i32);
                        match lhs_rational.checked_add(rhs) {
                            Some(num) => num.into(),
                            None => (*lhs as f64 + rhs.to_f64().unwrap_or(f64::NAN)).into(),
                        }
                    } else {
                        (*lhs as f64 + rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs + rhs).into(),
                Number::BigInt(rhs) => (&**lhs + &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() + *rhs).into(),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        (&**lhs + rhs.to_integer()).into()
                    } else {
                        (lhs.to_f64().unwrap() + rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs + *rhs as f64).into(),
                Number::Float(rhs) => (*lhs + *rhs).into(),
                Number::BigInt(rhs) => (*lhs + rhs.to_f64().unwrap()).into(),
                Number::Rational(rhs) => (*lhs + rhs.to_f64().unwrap_or(f64::NAN)).into(),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        let rhs_rational = Rational32::from_integer(*rhs as i32);
                        match rhs_rational.checked_add(lhs) {
                            Some(num) => num.into(),
                            None => (lhs.to_f64().unwrap_or(f64::NAN) + *rhs as f64).into(),
                        }
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) + *rhs as f64).into()
                    }
                }
                Number::Float(rhs) => (lhs.to_f64().unwrap_or(f64::NAN) + *rhs).into(),
                Number::BigInt(rhs) => {
                    if lhs.is_integer() {
                        (&**rhs + lhs.to_integer()).into()
                    } else {
                        (rhs.to_f64().unwrap() + lhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
                Number::Rational(rhs) => match lhs.checked_add(rhs) {
                    Some(num) => num.into(),
                    None => {
                        (lhs.to_f64().unwrap_or(f64::NAN) + rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                },
            },
        }
    }
}

impl Mul for Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        (&self).mul(&rhs)
    }
}

impl Mul for &Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => match lhs.checked_mul(rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) * rhs).into(),
                },
                Number::BigInt(rhs) => (&**rhs * lhs).into(),
                Number::Float(rhs) => (*lhs as f64 * rhs).into(),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        let lhs_rational = Rational32::from_integer(*lhs as i32);
                        match lhs_rational.checked_mul(rhs) {
                            Some(num) => num.into(),
                            None => (*lhs as f64 * rhs.to_f64().unwrap_or(f64::NAN)).into(),
                        }
                    } else {
                        (*lhs as f64 * rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs * rhs).into(),
                Number::BigInt(rhs) => (&**lhs * &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() * *rhs).into(),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        (&**lhs * rhs.to_integer()).into()
                    } else {
                        (lhs.to_f64().unwrap() * rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs * *rhs as f64).into(),
                Number::Float(rhs) => (*lhs * *rhs).into(),
                Number::BigInt(rhs) => (*lhs * rhs.to_f64().unwrap()).into(),
                Number::Rational(rhs) => (*lhs * rhs.to_f64().unwrap_or(f64::NAN)).into(),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        let rhs_rational = Rational32::from_integer(*rhs as i32);
                        match rhs_rational.checked_mul(lhs) {
                            Some(num) => num.into(),
                            None => (lhs.to_f64().unwrap_or(f64::NAN) * *rhs as f64).into(),
                        }
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) * *rhs as f64).into()
                    }
                }
                Number::Float(rhs) => (lhs.to_f64().unwrap_or(f64::NAN) * *rhs).into(),
                Number::BigInt(rhs) => {
                    if lhs.is_integer() {
                        (&**rhs * lhs.to_integer()).into()
                    } else {
                        (rhs.to_f64().unwrap() * lhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
                Number::Rational(rhs) => match lhs.checked_mul(rhs) {
                    Some(num) => num.into(),
                    None => {
                        (lhs.to_f64().unwrap_or(f64::NAN) * rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                },
            },
        }
    }
}

impl Sub for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        (&self).sub(&rhs)
    }
}

impl Sub for &Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => match lhs.checked_sub(rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) - rhs).into(),
                },
                Number::BigInt(rhs) => (lhs - &**rhs).into(),
                Number::Float(rhs) => (*lhs as f64 - rhs).into(),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        let lhs_rational = Rational32::from_integer(*lhs as i32);
                        match lhs_rational.checked_sub(rhs) {
                            Some(num) => num.into(),
                            None => (*lhs as f64 - rhs.to_f64().unwrap_or(f64::NAN)).into(),
                        }
                    } else {
                        (*lhs as f64 - rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs - rhs).into(),
                Number::BigInt(rhs) => (&**lhs - &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() - *rhs).into(),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        (&**lhs - rhs.to_integer()).into()
                    } else {
                        (lhs.to_f64().unwrap() - rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs - *rhs as f64).into(),
                Number::Float(rhs) => (*lhs - *rhs).into(),
                Number::BigInt(rhs) => (*lhs - rhs.to_f64().unwrap()).into(),
                Number::Rational(rhs) => (*lhs - rhs.to_f64().unwrap_or(f64::NAN)).into(),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        let rhs_rational = Rational32::from_integer(*rhs as i32);
                        match lhs.checked_sub(&rhs_rational) {
                            Some(num) => num.into(),
                            None => (lhs.to_f64().unwrap_or(f64::NAN) - *rhs as f64).into(),
                        }
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) - *rhs as f64).into()
                    }
                }
                Number::Float(rhs) => (lhs.to_f64().unwrap_or(f64::NAN) - *rhs).into(),
                Number::BigInt(rhs) => {
                    if lhs.is_integer() {
                        (BigInt::from(lhs.to_integer()) - &**rhs).into()
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) - rhs.to_f64().unwrap()).into()
                    }
                }
                Number::Rational(rhs) => match lhs.checked_sub(rhs) {
                    Some(num) => num.into(),
                    None => {
                        (lhs.to_f64().unwrap_or(f64::NAN) - rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                },
            },
        }
    }
}

impl Div for Number {
    type Output = Number;
    fn div(self, rhs: Self) -> Self::Output {
        (&self).div(&rhs)
    }
}

impl Div for &Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if lhs.to_i32().is_some() && rhs.to_i32().is_some() {
                        Rational32::new(*lhs as i32, *rhs as i32).into()
                    } else {
                        (*lhs as f64 / *rhs as f64).into()
                    }
                }
                Number::BigInt(rhs) => {
                    if lhs.to_i32().is_some() && rhs.to_i32().is_some() {
                        Rational32::new(*lhs as i32, rhs.to_i32().unwrap()).into()
                    } else {
                        (*lhs as f64 / rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
                Number::Float(rhs) => (*lhs as f64 / rhs).into(),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        match Rational32::from_integer(*lhs as i32).checked_div(rhs) {
                            Some(num) => num.into(),
                            None => (*lhs as f64 / rhs.to_f64().unwrap_or(f64::NAN)).into(),
                        }
                    } else {
                        (*lhs as f64 / rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if lhs.to_i32().is_some() && rhs.to_i32().is_some() {
                        (Rational32::new(lhs.to_i32().unwrap(), *rhs as i32)).into()
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) / *rhs as f64).into()
                    }
                }
                Number::BigInt(rhs) => {
                    if lhs.to_i32().is_some() && rhs.to_i32().is_some() {
                        (Rational32::new(lhs.to_i32().unwrap(), rhs.to_i32().unwrap())).into()
                    } else {
                        (lhs.to_f64().unwrap_or(f64::NAN) / rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
                Number::Float(rhs) => (lhs.to_f64().unwrap() / *rhs).into(),
                Number::Rational(rhs) => {
                    if lhs.to_i32().is_some() {
                        match Rational32::from_integer(lhs.to_i32().unwrap()).checked_div(rhs) {
                            Some(num) => num.into(),
                            None => {
                                (lhs.to_f64().unwrap() / rhs.to_f64().unwrap_or(f64::NAN)).into()
                            }
                        }
                    } else {
                        (lhs.to_f64().unwrap() / rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs / *rhs as f64).into(),
                Number::Float(rhs) => (*lhs / *rhs).into(),
                Number::BigInt(rhs) => (*lhs / rhs.to_f64().unwrap()).into(),
                Number::Rational(rhs) => (lhs / rhs.to_f64().unwrap_or(f64::NAN)).into(),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => {
                    if rhs.to_i32().is_some() {
                        match lhs.checked_div(&Rational32::from_integer(*rhs as i32)) {
                            Some(num) => num.into(),
                            None => (lhs.to_f64().unwrap_or(f64::MAX) / *rhs as f64).into(),
                        }
                    } else {
                        (lhs.to_f64().unwrap_or(f64::MAX) / *rhs as f64).into()
                    }
                }
                Number::Float(rhs) => (lhs.to_f64().unwrap_or(f64::NAN) / *rhs).into(),
                Number::BigInt(rhs) => {
                    if rhs.to_i32().is_some() {
                        match lhs.checked_div(&Rational32::from_integer(rhs.to_i32().unwrap())) {
                            Some(num) => num.into(),
                            None => {
                                (lhs.to_f64().unwrap_or(f64::MAX) / rhs.to_f64().unwrap()).into()
                            }
                        }
                    } else {
                        (lhs.to_f64().unwrap_or(f64::MAX) / rhs.to_f64().unwrap()).into()
                    }
                }
                Number::Rational(rhs) => match lhs.checked_div(rhs) {
                    Some(num) => num.into(),
                    None => {
                        (lhs.to_f64().unwrap_or(f64::NAN) / rhs.to_f64().unwrap_or(f64::NAN)).into()
                    }
                },
            },
        }
    }
}

impl Number {
    /// Quotient with truncation (rounding towards zero).
    ///
    /// The spec only defines this function for integer inputs. Since that includes integral floats,
    /// we define quotient over all floats with the expectation that the caller can check the
    /// inputs for strict conformance.
    pub fn quotient(&self, rhs: &Self) -> Option<Number> {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((lhs / rhs).into()),
                Number::BigInt(rhs) => Some((BigInt::from(*lhs) / &**rhs).into()),
                Number::Float(rhs) => lhs.to_f64().map(|lhs| (lhs as f64 / rhs).trunc().into()),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        Some((*lhs / rhs.to_i64().unwrap()).into())
                    } else {
                        None
                    }
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((&**lhs / rhs).into()),
                Number::BigInt(rhs) => Some((&**lhs / &**rhs).into()),
                Number::Float(rhs) => lhs.to_f64().map(|lhs| (lhs / rhs).trunc().into()),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        Some((&**lhs / BigInt::from(rhs.to_i32().unwrap())).into())
                    } else {
                        None
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((lhs / *rhs as f64).into()),
                Number::Float(rhs) => Some((lhs / rhs).trunc().into()),
                Number::BigInt(_) => None,
                Number::Rational(rhs) => rhs.to_f64().map(|rhs| (lhs / rhs).into()),
            },
            Number::Rational(lhs) if lhs.is_integer() => match rhs {
                Number::Fixnum(rhs) => Some((lhs.to_i64().unwrap() / *rhs).into()),
                Number::Float(rhs) => lhs.to_f64().map(|lhs| (lhs / rhs).trunc().into()),
                Number::BigInt(rhs) => Some((BigInt::from(lhs.to_i64().unwrap()) / &**rhs).into()),
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        Some((lhs / rhs).into())
                    } else {
                        None
                    }
                }
            },
            Number::Rational(_) => None,
        }
    }
}

impl Rem for Number {
    type Output = Option<Number>;
    fn rem(self, rhs: Self) -> Self::Output {
        (&self).rem(&rhs)
    }
}

impl Rem for &Number {
    type Output = Option<Number>;

    /// Remainder
    ///
    /// The spec only defines remainder for integers but this operation is also used by other
    /// functions that deal with numbers of all types internally.
    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((lhs % rhs).into()),
                Number::BigInt(rhs) => Some((BigInt::from(*lhs) % &**rhs).into()),
                Number::Float(rhs) => Some((*lhs as f64 % rhs).into()),
                Number::Rational(rhs) => {
                    let result = Rational64::from_integer(*lhs)
                        % Rational64::from((*rhs.numer() as i64, *rhs.denom() as i64));
                    // lhs % rhs is guaranteed to be less than rhs. Since lhs is a whole integer,
                    // both numerator and denominator still fit in an i32.
                    Some(Rational32::from((*result.numer() as i32, *result.denom() as i32)).into())
                }
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((&**lhs % rhs).into()),
                Number::BigInt(rhs) => Some((&**lhs % &**rhs).into()),
                Number::Float(_) => None,
                Number::Rational(rhs) => {
                    if rhs.is_integer() {
                        // fast path if rhs is an integer
                        Some((&**lhs % BigInt::from(rhs.to_i32().unwrap())).into())
                    } else {
                        // if rhs is a rational, we can mod the bigint by the numerator
                        // and then divide the result by the denominator.
                        // Exercise to the reader: Prove that X % N/M == (X % N) / M
                        let denom = rhs.denom();
                        let numer_mod =
                            (&**lhs * BigInt::from(*denom)) % BigInt::from(*rhs.numer());
                        Some(Rational32::new(numer_mod.to_i32().unwrap(), *denom).into())
                    }
                }
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((lhs % *rhs as f64).into()),
                Number::Float(rhs) => Some((lhs % rhs).into()),
                // BigInt::to_f64 converts to infinity if it is too large
                // which yields the correct result since a % b = a if a < b
                Number::BigInt(rhs) => rhs.to_f64().map(|rhs| (lhs % rhs).into()),
                Number::Rational(rhs) => rhs.to_f64().map(|rhs| (lhs % rhs).into()),
            },
            Number::Rational(lhs) => match rhs {
                Number::Fixnum(rhs) => Some((lhs.to_i64().unwrap() % *rhs).into()),
                Number::Float(rhs) => lhs.to_f64().map(|lhs| (lhs % rhs).into()),
                Number::BigInt(rhs) => Some((BigInt::from(lhs.to_i64().unwrap()) % &**rhs).into()),
                Number::Rational(rhs) => Some((lhs % rhs).into()),
            },
        }
    }
}

impl Hash for Number {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Number::Fixnum(num) => num.hash(state),
            Number::Float(_) => panic!("unexpected hash of f64"),
            Number::BigInt(num) => num.hash(state),
            Number::Rational(num) => num.hash(state),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Fixnum(num) => write!(f, "{}", num),
            Number::BigInt(num) => write!(f, "{}", num),
            Number::Float(num) if *num > 1E10 => write!(f, "{:e}", num),
            Number::Float(num) if self.is_integer() => write!(f, "{:.1}", num),
            Number::Float(num) => write!(f, "{}", num),
            Number::Rational(num) => write!(f, "{}", num),
        }
    }
}

fn write_float_fract(mut num: f64, radix: usize, f: &mut Formatter<'_>) -> fmt::Result {
    let mut first_digit = true;
    loop {
        num = num.fract() * radix as f64;
        if num == 0_f64 {
            break;
        }
        if first_digit {
            write!(f, ".{:x}", num.trunc().abs() as i64)?;
            first_digit = false;
        } else {
            write!(f, "{:x}", num.trunc().abs() as i64)?;
        }
    }
    Ok(())
}

impl LowerHex for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Number::Fixnum(num) => fmt::LowerHex::fmt(num, f),
            Number::Float(num) => {
                if *num < 0_f64 {
                    write!(f, "-")?;
                }
                write!(f, "{:x}", num.trunc().abs() as i64)?;
                write_float_fract(*num, 16, f)
            }
            Number::BigInt(num) => fmt::LowerHex::fmt(num.as_ref(), f),
            Number::Rational(num) => fmt::LowerHex::fmt(num, f),
        }
    }
}

impl Octal for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Number::Fixnum(num) => fmt::Octal::fmt(num, f),
            Number::Float(num) => {
                if *num < 0_f64 {
                    write!(f, "-")?;
                }
                write!(f, "{:o}", num.trunc().abs() as i64)?;
                write_float_fract(*num, 8, f)
            }
            Number::BigInt(num) => fmt::Octal::fmt(num.as_ref(), f),
            Number::Rational(num) => fmt::Octal::fmt(num, f),
        }
    }
}

impl Binary for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Number::Fixnum(num) => fmt::Binary::fmt(num, f),
            Number::Float(num) => {
                if *num < 0_f64 {
                    write!(f, "-")?;
                }
                write!(f, "{:b}", num.trunc().abs() as i64)?;
                write_float_fract(*num, 2, f)
            }
            Number::BigInt(num) => fmt::Binary::fmt(num.as_ref(), f),
            Number::Rational(num) => fmt::Binary::fmt(num, f),
        }
    }
}

impl From<u32> for Number {
    fn from(num: u32) -> Self {
        Number::Fixnum(num as i64)
    }
}

impl From<i32> for Number {
    fn from(num: i32) -> Self {
        Number::Fixnum(num as i64)
    }
}

impl From<u64> for Number {
    fn from(num: u64) -> Self {
        if num > i64::MAX as u64 {
            Number::new_bigint(BigInt::from(num))
        } else {
            Number::Fixnum(num as i64)
        }
    }
}

impl From<usize> for Number {
    fn from(num: usize) -> Self {
        if num > i64::MAX as usize {
            Number::new_bigint(BigInt::from(num))
        } else {
            Number::Fixnum(num as i64)
        }
    }
}

impl From<i64> for Number {
    fn from(num: i64) -> Self {
        Number::Fixnum(num)
    }
}

impl From<f64> for Number {
    fn from(num: f64) -> Self {
        Number::Float(num)
    }
}

impl From<BigInt> for Number {
    fn from(num: BigInt) -> Self {
        Number::BigInt(Rc::new(num))
    }
}

impl From<Rational32> for Number {
    fn from(num: Rational32) -> Self {
        Number::Rational(num)
    }
}

/// Trigonometric functions
impl Number {
    pub fn exp(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.exp().into()),
            Number::Float(num) => Some(num.exp().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.exp().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.exp().into()),
        }
    }

    pub fn log(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.log(f64::E()).into()),
            Number::Float(num) => Some(num.log(f64::E()).into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.log(f64::E()).into()),
            Number::Rational(num) => num.to_f64().map(|num| num.log(f64::E()).into()),
        }
    }

    pub fn sin(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.sin().into()),
            Number::Float(num) => Some(num.sin().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.sin().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.sin().into()),
        }
    }

    pub fn cos(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.cos().into()),
            Number::Float(num) => Some(num.cos().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.cos().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.cos().into()),
        }
    }

    pub fn tan(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.tan().into()),
            Number::Float(num) => Some(num.tan().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.tan().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.tan().into()),
        }
    }

    pub fn asin(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.asin().into()),
            Number::Float(num) => Some(num.asin().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.asin().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.asin().into()),
        }
    }

    pub fn acos(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.acos().into()),
            Number::Float(num) => Some(num.acos().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.acos().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.acos().into()),
        }
    }

    pub fn atan(&self) -> Option<Number> {
        match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.atan().into()),
            Number::Float(num) => Some(num.atan().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.atan().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.atan().into()),
        }
    }

    pub fn atan2(&self, x: Number) -> Option<Number> {
        if let Some(x) = x.to_f64() {
            match self {
                Number::Fixnum(num) => num.to_f64().map(|num| num.atan2(x).into()),
                Number::Float(num) => Some(num.atan2(x).into()),
                Number::BigInt(num) => num.to_f64().map(|num| num.atan2(x).into()),
                Number::Rational(num) => num.to_f64().map(|num| num.atan2(x).into()),
            }
        } else {
            None
        }
    }

    /// sqrt is undefined for complex results
    pub fn sqrt(&self) -> Option<Number> {
        let result: Option<Number> = match self {
            Number::Fixnum(num) => num.to_f64().map(|num| num.sqrt().into()),
            Number::Float(num) => Some(num.sqrt().into()),
            Number::BigInt(num) => num.to_f64().map(|num| num.sqrt().into()),
            Number::Rational(num) => num.to_f64().map(|num| num.sqrt().into()),
        };

        // convert NaN and infinity to None
        if let Some(r) = &result {
            if r.to_f64().unwrap().is_finite() {
                result
            } else {
                None
            }
        } else {
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::number::Number;
    use num::bigint::BigInt;
    use num::traits::FloatConst;
    use num::{FromPrimitive, Rational32};
    use std::mem;
    use std::str::FromStr;

    macro_rules! verify {
        ($func:expr, $($lhs:expr, $rhs:expr => $result:expr),+) => {{
             $(
                assert_eq!($func(Number::from($lhs), Number::from($rhs)), Number::from($result),
                           "{:?} {:?} != {:?}", $lhs, $rhs, $result);
                assert_eq!(mem::discriminant(&$func(Number::from($lhs), Number::from($rhs))),
                           mem::discriminant(&Number::from($result)),
                           "{:?} {:?} != {:?}", $lhs, $rhs, $result);
             )+
        }};
    }

    #[test]
    fn to_usize() {
        assert!(Number::from(0).to_usize().is_some());
        assert!(Number::from(-1).to_usize().is_none());
        assert!(Number::from(1.0).to_usize().is_none());
        let overflow_num = Number::from(BigInt::from(usize::MAX)) + Number::from(1);
        assert!(overflow_num.to_usize().is_none());
        assert_eq!(Number::from(Rational32::new(10, 1)).to_usize(), Some(10));
    }

    #[test]
    fn eq() {
        assert_eq!(Number::from(100), Number::from(100));
        assert_ne!(Number::from(100), Number::from(150));

        assert_eq!(Number::from(100.0), Number::from(100));
        assert_eq!(Number::from(100), Number::from(100.0));

        assert_eq!(
            Number::from(100),
            Number::from(BigInt::from_str("100").unwrap())
        );
        assert_eq!(
            Number::from(BigInt::from_str("100").unwrap()),
            Number::from(100)
        );

        assert_eq!(
            Number::from(100.0),
            Number::from(BigInt::from_str("100").unwrap())
        );
        assert_eq!(
            Number::from(BigInt::from_str("100").unwrap()),
            Number::from(100.0)
        );

        assert_eq!(Number::from(Rational32::new(1, 2)), Number::from(0.5));
        assert_eq!(Number::from(0.5), Number::from(Rational32::new(1, 2)));

        assert_ne!(Number::from(Rational32::new(1, 2)), Number::from(0));
    }

    #[test]
    fn partial_ord() {
        assert!(Number::from(200) > Number::from(100));
        assert!(!(Number::from(100) > Number::from(100)));
        assert!(Number::from(100) >= Number::from(100));
        assert!(Number::from(200) >= Number::from(100));

        assert!(Number::from(200) > Number::from(100.0));
        assert!(Number::from(200.0) > Number::from(100));
        assert!(Number::from(200.0) > Number::from(100.0));

        assert!(Number::from(200) > Number::new_bigint(100));
        assert!(Number::from(200) >= Number::new_bigint(100));
        assert!(Number::from(200.0) > Number::new_bigint(100));

        assert!(Number::new_bigint(200) > Number::new_bigint(100));
        assert!(Number::new_bigint(200) >= Number::new_bigint(100));
        assert!(Number::new_bigint(200) > Number::new_bigint(100));

        assert!(Number::new_bigint(200) > Number::from(100.0));
        assert!(Number::new_bigint(200) >= Number::from(100.0));
        assert!(Number::new_bigint(200) > Number::from(100.0));
    }

    #[test]
    fn add() {
        let i32_overflow = i32::MAX as i64 + 1;

        // FIXNUM + RHS
        verify![|x, y| x + y,
            100, 50 => 150,
            100, i64::MAX => BigInt::from(i64::MAX) + 100,
            100, BigInt::from(50) => BigInt::from(150),
            100, 50.0 => 150.0,
            100, Rational32::from_integer(50) => Rational32::from_integer(150),
            i32_overflow, Rational32::from_integer(50) => i32_overflow as f64 + 50_f64
        ];

        // BIGINT + RHS
        verify![|x, y| x + y,
            BigInt::from(100), 50 => BigInt::from(150),
            BigInt::from(100), i64::MAX => BigInt::from(i64::MAX) + 100,
            BigInt::from(100), 50.0 => 150.0,
            BigInt::from(100), Rational32::from_integer(50) => BigInt::from(150),
            BigInt::from(100), Rational32::new(1, 2) => 100.50
        ];

        // FLOAT + RHS
        verify![|x, y| x + y,
            100.0, 50 => 150.0,
            100.0, 50.0 => 150.0,
            100.0, BigInt::from(50) => 150.0,
            100.0, Rational32::from_integer(50) => 150.0
        ];

        // RATIONAL + RHS
        verify![|x, y| x + y,
            Rational32::from_integer(100), 50 => Rational32::from_integer(150),
            Rational32::from_integer(100), i32_overflow => 100.0 + i32_overflow as f64,
            Rational32::from_integer(100), 50.0 => 150.0,
            Rational32::from_integer(100), BigInt::from(50) => BigInt::from(150),
            Rational32::new(1, 2), BigInt::from(50) => 50.50,
            Rational32::from_integer(i32::MAX), Rational32::from_integer(1) => i32::MAX as f64 + 1_f64
        ];
    }

    #[test]
    fn mul() {
        let i32_overflow = i32::MAX as i64 + 1;

        // FIXNUM * RHS
        verify![|x, y| x * y,
            100, 50 => 5000,
            100, i64::MAX => BigInt::from(i64::MAX) * 100,
            100, BigInt::from(50) => BigInt::from(5000),
            100, 50.0 => 5000.0,
            100, Rational32::from_integer(50) => Rational32::from_integer(5000),
            i32_overflow, Rational32::from_integer(50) => i32_overflow as f64 * 50_f64
        ];

        // // BIGINT * RHS
        verify![|x, y| x * y,
            BigInt::from(100), 50 => BigInt::from(5000),
            BigInt::from(100), BigInt::from(50) => BigInt::from(5000),
            BigInt::from(100), 50.0 => 5000.0,
            BigInt::from(100), Rational32::from_integer(50) => BigInt::from(5000),
            BigInt::from(100), Rational32::new(1, 2) => 50.0
        ];
        //
        // // FLOAT * RHS
        verify![|x, y| x * y,
            100.0, 50 => 5000.0,
            100.0, 50.0 => 5000.0,
            100.0, BigInt::from(50) => 5000.0,
            100.0, Rational32::from_integer(50) => 5000.0
        ];
        //
        // // RATIONAL * RHS
        verify![|x, y| x * y,
            Rational32::from_integer(100), 50 => Rational32::from_integer(5000),
            Rational32::from_integer(100), i32_overflow => 100.0 * i32_overflow as f64,
            Rational32::from_integer(100), 50.0 => 5000.0,
            Rational32::from_integer(100), BigInt::from(50) => BigInt::from(5000),
            Rational32::new(1, 2), BigInt::from(100) => 50.0,
            Rational32::from_integer(100), Rational32::from_integer(50) => Rational32::from_integer(5000),
            Rational32::from_integer(i32::MAX), Rational32::from_integer(2) => i32::MAX as f64 * 2_f64
        ];
    }

    #[test]
    fn sub() {
        let i32_overflow = i32::MIN as i64 - 1;

        // FIXNUM - RHS
        verify![|x, y| x - y,
            100, 50 => 50,
            -100, i64::MAX => BigInt::from(-i64::MAX) - 100,
            100, BigInt::from(50) => BigInt::from(50),
            100, 50.0 => 50.0,
            100, Rational32::from_integer(50) => Rational32::from_integer(50),
            i32::MIN as i64, Rational32::from_integer(50) => i32::MIN as f64 - 50_f64,
            i32_overflow, Rational32::from_integer(50) => i32_overflow as f64 - 50_f64
        ];

        // // BIGINT - RHS
        verify![|x, y| x - y,
            BigInt::from(100), 50 => BigInt::from(50),
            BigInt::from(100), BigInt::from(50) => BigInt::from(50),
            BigInt::from(100), 50.0 => 50.0,
            BigInt::from(100), Rational32::from_integer(50) => BigInt::from(50),
            BigInt::from(100), Rational32::new(1, 2) => 99.50
        ];

        // FLOAT - RHS
        verify![|x, y| x - y,
            100.0, 50 => 50.0,
            100.0, 50.0 => 50.0,
            100.0, BigInt::from(50) => 50.0,
            100.0, Rational32::from_integer(50) => 50.0,
            100.0, Rational32::new(1, 2) => 99.50
        ];

        // RATIONAL - RHS
        verify![|x, y| x - y,
            Rational32::from_integer(100), 50 => Rational32::from_integer(50),
            Rational32::from_integer(100), i32::MIN as i64 => 100.0 - i32::MIN as f64,
            Rational32::from_integer(100), i32_overflow => 100.0 - i32_overflow as f64,
            Rational32::from_integer(100), 50.0 => 50.0,
            Rational32::from_integer(100), BigInt::from(50) => BigInt::from(50),
            Rational32::new(1, 2), BigInt::from(50) => -49.5,
            Rational32::from_integer(100), Rational32::from_integer(50) => Rational32::from_integer(50),
            Rational32::from_integer(i32::MIN), Rational32::from_integer(1) => i32::MIN as f64 - 1_f64
        ];
    }

    #[test]
    fn div() {
        let i32_uflow = i32::MIN as i64 - 1;
        let i32_oflow = i32::MAX as i64 + 1;

        // FIXNUM / RHS
        verify![|x, y| x / y,
            100, 50 => Rational32::from_integer(2),
            i32_uflow, i32_uflow => 1.0,
            i32_oflow, i32_oflow => 1.0,
            100, BigInt::from(50) => Rational32::from_integer(2),
            i32_oflow, BigInt::from(i32_oflow) => 1.0,
            i32_oflow, BigInt::from(2) => i32_oflow as f64 / 2_f64,
            100, Rational32::from_integer(50) => Rational32::from_integer(2),
            100, Rational32::new(1, 2) => Rational32::from_integer(200),
            i32_oflow, Rational32::from_integer(2) => i32_oflow as f64 / 2_f64,
            i32::MAX as i64, Rational32::new(1, 2) => i32::MAX as f64 * 2.0,
            -100, 1.0 => -100.0
        ];

        // BIGINT / RHS
        verify![|x, y| x / y,
            BigInt::from(100), 50 => Rational32::from_integer(2),
            BigInt::from(i32_oflow), 2 => i32_oflow as f64 / 2_f64,
            BigInt::from(100), i32_oflow => 100 as f64 / i32_oflow as f64,
            BigInt::from(100), BigInt::from(50) => Rational32::from_integer(2),
            BigInt::from(i32_oflow), BigInt::from(100) => i32_oflow as f64 / 100_f64,
            BigInt::from(100), BigInt::from(i32_oflow) => 100_f64 / i32_oflow as f64,
            BigInt::from(100), 50.0 => 2.0,
            BigInt::from(100), Rational32::from_integer(50) => Rational32::from_integer(2),
            BigInt::from(i32_oflow), Rational32::from_integer(50) =>  i32_oflow as f64 / 50_f64,
            BigInt::from(i32::MAX), Rational32::new(1, 2) => Number::from(i32::MAX as f64 * 2_f64)
        ];

        // FLOAT / RHS
        verify![|x, y| x / y,
            100.0, 50 => 2.0,
            100.0, 50.0 => 2.0,
            100.0, BigInt::from(50) => 2.0,
            100.0, Rational32::from_integer(50) => 2.0,
            100.0, Rational32::new(1, 2) => 200.0
        ];

        // RATIONAL / RHS
        verify![|x, y| x / y,
            Rational32::from_integer(100), 50 => Rational32::from_integer(2),
            Rational32::from_integer(100), i32_oflow => 100_f64 / i32_oflow as f64,
            Rational32::from_integer(100), 50_f64 => 2_f64,
            Rational32::from_integer(100), BigInt::from(50) => Rational32::from_integer(2),
            Rational32::from_integer(100), BigInt::from(i32_oflow) => 100_f64 / i32_oflow as f64,
            Rational32::from_integer(100), Rational32::from_integer(50) => Rational32::from_integer(2),
            Rational32::from_integer(i32::MAX), Rational32::new(1,2) => i32::MAX as f64 * 2.0
        ];
    }

    #[test]
    fn rem() {
        // FIXNUM % RHS
        assert_eq!(Number::from(100) % Number::from(70), Some(Number::from(30)));
        assert_eq!(
            Number::from(100) % Number::from(BigInt::from(70)),
            Some(Number::from(30))
        );
        assert_eq!(
            Number::from(100) % Number::from(BigInt::from(i64::MAX) + BigInt::from(i64::MAX)),
            Some(Number::from(BigInt::from(100)))
        );
        assert_eq!(
            Number::from(100) % Number::from(1.0),
            Some(Number::Float(0.0))
        );
        assert_eq!(
            Number::from(100) % Number::from(Rational32::from_integer(70)),
            Some(Number::from(30))
        );
        assert_eq!(
            Number::from(100) % Number::from(Rational32::new(1, 2)),
            Some(Number::from(0))
        );
        assert_eq!(
            Number::from(1) % Number::from(Rational32::new(2, 5)),
            Some(Number::from(Rational32::new(1, 5)))
        );
        assert_eq!(
            Number::from(1) % Number::from(Rational32::new(3, 5)),
            Some(Number::from(Rational32::new(2, 5)))
        );

        // BIGINT % RHS
        assert_eq!(
            Number::from(BigInt::from(100)) % Number::from(70),
            Some(Number::from(30))
        );
        assert_eq!(
            Number::from(BigInt::from(100)) % Number::from(BigInt::from(70)),
            Some(Number::from(BigInt::from(30)))
        );
        // cannot support bigint % float (yet?)
        assert_eq!(Number::from(BigInt::from(100)) % Number::from(1.0), None);
        assert_eq!(
            Number::from(BigInt::from(100)) % Number::from(Rational32::from_integer(70)),
            Some(Number::from(BigInt::from(30)))
        );
        assert_eq!(
            Number::from(BigInt::from(1)) % Number::from(Rational32::new(2, 3)),
            Some(Number::from(Rational32::new(1, 3)))
        );

        // RATIONAL % RHS
        assert_eq!(
            Number::from(Rational32::from_integer(100)) % Number::from(70),
            Some(Number::from(30))
        );
        assert_eq!(
            Number::from(Rational32::from_integer(100)) % Number::from(BigInt::from(70)),
            Some(Number::from(BigInt::from(30)))
        );
        assert_eq!(
            Number::from(Rational32::from_integer(100))
                % Number::from(Rational32::from_integer(70)),
            Some(Number::from(Rational32::from_integer(30)))
        );
        assert_eq!(
            Number::from(Rational32::from_integer(1)) % Number::from(Rational32::new(2, 3)),
            Some(Number::from(Rational32::new(1, 3)))
        );

        // FLOAT % RHS
        assert_eq!(
            Number::from(0.5) % Number::from(70),
            Some(Number::from(0.5))
        );
        assert_eq!(
            Number::from(100.0) % Number::from(70),
            Some(Number::from(30.0))
        );
        assert!((Number::from(f64::INFINITY) % Number::from(70))
            .unwrap()
            .to_f64()
            .unwrap()
            .is_nan());
        assert_eq!(
            Number::from(100.0) % Number::from(BigInt::from(70)),
            Some(Number::from(30.0))
        );
        assert_eq!(
            Number::from(100.0) % Number::from(BigInt::from(i64::MAX) + BigInt::from(i64::MAX)),
            Some(Number::from(100.0))
        );
        assert_eq!(
            Number::from(101.0) % Number::from(2.0),
            Some(Number::from(1.0))
        );
        assert_eq!(
            Number::from(100.0) % Number::from(Rational32::from_integer(70)),
            Some(Number::from(30.0))
        );
        assert_eq!(
            Number::from(100.0) % Number::from(Rational32::new(1, 2)),
            Some(Number::from(0.0))
        );
        assert_eq!(
            Number::from(0.75) % Number::from(Rational32::new(1, 2)),
            Some(Number::from(0.25))
        );
    }

    #[test]
    fn modulo() {
        assert_eq!(
            Number::from(-21).modulo(&Number::from(4)),
            Some(Number::from(3))
        );
    }

    #[test]
    fn round() {
        assert_eq!(Number::from(-4.3).floor(), Number::from(-5));
        assert_eq!(Number::from(-4.3).ceil(), Number::from(-4));
        assert_eq!(Number::from(-4.3).truncate(), Number::from(-4));
        assert_eq!(Number::from(-4.3).round(), Number::from(-4));

        assert_eq!(Number::from(3.5).floor(), Number::from(3));
        assert_eq!(Number::from(3.5).ceil(), Number::from(4));
        assert_eq!(Number::from(3.5).truncate(), Number::from(3));
        assert_eq!(Number::from(3.5).round(), Number::from(4));

        assert_eq!(Number::from(Rational32::new(7, 2)).round(), Number::from(4));
    }

    #[test]
    fn abs() {
        assert_eq!(Number::from(-10).abs(), Number::from(10));
        assert_eq!(
            Number::from(i64::MIN).abs(),
            Number::from(BigInt::from(9223372036854775808_u64))
        );
    }

    #[test]
    fn sin() {
        assert_eq!(Number::from(0).sin(), Some(Number::from(0)));
        assert_eq!(
            Number::from(f64::FRAC_PI_2()).sin(),
            Some(Number::from(1.0))
        );
        assert_eq!(
            Number::from(Rational32::from_integer(0)).sin(),
            Some(Number::from(0.0))
        );
        assert_eq!(Number::from(BigInt::from(0)).sin(), Some(Number::from(0.0)));
        assert!(Number::from(BigInt::from_f64(f64::MAX).unwrap() * 2)
            .sin()
            .unwrap()
            .to_f64()
            .unwrap()
            .is_nan());
    }
}
