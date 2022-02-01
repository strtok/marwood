use num::bigint::BigInt;
use num::{Num, ToPrimitive};
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign, Div, Mul, MulAssign, Rem, Sub};
use std::rc::Rc;

#[derive(Debug)]
pub enum Exactness {
    Exact,
    Inexact,
    Unspecified,
}

#[derive(Clone, Debug)]
pub enum Number {
    Fixnum(i64),
    Float(f64),
    BigInt(Rc<BigInt>),
}

impl Number {
    pub fn new_bigint<T: Into<BigInt>>(num: T) -> Number {
        Number::BigInt(Rc::new(num.into()))
    }

    pub fn parse(text: &str, exactness: Exactness, radix: u32) -> Option<Number> {
        match exactness {
            Exactness::Exact | Exactness::Unspecified => {
                if let Ok(num) = i64::from_str_radix(text, radix) {
                    Some(Number::from(num))
                } else if let Ok(num) = BigInt::from_str_radix(text, radix) {
                    Some(Number::from(num))
                } else if let Ok(num) = f64::from_str_radix(text, radix) {
                    Some(Number::from(num))
                } else {
                    None
                }
            }
            Exactness::Inexact => {
                if let Ok(num) = f64::from_str_radix(text, radix) {
                    Some(Number::from(num))
                } else {
                    None
                }
            }
        }
    }

    pub fn to_usize(&self) -> Option<usize> {
        match self {
            Number::Fixnum(num) if *num >= 0 => Some(*num as usize),
            Number::BigInt(num) if **num > BigInt::from(0) && **num <= BigInt::from(usize::MAX) => {
                Some(num.to_usize().unwrap())
            }
            _ => None,
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
                Number::Float(_) => false,
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => **lhs == BigInt::from(*rhs),
                Number::BigInt(rhs) => lhs == rhs,
                Number::Float(_) => false,
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(_) => false,
                Number::Float(rhs) => lhs == rhs,
                Number::BigInt(_) => false,
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
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (**lhs).partial_cmp(&BigInt::from(*rhs)),
                Number::BigInt(rhs) => (**lhs).partial_cmp(&**rhs),
                Number::Float(rhs) => (**lhs).to_f64().unwrap().partial_cmp(rhs),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Number::Float(rhs) => lhs.partial_cmp(rhs),
                Number::BigInt(rhs) => lhs.partial_cmp(&(**rhs).to_f64().unwrap()),
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
                Number::Fixnum(rhs) => match lhs.checked_add(*rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) + rhs).into(),
                },
                Number::BigInt(rhs) => (&**rhs + lhs).into(),
                Number::Float(rhs) => (*lhs as f64 + rhs).into(),
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs + rhs).into(),
                Number::BigInt(rhs) => (&**lhs + &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() + *rhs).into(),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs + *rhs as f64).into(),
                Number::Float(rhs) => (*lhs + *rhs).into(),
                Number::BigInt(rhs) => (*lhs + rhs.to_f64().unwrap()).into(),
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
                Number::Fixnum(rhs) => match lhs.checked_mul(*rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) * rhs).into(),
                },
                Number::BigInt(rhs) => (&**rhs * lhs).into(),
                Number::Float(rhs) => (*lhs as f64 * rhs).into(),
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs * rhs).into(),
                Number::BigInt(rhs) => (&**lhs * &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() * *rhs).into(),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs * *rhs as f64).into(),
                Number::Float(rhs) => (*lhs * *rhs).into(),
                Number::BigInt(rhs) => (*lhs * rhs.to_f64().unwrap()).into(),
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
                Number::Fixnum(rhs) => match lhs.checked_sub(*rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) - rhs).into(),
                },
                Number::BigInt(rhs) => (lhs - &**rhs).into(),
                Number::Float(rhs) => (*lhs as f64 - rhs).into(),
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs - rhs).into(),
                Number::BigInt(rhs) => (&**lhs - &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() - *rhs).into(),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs - *rhs as f64).into(),
                Number::Float(rhs) => (*lhs - *rhs).into(),
                Number::BigInt(rhs) => (*lhs - rhs.to_f64().unwrap()).into(),
            },
        }
    }
}

impl Div for Number {
    type Output = Number;
    fn div(self, rhs: Self) -> Self::Output {
        (&self).sub(&rhs)
    }
}

impl Div for &Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => match lhs.checked_div(*rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) / rhs).into(),
                },
                Number::BigInt(rhs) => (lhs / &**rhs).into(),
                Number::Float(rhs) => (*lhs as f64 / rhs).into(),
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs / rhs).into(),
                Number::BigInt(rhs) => (&**lhs / &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() / *rhs).into(),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs / *rhs as f64).into(),
                Number::Float(rhs) => (*lhs / *rhs).into(),
                Number::BigInt(rhs) => (*lhs / rhs.to_f64().unwrap()).into(),
            },
        }
    }
}

impl Rem for Number {
    type Output = Number;
    fn rem(self, rhs: Self) -> Self::Output {
        (&self).sub(&rhs)
    }
}

impl Rem for &Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match self {
            Number::Fixnum(lhs) => match rhs {
                Number::Fixnum(rhs) => match lhs.checked_rem(*rhs) {
                    Some(num) => Number::Fixnum(num),
                    None => (BigInt::from(*lhs) % rhs).into(),
                },
                Number::BigInt(rhs) => (lhs % &**rhs).into(),
                Number::Float(rhs) => (*lhs as f64 % rhs).into(),
            },
            Number::BigInt(lhs) => match rhs {
                Number::Fixnum(rhs) => (&**lhs % rhs).into(),
                Number::BigInt(rhs) => (&**lhs % &**rhs).into(),
                Number::Float(rhs) => (lhs.to_f64().unwrap() % *rhs).into(),
            },
            Number::Float(lhs) => match rhs {
                Number::Fixnum(rhs) => (*lhs % *rhs as f64).into(),
                Number::Float(rhs) => (*lhs % *rhs).into(),
                Number::BigInt(rhs) => (*lhs % rhs.to_f64().unwrap()).into(),
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
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Fixnum(num) => write!(f, "{}", num),
            Number::BigInt(num) => write!(f, "{}", num),
            Number::Float(num) if *num > 1E10 => write!(f, "{:E}", num),
            Number::Float(num) => write!(f, "{}", num),
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

#[cfg(test)]
mod tests {
    use crate::number::Number;
    use num::bigint::BigInt;
    use std::str::FromStr;

    #[test]
    fn to_usize() {
        assert!(Number::from(0).to_usize().is_some());
        assert!(Number::from(-1).to_usize().is_none());
        assert!(Number::from(1.0).to_usize().is_none());
        let overflow_num = Number::from(BigInt::from(usize::MAX)) + Number::from(1);
        assert!(overflow_num.to_usize().is_none());
    }

    #[test]
    fn eq() {
        assert_eq!(Number::from(100), Number::from(100));
        assert_ne!(Number::from(100), Number::from(150));

        assert_ne!(Number::from(100.0), Number::from(100));
        assert_ne!(Number::from(100), Number::from(100.0));

        assert_eq!(
            Number::from(100),
            Number::from(BigInt::from_str("100").unwrap())
        );
        assert_eq!(
            Number::from(BigInt::from_str("100").unwrap()),
            Number::from(100)
        );

        assert_ne!(
            Number::from(100.0),
            Number::from(BigInt::from_str("100").unwrap())
        );
        assert_ne!(
            Number::from(BigInt::from_str("100").unwrap()),
            Number::from(100.0)
        );
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
        assert_eq!(
            Number::Fixnum(100) + Number::Fixnum(100),
            Number::Fixnum(200)
        );

        assert_eq!(
            Number::Fixnum(100) + Number::Float(1.0),
            Number::Float(101.0)
        );

        assert_eq!(
            Number::Float(1.0) + Number::Fixnum(100),
            Number::Float(101.0)
        );

        assert_eq!(
            Number::Fixnum(100) + Number::new_bigint(100),
            Number::new_bigint(200)
        );

        assert_eq!(
            Number::new_bigint(100) + Number::Fixnum(100),
            Number::new_bigint(200)
        );

        assert_eq!(
            Number::Fixnum(i64::MAX) + Number::Fixnum(1),
            Number::new_bigint(i64::MAX) + Number::new_bigint(1)
        );

        assert_eq!(
            Number::Fixnum(1) + Number::Fixnum(i64::MAX),
            Number::new_bigint(i64::MAX) + Number::new_bigint(1)
        );

        assert!(matches!(
            Number::Float(100.0) + Number::new_bigint(100),
            Number::Float(_)
        ));

        assert!(matches!(
            Number::new_bigint(100) + Number::Float(100.0),
            Number::Float(_)
        ));
    }

    #[test]
    fn mul() {
        assert_eq!(Number::Fixnum(5) * Number::Fixnum(10), Number::Fixnum(50));
        assert_eq!(Number::Fixnum(5) * Number::Float(10.0), Number::Float(50.0));
        assert_eq!(Number::Float(5.0) * Number::Fixnum(10), Number::Float(50.0));
        assert_eq!(
            Number::Fixnum(5) * Number::new_bigint(10),
            Number::new_bigint(50)
        );
        assert_eq!(
            Number::new_bigint(5) * Number::Fixnum(10),
            Number::new_bigint(50)
        );
        assert_eq!(
            Number::Fixnum(i64::MAX) * Number::Fixnum(2),
            Number::new_bigint(i64::MAX) * Number::new_bigint(2)
        );

        assert_eq!(
            Number::Fixnum(2) * Number::Fixnum(i64::MAX),
            Number::new_bigint(i64::MAX) * Number::new_bigint(2)
        );

        assert!(matches!(
            Number::Float(5.0) * Number::new_bigint(10),
            Number::Float(_)
        ));

        assert!(matches!(
            Number::new_bigint(5) * Number::Float(10.0),
            Number::Float(_)
        ));
    }

    #[test]
    fn sub() {
        assert_eq!(Number::Fixnum(5) - Number::Fixnum(10), Number::Fixnum(-5));
        assert_eq!(Number::Fixnum(5) - Number::Float(10.0), Number::Float(-5.0));
        assert_eq!(Number::Float(5.0) - Number::Fixnum(10), Number::Float(-5.0));
        assert_eq!(
            Number::Fixnum(5) - Number::new_bigint(10),
            Number::new_bigint(-5)
        );
        assert_eq!(
            Number::new_bigint(5) - Number::Fixnum(10),
            Number::new_bigint(-5)
        );
        assert_eq!(
            Number::Fixnum(i64::MIN) - Number::Fixnum(1),
            Number::new_bigint(i64::MIN) - Number::new_bigint(1)
        );

        assert!(matches!(
            Number::Float(5.0) - Number::new_bigint(10),
            Number::Float(_)
        ));

        assert!(matches!(
            Number::new_bigint(5) - Number::Float(10.0),
            Number::Float(_)
        ));
    }
}
