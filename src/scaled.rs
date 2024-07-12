pub type Scaled = i32;

// 101.
pub const UNITY: i32 = 0x10000; // 2^16, represents 1.0 in Scaled
pub const TWO: i32 = 0x20000; // 2^17, represents 2.0 in Scaled

/// Indicates an arithmetic error.
#[derive(Debug)]
pub struct ArithError;

/// See 105.
pub fn nx_plus_y(n: i32, x: Scaled, y: Scaled) -> Result<Scaled, ArithError> {
    mult_and_add(n, x, y, i32::MAX >> 1)
}

// NOTE Should Scaled become its own type, this trait should become part
// of that implementation.
pub trait DimenMult {
    fn multiply(&mut self, factor: i32) -> Result<(), ArithError>;
}

impl DimenMult for Scaled {
    fn multiply(&mut self, factor: i32) -> Result<(), ArithError> {
        *self = nx_plus_y(*self, factor, 0)?;
        Ok(())
    }
}

/// See 105.
pub fn mult_integers(n: i32, x: Scaled) -> Result<Scaled, ArithError> {
    mult_and_add(n, x, 0, i32::MAX)
}

/// Calculates n*x + y.
/// Returns an error if the absolute value of the calculation would exceed max_answer.
/// See 105.
fn mult_and_add(
    mut n: i32,
    mut x: Scaled,
    y: Scaled,
    max_answer: Scaled,
) -> Result<Scaled, ArithError> {
    if n < 0 {
        x = -x;
        n = -n;
    }
    if n == 0 {
        return Ok(y);
    }
    if (x <= (max_answer - y) / n) && (-x <= (max_answer + y) / n) {
        Ok(n * x + y)
    } else {
        Err(ArithError)
    }
}

/// This function calculates `x` times `n` divided by `d`.
/// Both `n` and `d` must be at most 2^16, `n` must be non-negative, `d` must be positive.
/// Note that x as a `Scaled` may at most be 2^30 - 1.
/// See 107.
pub fn xn_over_d(x: Scaled, n: i32, d: i32) -> Result<Scaled, ArithError> {
    let (res, _) = xn_over_d_with_remainder(x, n, d)?;
    Ok(res)
}

/// This function calculates `x` times `n` divided by `d`.
/// Both `n` and `d` must be at most 2^16, `n` must be non-negative, `d` must be positive.
/// Note that x as a `Scaled` may at most be 2^30 - 1.
/// See 107.
pub fn xn_over_d_with_remainder(
    mut x: Scaled,
    n: i32,
    d: i32,
) -> Result<(Scaled, Scaled), ArithError> {
    let positive = if x >= 0 {
        true
    } else {
        x = -x;
        false
    };
    // t are the low 15 bits of x multiplied by n.
    let t = (x % 0x8000) * n;
    // u are the top 17 bits of x after multiplication with n.
    let u = (x / 0x8000) * n + (t / 0x8000);
    // The low 15 bits of x*n plus that part of the top 17 bits of x*n that would be lost
    // when dividing by d.
    let v = (u % d) * 0x8000 + (t % 0x8000);
    // If u / d needs more than 15 bits, then 2^15 * u / d would be larger or equal to 2^30
    // which we don't allow for `Scaled`, so we need to catch that.
    if u / d >= 0x8000 {
        return Err(ArithError);
    }
    let res = 0x8000 * (u / d) + (v / d);
    if positive {
        let remainder = v % d;
        Ok((res, remainder))
    } else {
        let remainder = -(v % d);
        Ok((-res, remainder))
    }
}

pub const INF_BAD: i32 = 10000;

/// NOTE `t` must be non-negative.
/// See 108.
pub fn calculate_badness(t: Scaled, s: Scaled) -> i32 {
    let result;
    let r: i32;
    if t == 0 {
        result = 0;
    } else if s <= 0 {
        result = INF_BAD;
    } else {
        if t <= 7_230_584 {
            r = (t * 297) / s;
        } else if s >= 1_663_497 {
            r = t / (s / 297);
        } else {
            r = t;
        }
        if r > 1290 {
            result = INF_BAD;
        } else {
            result = (r * r * r + 0o400000) / 0o1000000;
        }
    }
    result
}
