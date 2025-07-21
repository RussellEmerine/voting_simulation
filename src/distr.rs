//! Utility random distributions not provided by the `rand_distr` crate

use rand::distr::Distribution;
use rand::Rng;
use std::ops::RangeBounds;

/// A constant distribution that always returns the same value. (Perhaps there is some way to use
/// Uniform to the same effect, but it is unclear in the case of floating points that it would
/// always work, and this is easy enough to implement anyway.)
pub struct Constant<T> {
    value: T,
}

impl<T> Constant<T> {
    pub fn new(value: T) -> Self {
        Constant { value }
    }
}

impl<T> Distribution<T> for Constant<T>
where
    T: Clone,
{
    fn sample<R: Rng + ?Sized>(&self, _: &mut R) -> T {
        self.value.clone()
    }
}

/// Truncate a distribution to the given bounds.
pub struct Truncate<D, B> {
    distr: D,
    range: B,
}

impl<D, B> Truncate<D, B> {
    pub fn new(distr: D, range: B) -> Truncate<D, B> {
        Truncate { distr, range }
    }
}

impl<T, D, B> Distribution<T> for Truncate<D, B>
where
    D: Distribution<T>,
    B: RangeBounds<T>,
    T: PartialOrd,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> T {
        loop {
            let x = rng.sample(&self.distr);
            if self.range.contains(&x) {
                return x;
            }
        }
    }
}

/// Use distr to choose between distr1 and distr2, then use distr1 or distr2 to choose the value
/// accordingly. This could be more accurately called a "mixed" rather than "bimodal" distribution.
pub struct Bimodal<D, D1, D2> {
    distr: D,
    distr1: D1,
    distr2: D2,
}

impl<D, D1, D2> Bimodal<D, D1, D2> {
    pub fn new(distr: D, distr1: D1, distr2: D2) -> Bimodal<D, D1, D2> {
        Bimodal {
            distr,
            distr1,
            distr2,
        }
    }
}

impl<T, D, D1, D2> Distribution<T> for Bimodal<D, D1, D2>
where
    D: Distribution<bool>,
    D1: Distribution<T>,
    D2: Distribution<T>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> T {
        if rng.sample(&self.distr) {
            rng.sample(&self.distr1)
        } else {
            rng.sample(&self.distr2)
        }
    }
}
