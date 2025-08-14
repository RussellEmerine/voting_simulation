//! The quartiles in the `plotters` library calculates the "lower fence" and "upper fence" using
//! the IQR. This behavior is not what I wanted. Instead I need the lower whisker to be the minimum
//! and the upper whisker to be the maximum. This file implements quartiles the way I want them.
//!
//! I can also remove the unnecessary casts to `f32`.

/// The quartiles
#[derive(Clone, Debug)]
pub struct Quartiles {
    minimum: f64,
    lower: f64,
    median: f64,
    upper: f64,
    maximum: f64,
}

impl Quartiles {
    // Extract a value representing the `pct` percentile of a
    // sorted `s`, using linear interpolation.
    fn percentile_of_sorted<T: Into<f64> + Copy>(s: &[T], pct: f64) -> f64 {
        assert!(!s.is_empty());
        if s.len() == 1 {
            return s[0].into();
        }
        assert!(0_f64 <= pct);
        let hundred = 100_f64;
        assert!(pct <= hundred);
        if (pct - hundred).abs() < f64::EPSILON {
            return s[s.len() - 1].into();
        }
        let length = (s.len() - 1) as f64;
        let rank = (pct / hundred) * length;
        let lower_rank = rank.floor();
        let d = rank - lower_rank;
        let n = lower_rank as usize;
        let lo = s[n].into();
        let hi = s[n + 1].into();
        lo + (hi - lo) * d
    }

    /// Create a new quartiles struct with the values calculated from the argument.
    ///
    /// - `s`: The array of the original values
    /// - **returns** The newly created quartiles
    ///
    /// ```rust
    /// use plotters::prelude::*;
    ///
    /// let quartiles = Quartiles::new(&[7, 15, 36, 39, 40, 41]);
    /// assert_eq!(quartiles.median(), 37.5);
    /// ```
    pub fn new<T: Into<f64> + Copy + PartialOrd>(s: &[T]) -> Self {
        let mut s = s.to_owned();
        s.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap());

        assert!(!s.is_empty());
        let minimum = s[0].into();
        let lower = Quartiles::percentile_of_sorted(&s, 25_f64);
        let median = Quartiles::percentile_of_sorted(&s, 50_f64);
        let upper = Quartiles::percentile_of_sorted(&s, 75_f64);
        let maximum = (*s.last().unwrap()).into();
        Self {
            minimum,
            lower,
            median,
            upper,
            maximum,
        }
    }

    /// Get the quartiles values.
    ///
    /// - **returns** The array [lower fence, lower quartile, median, upper quartile, upper fence]
    ///
    /// ```rust
    /// use plotters::prelude::*;
    ///
    /// let quartiles = Quartiles::new(&[7, 15, 36, 39, 40, 41]);
    /// let values = quartiles.values();
    /// assert_eq!(values, [-9.0, 20.25, 37.5, 39.75, 69.0]);
    /// ```
    pub fn values(&self) -> [f64; 5] {
        [
            self.minimum,
            self.lower,
            self.median,
            self.upper,
            self.maximum,
        ]
    }

    /// Get the quartiles median.
    ///
    /// - **returns** The median
    ///
    /// ```rust
    /// use plotters::prelude::*;
    ///
    /// let quartiles = Quartiles::new(&[7, 15, 36, 39, 40, 41]);
    /// assert_eq!(quartiles.median(), 37.5);
    /// ```
    pub fn median(&self) -> f64 {
        self.median
    }
}
