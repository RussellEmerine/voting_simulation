use ordered_float::NotNan;
use rand::distr::Distribution;
use rand::prelude::SliceRandom;
use rand::Rng;
use std::cmp::Reverse;
// For now I just generate all of the utilities at once, but when I scale it up I might instead
// provide a way to generate a voter and its utilities on the fly, so the data doesn't have to all
// be stored in memory at one time. That is probably unnecessary for small candidate counts.

/// If there are n voters, they are numbered from 0 to n - 1.
pub type Voter = usize;

/// If there are c candidates, they are numbered from 0 to c - 1.
pub type Candidate = usize;

/// A representation of the technique used to determine poll order used.
pub enum PollOrder {
    /// Random poll order, independent of the actual population utilities
    Random,
    /// The candidates are ordered by their actual average utilities.
    Range,
    /// The candidates are ordered by how many voters consider them the most liked candidate.
    Plurality,
}

/// A structure representing the utility for each voter-candidate pair.
/// It is assumed that all utilities are in [0.0, 1.0].
#[derive(Clone, Debug)]
pub struct UtilityMap(Vec<Vec<NotNan<f64>>>, Vec<Candidate>);

impl UtilityMap {
    /// Construct a utility map directly from a Vec<Vec<NotNan<f64>>>.
    /// This checks that there is at least one voter and at least two candidates,
    /// that every voter has utilities for each candidate,
    /// and that every utility is normalized to [0, 1].
    pub fn new(map: Vec<Vec<NotNan<f64>>>, poll_order: PollOrder) -> Self {
        assert!(!map.is_empty());
        assert!(map[0].len() >= 2);
        assert!(map.iter().all(|row| row.len() == map[0].len()));
        assert!(map
            .iter()
            .flatten()
            .all(|&u| NotNan::new(0.0).unwrap() <= u && u <= NotNan::new(1.0).unwrap()));
        let mut utility_map = Self(map, vec![]);
        let mut v: Vec<_> = (0..utility_map.candidate_count()).collect();
        match poll_order {
            PollOrder::Random => {
                v.shuffle(&mut rand::rng());
            }
            PollOrder::Range => {
                v.sort_by_cached_key(|&c| Reverse(utility_map.evaluate(c)));
            }
            PollOrder::Plurality => {
                let mut votes = vec![0; utility_map.candidate_count()];
                for i in 0..utility_map.voter_count() {
                    let c = (0..utility_map.candidate_count())
                        .max_by_key(|&c| utility_map.utility(i, c))
                        .expect("no candidates found");
                    votes[c] += 1;
                }
                v.sort_by_key(|&c| Reverse(votes[c]));
            }
        }
        utility_map.1 = v;
        utility_map
    }

    /// The number of candidates - this should always be at least 2.
    pub fn candidate_count(&self) -> usize {
        self.0[0].len()
    }

    /// The number of voters - this should always be at least 1.
    pub fn voter_count(&self) -> usize {
        self.0.len()
    }

    /// The utility for voter `i` of candidate `c`.
    pub fn utility(&self, i: Voter, c: Candidate) -> NotNan<f64> {
        self.0[i][c]
    }

    /// The total summed utility for candidate `c` across all voters.
    pub fn evaluate(&self, c: Candidate) -> NotNan<f64> {
        (0..self.voter_count())
            .map(|voter| self.utility(voter, c))
            .sum()
    }

    /// The objectively best candidate by measure of total utility.
    pub fn best_candidate(&self) -> Candidate {
        (0..self.candidate_count())
            .max_by_key(|&candidate| self.evaluate(candidate))
            .expect("no candidate")
    }

    /// Assumes accurate honest polling and returns the candidates, likeliest first.
    /// (We could return utility sum information, but that is usually unnecessary for
    /// strategical voting, and also can be calculated using the UtilityMap itself.)
    pub fn polling_order(&self) -> Vec<Candidate> {
        self.1.clone()
    }

    /// For COAF systems (in the limit of infinite voters and with some other assumptions),
    /// the optimal strategy is to consider the candidates in polling order and greedily take
    /// the subset of the vote space that maximizes the voter's potential effect on the
    /// current candidate's chances to be elected. This function returns the candidates in polling
    /// order and also tells whether the voter should support or oppose the candidate according
    /// to the moving average metric, as described in Smith 2000.
    ///
    /// If the `j`th element is `(c, b)` then `c` is the `j`th candidate in the polling order.
    /// If `b` is `true`, then the voter wants to support the candidate based on the moving average,
    /// otherwise the voter wants to oppose the candidate.
    pub fn moving_average_preference(&self, i: Voter) -> Vec<(Candidate, bool)> {
        let order = self.polling_order();
        let mut out: Vec<_> = order.iter().map(|&c| (c, false)).collect();
        if self.utility(i, order[0]) >= self.utility(i, order[1]) {
            out[0].1 = true;
        } else {
            out[1].1 = true;
        }
        let mut total = self.utility(i, order[0]) + self.utility(i, order[1]);
        for k in 2..order.len() {
            out[k].1 = self.utility(i, order[k]) >= total / NotNan::new(k as f64).unwrap();
            total += self.utility(i, order[k]);
        }
        out
    }

    /// The regret of choosing candidate `c`, i.e. how much utility could have been achieved
    /// by choosing the true optimal candidate instead.
    pub fn regret(&self, c: Candidate) -> NotNan<f64> {
        self.evaluate(self.best_candidate()) - self.evaluate(c)
    }

    /// A common complaint against the concept of using raw utility to evaluate voting systems is
    /// that voters have no way of knowing how their own utility values compare to others'. One
    /// way to address this is to scale all values to [0.0, 1.0] for each voter, where the minimum
    /// utility candidate is reassigned to 0.0 and the maximum utility candidate to 1.0.
    ///
    /// The interface of consuming `self` and returning a new `Self` allows for using
    /// `utility_map.clone().scale_voter_utilities()` to select a candidate, and then to reevaluate
    /// the selected candidate against the original `utility_map`, if so desired.
    pub fn scale_voter_utilities(mut self) -> Self {
        for i in 0..self.voter_count() {
            let min = (0..self.candidate_count())
                .map(|c| self.utility(i, c))
                .min()
                .expect("no candidates found");
            let max = (0..self.candidate_count())
                .map(|c| self.utility(i, c))
                .max()
                .expect("no candidates found");
            for c in 0..self.candidate_count() {
                self.0[i][c] = (self.utility(i, c) - min) / (max - min);
            }
        }
        self
    }

    // TODO: regret with variance for incomplete knowledge

    /// Generate utilities for every voter-candidate pair using the specified random distribution.
    /// *This distribution is expected to be in [0.0, 0.1].*
    pub fn random(
        n: usize,
        c: usize,
        rng: &mut impl Rng,
        distr: impl Distribution<f64>,
        poll_order: PollOrder,
    ) -> UtilityMap {
        UtilityMap::new(
            (0..n)
                .map(|_| {
                    (0..c)
                        .map(|_| NotNan::new(rng.sample(&distr)).unwrap())
                        .collect()
                })
                .collect(),
            poll_order,
        )
    }

    /// Issue-based utilities: There are a number (k) of issues and each candidate and voter has a vector
    /// of stances on each issue. If the stances are similar between a candidate and a voter,
    /// the voter will assign a higher utility to the candidate.
    /// In particular, each issue stance must be randomized within [-1, 1], and there must be at
    /// least one issue. Then, the dot product is at maximum k and at minimum -k,
    /// so adding k and dividing by 2k normalizes to [0, 1].
    pub fn random_issue_based<Rng: rand::Rng>(
        n: usize,
        c: usize,
        rng: &mut Rng,
        issue_distrs: &[Box<dyn Fn(&mut Rng) -> f64>],
        poll_order: PollOrder,
    ) -> UtilityMap {
        let candidate_stances: Vec<Vec<f64>> = (0..c)
            .map(|_| issue_distrs.iter().map(|distr| distr(rng)).collect())
            .collect();
        UtilityMap::new(
            (0..n)
                .map(|_| {
                    let voter_stances: Vec<f64> =
                        issue_distrs.iter().map(|distr| distr(rng)).collect();
                    (0..c)
                        .map(|c| {
                            let dot = candidate_stances[c]
                                .iter()
                                .zip(&voter_stances)
                                .map(|(&x, &y)| x * y)
                                .sum::<f64>();
                            NotNan::new(
                                (dot + issue_distrs.len() as f64) / (2 * issue_distrs.len()) as f64,
                            )
                            .unwrap()
                        })
                        .collect()
                })
                .collect(),
            poll_order,
        )
    }
}
