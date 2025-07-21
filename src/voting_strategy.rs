use crate::utility_map::{Candidate, UtilityMap};
use ordered_float::NotNan;
use std::cmp::Reverse;
use std::marker::PhantomData;

/// This trait includes both the voting system itself and the strategy used by the voters.
/// It may be possible to represent the voting information itself as an intermediate stage
/// but that is unnecessary for the purposes of simple simulation.
pub trait VotingStrategy {
    /// Vote on a candidate with the given strategy and utility map.
    /// `&self` may contain parameters such as the ratio of strategic voters.
    fn vote(&self, utility_map: &UtilityMap) -> Candidate;

    fn regret(&self, utility_map: &UtilityMap) -> NotNan<f64> {
        utility_map.regret(self.vote(utility_map))
    }
}

/// A trait representing that a voting strategy has exactly one parameter, which is the ratio of
/// honest voters.
///
/// Maybe there is an easy way to make a derive-macro for this but I'll just do it manually for now.
///
/// Note that this trait is incompatible with support for any parameters other than the honest
/// ratio. It is completely possible to make a trait that supports other parameters by making them
/// an associated type, but this is not necessary for any of the types defined in this file.
pub trait MixedVotingStrategy: VotingStrategy {
    /// Create a voting system with the given ratio of honest voters; e.g. 1.0 for completely honest.
    fn new(honest_ratio: f64) -> Self;
}

/// A voting strategy where all voters vote honestly.
///
/// Usage: `HonestVotingStrategy::<T>::default().regret(&utility_map)`
#[derive(Copy, Clone, Debug)]
pub struct HonestVotingStrategy<T>(PhantomData<T>);

// Unfortunately, rust does not support deriving this particular Default instance.
impl<T> Default for HonestVotingStrategy<T> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T> VotingStrategy for HonestVotingStrategy<T>
where
    T: MixedVotingStrategy,
{
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        T::new(1.0).vote(utility_map)
    }
}

/// A voting strategy where all voters vote strategically. Here, "strategically" can mean
/// perfectly rational, or just something with better expected utility than honest voting,
/// as specified in the implementing type.
///
/// Usage: `StrategicVotingStrategy::<T>::default().regret(&utility_map)`
#[derive(Copy, Clone, Debug)]
pub struct StrategicVotingStrategy<T>(PhantomData<T>);

// Unfortunately, rust does not support deriving this particular Default instance.
impl<T> Default for StrategicVotingStrategy<T> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T> VotingStrategy for StrategicVotingStrategy<T>
where
    T: MixedVotingStrategy,
{
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        T::new(0.0).vote(utility_map)
    }
}

/// Winner is chosen randomly from all candidates. Useful as a frame of reference.
#[derive(Debug, Copy, Clone)]
pub struct RandomWinner;

impl VotingStrategy for RandomWinner {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        rand::random_range(0..utility_map.candidate_count())
    }
}

/// A random dictator is chosen among the voters, and that dictator chooses the winner.
/// This is somewhat better than random winner in that it takes voter opinions into account
/// in some way, but is of course very bad.
#[derive(Debug, Copy, Clone)]
pub struct RandomDictator;

impl VotingStrategy for RandomDictator {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let dictator = rand::random_range(0..utility_map.voter_count());
        (0..utility_map.candidate_count())
            .max_by_key(|&c| utility_map.utility(dictator, c))
            .expect("no candidates found")
    }
}

/// The candidate with the worst total utility is chosen. Useful as a frame of reference.
#[derive(Debug, Copy, Clone)]
pub struct WorstCandidate;

impl VotingStrategy for WorstCandidate {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        (0..utility_map.candidate_count())
            .min_by_key(|&c| utility_map.evaluate(c))
            .expect("no candidates found")
    }
}

/// In a range voting system, voters assign each candidate any number in [0.0, 1.0], and the
/// candidate with the highest average wins.
///
/// This particular "threshold" strategy is where strategic voters take the average of their
/// utility of the two frontrunners as a threshold, and give each candidate 0.0 if below the
/// threshold or 1.0 if above the threshold. This strategy is likely what real voters would use,
/// and is the same as optimal in almost all cases.
#[derive(Debug, Copy, Clone)]
pub struct RangeThreshold {
    honest_ratio: f64,
}

impl MixedVotingStrategy for RangeThreshold {
    fn new(honest_ratio: f64) -> Self {
        RangeThreshold { honest_ratio }
    }
}

impl VotingStrategy for RangeThreshold {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![NotNan::new(0.0).unwrap(); utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            // test using <= here since utility generation is symmetric by voter
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                for c in 0..utility_map.candidate_count() {
                    votes[c] += utility_map.utility(i, c);
                }
            } else {
                let threshold = (utility_map.utility(i, order[0])
                    + utility_map.utility(i, order[1]))
                    / NotNan::new(2.0).unwrap();
                for c in 0..utility_map.candidate_count() {
                    if utility_map.utility(i, c) >= threshold {
                        votes[c] += NotNan::new(1.0).unwrap();
                    }
                }
            }
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In a range voting system, voters assign each candidate any number in [0.0, 1.0], and the
/// candidate with the highest average wins.
///
/// This particular "moving average" strategy assigns 0.0 or 1.0 to each candidate in polling order
/// by comparing to the moving average.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct RangeMovingAverage {
    honest_ratio: f64,
}

impl MixedVotingStrategy for RangeMovingAverage {
    fn new(honest_ratio: f64) -> Self {
        RangeMovingAverage { honest_ratio }
    }
}

impl VotingStrategy for RangeMovingAverage {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![NotNan::new(0.0).unwrap(); utility_map.candidate_count()];
        for i in 0..utility_map.voter_count() {
            // test using <= here since utility generation is symmetric by voter
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                for c in 0..utility_map.candidate_count() {
                    votes[c] += utility_map.utility(i, c);
                }
            } else {
                for (c, b) in utility_map.moving_average_preference(i) {
                    votes[c] += NotNan::new(if b { 1.0 } else { 0.0 }).unwrap();
                }
            }
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In an approval voting system, voters assign each candidate 0 or 1, and the candidate with the
/// highest average wins.
///
/// This particular "threshold" strategy is where strategic voters take the average of their
/// utility of the two frontrunners as a threshold, and give each candidate 0.0 if below the
/// threshold or 1.0 if above the threshold. This strategy is likely what real voters would use,
/// and is the same as optimal in almost all cases. Honest voters use the average of all their
/// utilities as the threshold.
#[derive(Debug, Copy, Clone)]
pub struct ApprovalThreshold {
    honest_ratio: f64,
}

impl MixedVotingStrategy for ApprovalThreshold {
    fn new(honest_ratio: f64) -> Self {
        ApprovalThreshold { honest_ratio }
    }
}

impl VotingStrategy for ApprovalThreshold {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            // test using <= here since utility generation is symmetric by voter
            let threshold = if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                (0..utility_map.candidate_count())
                    .map(|c| utility_map.utility(i, c))
                    .sum::<NotNan<f64>>()
                    / NotNan::new(utility_map.candidate_count() as f64).unwrap()
            } else {
                (utility_map.utility(i, order[0]) + utility_map.utility(i, order[1]))
                    / NotNan::new(2.0).unwrap()
            };
            for c in 0..utility_map.candidate_count() {
                if utility_map.utility(i, c) >= threshold {
                    votes[c] += 1;
                }
            }
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In an approval voting system, voters assign each candidate 0 or 1, and the candidate with the
/// highest average wins.
///
/// This particular "moving average" strategy assigns 0 or 1 to each candidate in polling order by
/// comparing to the moving average.
/// Honest voters use the average of all their utilities as the threshold.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct ApprovalMovingAverage {
    honest_ratio: f64,
}

impl MixedVotingStrategy for ApprovalMovingAverage {
    fn new(honest_ratio: f64) -> Self {
        ApprovalMovingAverage { honest_ratio }
    }
}

impl VotingStrategy for ApprovalMovingAverage {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        for i in 0..utility_map.voter_count() {
            // test using <= here since utility generation is symmetric by voter
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                let threshold = (0..utility_map.candidate_count())
                    .map(|c| utility_map.utility(i, c))
                    .sum::<NotNan<f64>>()
                    / NotNan::new(utility_map.candidate_count() as f64).unwrap();
                for c in 0..utility_map.candidate_count() {
                    if utility_map.utility(i, c) >= threshold {
                        votes[c] += 1;
                    }
                }
            } else {
                for (c, b) in utility_map.moving_average_preference(i) {
                    votes[c] += b as usize;
                }
            };
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In a plurality voting system, each voter chooses one candidate, and the candidate with the most
/// votes wins.
///
/// Strategic voters vote for their favorite candidate among the two frontrunners.
/// Honest voters simply vote for their favorite candidate.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct Plurality {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Plurality {
    fn new(honest_ratio: f64) -> Self {
        Plurality { honest_ratio }
    }
}

impl VotingStrategy for Plurality {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                let c = (0..utility_map.candidate_count())
                    .max_by_key(|&c| utility_map.utility(i, c))
                    .expect("no candidates found");
                votes[c] += 1;
            } else {
                if utility_map.utility(i, order[0]) > utility_map.utility(i, order[1]) {
                    votes[order[0]] += 1;
                } else {
                    votes[order[1]] += 1;
                }
            }
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// As an attempt to reduce the effects of "spoilers", this system does a runoff between the
/// two most popular candidates.
///
/// This is not a COAF system, and the optimal strategy is complicated. In particular, there are
/// cases involving voters that support the same candidates voting in different ways at once,
/// and cases where the winner under strategic voting is indeterminate - see Niou 2001.
///
/// The strategy used here is simple rather than optimal, where strategic voters vote for one of the
/// two frontrunners as in standard plurality voting. This does not account for more sophisticated
/// methods that attempt to prevent a candidate from reaching the runoff.
#[derive(Debug, Copy, Clone)]
pub struct PluralityWithRunoff {
    honest_ratio: f64,
}

impl MixedVotingStrategy for PluralityWithRunoff {
    fn new(honest_ratio: f64) -> Self {
        PluralityWithRunoff { honest_ratio }
    }
}

impl VotingStrategy for PluralityWithRunoff {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                let c = (0..utility_map.candidate_count())
                    .max_by_key(|&c| utility_map.utility(i, c))
                    .expect("no candidates found");
                votes[c] += 1;
            } else {
                if utility_map.utility(i, order[0]) > utility_map.utility(i, order[1]) {
                    votes[order[0]] += 1;
                } else {
                    votes[order[1]] += 1;
                }
            }
        }
        let mut candidates: Vec<_> = (0..utility_map.candidate_count()).collect();
        candidates.sort_by_key(|&c| Reverse(votes[c]));
        let mut fst = 0;
        let mut snd = 0;
        for i in 0..utility_map.voter_count() {
            if utility_map.utility(i, candidates[0]) > utility_map.utility(i, candidates[1]) {
                fst += 1;
            } else {
                snd += 1;
            }
        }
        if fst >= snd {
            candidates[0]
        } else {
            candidates[1]
        }
    }
}

/// In a bullet voting system (under Smith's usage; terminology varies), each voter chooses one
/// candidate to vote against, and the candidate with the fewest votes wins.
///
/// Strategic voters vote against their least favorite candidate among the two frontrunners.
/// Honest voters simply vote for their least favorite candidate.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct Bullet {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Bullet {
    fn new(honest_ratio: f64) -> Self {
        Bullet { honest_ratio }
    }
}

impl VotingStrategy for Bullet {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            // the worst candidate, to be excluded
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                let c = (0..utility_map.candidate_count())
                    .min_by_key(|&c| utility_map.utility(i, c))
                    .expect("no candidates found");
                votes[c] += 1;
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    // if 0 is preferred, then vote *against* 1.
                    votes[order[1]] += 1;
                } else {
                    votes[order[0]] += 1;
                }
            };
        }
        (0..utility_map.candidate_count())
            .min_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In a Borda voting system, each voter ranks the candidates by order of preference, and each
/// candidate receives a corresponding linearly weighted vote, with the most liked candidate
/// receiving c - 1 and the least liked candidate receiving 0. The candidate with the highest
/// average wins.
///
/// Strategic voters use the "moving average" metric, notably ranking the frontrunners as most or
/// least preferred. Honest voters use honest ordering.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct Borda {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Borda {
    fn new(honest_ratio: f64) -> Self {
        Borda { honest_ratio }
    }
}

impl VotingStrategy for Borda {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                let (mut start, mut end) = (0, pref.len());
                for (c, b) in utility_map.moving_average_preference(i) {
                    if b {
                        pref[start] = c;
                        start += 1;
                    } else {
                        end -= 1;
                        pref[end] = c;
                    }
                }
            }
            for (x, &c) in pref.iter().rev().enumerate() {
                votes[c] += x;
            }
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In a Dabagh voting system, each voter ranks the candidates by order of preference, and each
/// candidate received a weighted vote, with the most liked candidate receiving 1, the next most
/// liked candidate receiving 0.5, and the rest receiving 0. (Equivalently, they receive 2, 1, and
/// 0 if integers are preferred.) The candidate with the highest average wins.
///
/// Strategic voters use the "moving average" metric, notably ranking the frontrunners as most or
/// least preferred. Honest voters use honest ordering.
///
/// This method of strategic voting is theoretically optimal.
#[derive(Debug, Copy, Clone)]
pub struct Dabagh {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Dabagh {
    fn new(honest_ratio: f64) -> Self {
        Dabagh { honest_ratio }
    }
}

impl VotingStrategy for Dabagh {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![0; utility_map.candidate_count()];
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                let (mut start, mut end) = (0, pref.len());
                for (c, b) in utility_map.moving_average_preference(i) {
                    if b {
                        pref[start] = c;
                        start += 1;
                    } else {
                        end -= 1;
                        pref[end] = c;
                    }
                }
            }
            votes[pref[0]] += 2;
            votes[pref[1]] += 1;
        }
        (0..utility_map.candidate_count())
            .max_by_key(|&c| votes[c])
            .expect("no candidates found")
    }
}

/// In Condorcet Least Reversal as described by Smith, each voter submits a rank ordering of all
/// the candidates, and the pairwise comparisons are reported. For each candidate, consider the
/// losing margins against all other candidates (ignoring the winning margins). The candidate with
/// the least sum of losing margins wins. This is the minimum weight of an edge set that need to be
/// reversed for there to be a unique pairwise winner.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct CondorcetLeastReversal {
    honest_ratio: f64,
}

impl MixedVotingStrategy for CondorcetLeastReversal {
    fn new(honest_ratio: f64) -> Self {
        CondorcetLeastReversal { honest_ratio }
    }
}

impl VotingStrategy for CondorcetLeastReversal {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // pairs[c][d] = how many voters rank c before d
        let mut pairs = vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            for c in 0..utility_map.candidate_count() {
                for d in c + 1..utility_map.candidate_count() {
                    // pref[c] comes before pref[d]
                    pairs[pref[c]][pref[d]] += 1;
                }
            }
        }

        // the sum of all *losing* margins that need to be reversed
        let mut margins = vec![0; utility_map.candidate_count()];
        for c in 0..utility_map.candidate_count() {
            for d in c + 1..utility_map.candidate_count() {
                if pairs[c][d] > pairs[d][c] {
                    // d loses by this much against c
                    margins[d] += pairs[c][d] - pairs[d][c];
                } else {
                    // c loses by this much against d
                    margins[c] += pairs[d][c] - pairs[c][d];
                }
            }
        }

        (0..utility_map.candidate_count())
            .min_by_key(|&c| margins[c])
            .expect("no candidates found")
    }
}

/// In Condorcet Ranked Pairs, each voter submits a rank ordering of all the candidates, and the
/// pairwise comparisons are reported. Consider each pair of candidates in order of largest to
/// smallest margin of victory. If the pair does not form a Condorcet cycle, then add an edge
/// between the two candidates; otherwise ignore it. At the end there will be a unique candidate
/// that wins all pairwise comparisons under consideration.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct CondorcetRankedPairs {
    honest_ratio: f64,
}

impl MixedVotingStrategy for CondorcetRankedPairs {
    fn new(honest_ratio: f64) -> Self {
        CondorcetRankedPairs { honest_ratio }
    }
}

impl VotingStrategy for CondorcetRankedPairs {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // pairs[c][d] = how many voters rank c before d
        let mut pairs = vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            for c in 0..utility_map.candidate_count() {
                for d in c + 1..utility_map.candidate_count() {
                    // pref[c] comes before pref[d]
                    pairs[pref[c]][pref[d]] += 1;
                }
            }
        }

        let mut pair_order: Vec<_> = (0..utility_map.candidate_count())
            .flat_map(|c| (0..utility_map.candidate_count()).map(move |d| (c, d)))
            .filter(|&(c, d)| pairs[c][d] > pairs[d][c])
            .collect();
        // smaller minus larger here so they're in decreasing order
        pair_order.sort_by_key(|&(c, d)| pairs[d][c] - pairs[c][d]);
        // graph[c] is the list of candidates that *win against* c,
        // out of the pairs considered so far.
        let mut graph: Vec<Vec<usize>> = vec![vec![]; utility_map.candidate_count()];
        for (c, d) in pair_order {
            // dfs to determine if c is already beaten by d
            let mut reachable = vec![false; utility_map.candidate_count()];
            let mut q = vec![c];
            while let Some(x) = q.pop() {
                if reachable[x] {
                    continue;
                }
                reachable[x] = true;
                for &y in &graph[x] {
                    if !reachable[y] {
                        q.push(y);
                    }
                }
            }
            if !reachable[d] {
                // c is not already beaten by d, so we can mark that c wins against d.
                graph[d].push(c);
            }
        }

        (0..utility_map.candidate_count())
            .filter(|&c| graph[c].is_empty())
            .next()
            .expect("algorithmic error, there should be a Condorcet winner")
    }
}

/// In Hare's Single Transferable Vote (or Instant Runoff Voting) system, each voter submits
/// a rank ordering of all the candidates, and the candidate with the fewest first-place rankings
/// is repeatedly removed until there is one left.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct SingleTransferableFewestFirst {
    honest_ratio: f64,
}

impl MixedVotingStrategy for SingleTransferableFewestFirst {
    fn new(honest_ratio: f64) -> Self {
        SingleTransferableFewestFirst { honest_ratio }
    }
}

impl VotingStrategy for SingleTransferableFewestFirst {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // For STV, the best we can do to make the representation efficient is to use a hashmap
        // counter to cap the storage at min(number of voters, number of distinct votes). This is
        // too much trouble and I will simply have a list of all the votes.
        let mut votes = vec![vec![]; utility_map.voter_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            votes[i] = pref;
        }

        let mut candidates: Vec<_> = (0..utility_map.candidate_count()).collect();

        while candidates.len() > 1 {
            let mut first_count = vec![0; utility_map.candidate_count()];
            for v in &votes {
                first_count[v[0]] += 1;
            }
            let c = candidates
                .iter()
                .copied()
                .min_by_key(|&c| first_count[c])
                .unwrap();
            candidates.retain(|&d| d != c);
            for v in &mut votes {
                v.retain(|&d| d != c);
            }
        }

        candidates[0]
    }
}

/// In Coombs's Single Transferable Vote (or Instant Runoff Voting) system, each voter submits
/// a rank ordering of all the candidates, and the candidate with the most last-place rankings
/// is repeatedly removed until there is one left.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct SingleTransferableMostLast {
    honest_ratio: f64,
}

impl MixedVotingStrategy for SingleTransferableMostLast {
    fn new(honest_ratio: f64) -> Self {
        SingleTransferableMostLast { honest_ratio }
    }
}

impl VotingStrategy for SingleTransferableMostLast {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // For STV, the best we can do to make the representation efficient is to use a hashmap
        // counter to cap the storage at min(number of voters, number of distinct votes). This is
        // too much trouble and I will simply have a list of all the votes.
        let mut votes = vec![vec![]; utility_map.voter_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                // negative here to put the best first
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            votes[i] = pref;
        }

        let mut candidates: Vec<_> = (0..utility_map.candidate_count()).collect();

        while candidates.len() > 1 {
            let mut last_count = vec![0; utility_map.candidate_count()];
            for v in &votes {
                last_count[*v.last().unwrap()] += 1;
            }
            let c = candidates
                .iter()
                .copied()
                .max_by_key(|&c| last_count[c])
                .unwrap();
            candidates.retain(|&d| d != c);
            for v in &mut votes {
                v.retain(|&d| d != c);
            }
        }

        candidates[0]
    }
}

/// In Copeland voting, each voter submits a rank ordering of all the candidates, and the
/// pairwise comparisons are reported. The candidate that wins against the greatest number of
/// other individual candidates (with ties counting as 1/2) wins the election.
///
/// Since ties are common no matter the size of the voter count, some tiebreaker is necessary.
/// Borda count is a common tiebreaker, and is what I use.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct Copeland {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Copeland {
    fn new(honest_ratio: f64) -> Self {
        Copeland { honest_ratio }
    }
}

impl VotingStrategy for Copeland {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // pairs[c][d] = how many voters rank c before d
        let mut pairs = vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let mut borda = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            for c in 0..utility_map.candidate_count() {
                for d in c + 1..utility_map.candidate_count() {
                    // pref[c] comes before pref[d]
                    pairs[pref[c]][pref[d]] += 1;
                }
            }
            // also update borda count here - note that you can't strategize against the Borda
            // count separately from the Copeland system.
            for (x, &c) in pref.iter().rev().enumerate() {
                borda[c] += x;
            }
        }

        let mut wins = vec![0; utility_map.candidate_count()];
        for c in 0..utility_map.candidate_count() {
            for d in 0..utility_map.candidate_count() {
                if pairs[c][d] > pairs[d][c] {
                    // 2 because doubled to let the tie case be 1.
                    wins[c] += 2;
                } else if pairs[c][d] == pairs[d][c] {
                    wins[c] += 1;
                }
            }
        }

        (0..utility_map.candidate_count())
            .max_by_key(|&c| (wins[c], borda[c]))
            .expect("no candidates found")
    }
}

/// In Black voting, each voter submits a rank ordering of all the candidates, and the
/// pairwise comparisons are reported. If there is a candidate that wins all pairwise elections
/// (i.e. a Condorcet winner) then that candidate wins. Otherwise, use Borda.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct Black {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Black {
    fn new(honest_ratio: f64) -> Self {
        Black { honest_ratio }
    }
}

impl VotingStrategy for Black {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // pairs[c][d] = how many voters rank c before d
        let mut pairs = vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let mut borda = vec![0; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            for c in 0..utility_map.candidate_count() {
                for d in c + 1..utility_map.candidate_count() {
                    // pref[c] comes before pref[d]
                    pairs[pref[c]][pref[d]] += 1;
                }
            }
            // also update borda count here - note that you can't strategize against the Borda
            // count separately from the Black system.
            // (rev to make the worst get 0)
            for (x, &c) in pref.iter().rev().enumerate() {
                borda[c] += x;
            }
        }

        // we only care if wins[c] == #candidates - 1
        let mut wins = vec![0; utility_map.candidate_count()];
        for c in 0..utility_map.candidate_count() {
            for d in 0..utility_map.candidate_count() {
                if pairs[c][d] > pairs[d][c] {
                    wins[c] += 1;
                }
            }
        }

        if let Some(c) = (0..utility_map.candidate_count())
            .filter(|&c| wins[c] == utility_map.candidate_count() - 1)
            .next()
        {
            c
        } else {
            (0..utility_map.candidate_count())
                .max_by_key(|&c| borda[c])
                .expect("no candidates found")
        }
    }
}

/// In Bucklin voting, each voter submits a rank ordering of all the candidates. If there is a
/// candidate c with more than half of the voters placing c in first, then c wins. If not, then
/// if there is a candidate c with more than half of the voters placing c in first or second, then
/// c wins. This repeats for ranks 1-k for each k. If there are ties, they are broken by the count
/// of voters placing in ranks 1-k.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters place their
/// favorite of the two frontrunners as most preferred and the other as least preferred, and order
/// the rest honestly. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct Bucklin {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Bucklin {
    fn new(honest_ratio: f64) -> Self {
        Bucklin { honest_ratio }
    }
}

impl VotingStrategy for Bucklin {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        // rank_counts[c][k] = how many voters place c in rank <= k ("first" being rank 0).
        let mut rank_counts =
            vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let order = utility_map.polling_order();
        for i in 0..utility_map.voter_count() {
            let mut pref: Vec<_> = (0..utility_map.candidate_count()).collect();
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                pref.sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            } else {
                if utility_map.utility(i, order[0]) >= utility_map.utility(i, order[1]) {
                    pref[0] = order[0];
                    pref[utility_map.candidate_count() - 1] = order[1];
                } else {
                    pref[utility_map.candidate_count() - 1] = order[0];
                    pref[0] = order[1];
                }
                // all candidates that aren't the frontrunners...
                pref[1..utility_map.candidate_count() - 1].copy_from_slice(&order[2..]);
                // ...are sorted honestly.
                pref[1..utility_map.candidate_count() - 1]
                    .sort_by_key(|&c| Reverse(utility_map.utility(i, c)));
            }
            for k in 0..utility_map.candidate_count() {
                for r in 0..=k {
                    rank_counts[pref[r]][k] += 1;
                }
            }
        }

        (0..utility_map.candidate_count())
            .filter_map(|k| {
                (0..utility_map.candidate_count())
                    .filter(|&c| rank_counts[c][k] * 2 > utility_map.voter_count())
                    .max_by_key(|&c| rank_counts[c][k])
            })
            .next()
            .expect("no candidates found")
    }
}

/// In Star voting, each voter submits a rating of all the candidates. Then, the two candidates
/// with the highest rating (call them c1 and c2) are put in a runoff. If more voters place c1
/// before c2, then c1 wins, otherwise c2 wins.
///
/// Wikipedia specifies that the ratings are integers in [0, 5], but I instead just use
/// reals in [0.0, 0.1] as with standard range voting.
///
/// This is not a COAF system and optimal voting is complicated. Here, strategic voters rate the
/// candidates as 0.0 or 1.0 using the midpoint between the utilities of the two frontrunners
/// as a threshold. Honest voters use honest ordering.
#[derive(Debug, Copy, Clone)]
pub struct Star {
    honest_ratio: f64,
}

impl MixedVotingStrategy for Star {
    fn new(honest_ratio: f64) -> Self {
        Star { honest_ratio }
    }
}

impl VotingStrategy for Star {
    fn vote(&self, utility_map: &UtilityMap) -> Candidate {
        let mut votes = vec![NotNan::new(0.0).unwrap(); utility_map.candidate_count()];
        // pairs[i][j] = how many voters place i higher than j
        let mut pairs = vec![vec![0; utility_map.candidate_count()]; utility_map.candidate_count()];
        let order = utility_map.polling_order();

        for i in 0..utility_map.voter_count() {
            // test using <= here since utility generation is symmetric by voter
            if i as f64 <= utility_map.voter_count() as f64 * self.honest_ratio {
                for c in 0..utility_map.candidate_count() {
                    votes[c] += utility_map.utility(i, c);
                }

                for c in 0..utility_map.candidate_count() {
                    for d in 0..utility_map.candidate_count() {
                        if utility_map.utility(i, c) >= utility_map.utility(i, d) {
                            pairs[c][d] += 1;
                        } else {
                            pairs[d][c] += 1;
                        }
                    }
                }
            } else {
                let threshold = (utility_map.utility(i, order[0])
                    + utility_map.utility(i, order[1]))
                    / NotNan::new(2.0).unwrap();
                for c in 0..utility_map.candidate_count() {
                    if utility_map.utility(i, c) >= threshold {
                        votes[c] += NotNan::new(1.0).unwrap();
                    }
                }

                for c in 0..utility_map.candidate_count() {
                    for d in 0..utility_map.candidate_count() {
                        if utility_map.utility(i, c) >= threshold
                            && threshold >= utility_map.utility(i, d)
                        {
                            // voted as (1.0, 0.0)
                            pairs[c][d] += 1;
                        } else if utility_map.utility(i, d) >= threshold
                            && threshold >= utility_map.utility(i, c)
                        {
                            // voted as (0.0, 1.0)
                            pairs[d][c] += 1;
                        } // else voted as (0.0, 0.0) or (1.0, 1.0)
                    }
                }
            }
        }

        let mut candidates: Vec<_> = (0..utility_map.candidate_count()).collect();
        candidates.sort_by_key(|&c| Reverse(votes[c]));
        let (c, d) = (candidates[0], candidates[1]);
        if pairs[c][d] > pairs[d][c] {
            c
        } else {
            d
        }
    }
}
