mod distr;
mod utility_map;
mod voting_strategy;

use crate::utility_map::UtilityMap;
use crate::voting_strategy::*;
use plotters::coord::ranged1d::SegmentedCoord;
use plotters::coord::types::{RangedCoordf32, RangedSlice};
use plotters::prelude::*;
use rand::prelude::Distribution;
use rand::rng;

/// plot results some voting strategy
///
/// TODO: figure out better type for voting_strategy
///
/// TODO: more sophisticated parameters
///
/// TODO: also give a way to generate utility distributions
fn plot_voting_strategy<'a, Rng: rand::Rng>(
    chart: &mut ChartContext<
        impl DrawingBackend,
        Cartesian2d<RangedCoordf32, SegmentedCoord<RangedSlice<'a, String>>>,
    >,
    label: &'a String,
    rng: &mut Rng,
    utility_map_fn: impl Fn(&mut Rng) -> UtilityMap,
    voting_strategy: &Box<dyn VotingStrategy>,
    trials: usize,
) {
    let regrets: Vec<_> = (0..trials)
        .map(|_| {
            let map = utility_map_fn(rng);
            voting_strategy.regret(&map) / map.voter_count() as f64
        })
        .collect();

    chart
        .draw_series(vec![Boxplot::new_horizontal(
            SegmentValue::CenterOf(label),
            &Quartiles::new(&regrets),
        )])
        .unwrap();
}

fn plot<Rng: rand::Rng>(
    path: &str,
    rng: &mut Rng,
    utility_map_fn: impl Fn(&mut Rng) -> UtilityMap,
    labeled_strategies: Vec<(String, Box<dyn VotingStrategy>)>,
    trials: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    // reverse to put the first at the top
    let (labels, strategies): (Vec<_>, Vec<_>) = labeled_strategies.into_iter().rev().unzip();

    let root = SVGBackend::new(path, (1000, 100 * labels.len() as u32)).into_drawing_area();
    root.fill(&WHITE)?;
    let root = root.margin(5, 5, 5, 5);
    let mut chart = ChartBuilder::on(&root)
        .x_label_area_size(50)
        .y_label_area_size(250)
        .caption("Regret under various voting systems", ("sans-serif", 20))
        .build_cartesian_2d(0.0_f32..0.3, labels[..].into_segmented())?;

    chart
        .configure_mesh()
        .x_desc("Regret")
        .y_desc("Voting system")
        .disable_y_mesh()
        .axis_style(WHITE)
        .light_line_style(WHITE)
        .draw()?;

    for (label, strategy) in labels.iter().zip(strategies.iter()) {
        plot_voting_strategy(&mut chart, label, rng, &utility_map_fn, strategy, trials);
    }

    // To avoid the IO failure being ignored silently, we manually call the present function
    root.present().expect(
        "Unable to write result to file, please make sure 'images' dir exists under current dir",
    );

    println!("Result has been saved to {}", path);

    Ok(())
}

fn plot_utility_map_fn<Rng: rand::Rng>(
    dir: &str,
    rng: &mut Rng,
    utility_map_fn: impl Fn(&mut Rng) -> UtilityMap,
    trials: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    plot(
        &format!("images/{}/frame_of_reference.png", dir),
        rng,
        &utility_map_fn,
        vec![
            ("Random winner".to_string(), Box::new(RandomWinner)),
            ("Random dictator".to_string(), Box::new(RandomDictator)),
            ("Worst candidate".to_string(), Box::new(WorstCandidate)),
        ],
        trials,
    )?;

    plot(
        &format!("images/{}/honest_coaf.png", dir),
        rng,
        &utility_map_fn,
        vec![
            (
                "Range".to_string(),
                Box::new(<HonestVotingStrategy<RangeThreshold>>::default()),
            ),
            (
                "Approval".to_string(),
                Box::new(<HonestVotingStrategy<ApprovalThreshold>>::default()),
            ),
            (
                "Plurality".to_string(),
                Box::new(<HonestVotingStrategy<Plurality>>::default()),
            ),
            (
                "Bullet".to_string(),
                Box::new(<HonestVotingStrategy<Bullet>>::default()),
            ),
            (
                "Borda".to_string(),
                Box::new(<HonestVotingStrategy<Borda>>::default()),
            ),
            (
                "Dabagh".to_string(),
                Box::new(<HonestVotingStrategy<Dabagh>>::default()),
            ),
        ],
        trials,
    )?;

    plot(
        &format!("images/{}/honest_non_coaf.png", dir),
        rng,
        &utility_map_fn,
        vec![
            (
                "Plurality + runoff".to_string(),
                Box::new(<HonestVotingStrategy<PluralityWithRunoff>>::default()),
            ),
            (
                "Condorcet Least Reversal".to_string(),
                Box::new(<HonestVotingStrategy<CondorcetLeastReversal>>::default()),
            ),
            (
                "Condorcet Ranked Pairs".to_string(),
                Box::new(<HonestVotingStrategy<CondorcetRankedPairs>>::default()),
            ),
            (
                "Single Transferable Fewest First".to_string(),
                Box::new(<HonestVotingStrategy<SingleTransferableFewestFirst>>::default()),
            ),
            (
                "Single Transferable Most Last".to_string(),
                Box::new(<HonestVotingStrategy<SingleTransferableMostLast>>::default()),
            ),
            (
                "Copeland".to_string(),
                Box::new(<HonestVotingStrategy<Copeland>>::default()),
            ),
            (
                "Black".to_string(),
                Box::new(<HonestVotingStrategy<Black>>::default()),
            ),
            (
                "Bucklin".to_string(),
                Box::new(<HonestVotingStrategy<Bucklin>>::default()),
            ),
            (
                "Star".to_string(),
                Box::new(<HonestVotingStrategy<Star>>::default()),
            ),
        ],
        trials,
    )?;

    for honest_ratio in [0.25, 0.5, 0.75] {
        plot(
            &format!("images/{}/ratio_{:.2}_coaf.png", dir, honest_ratio),
            rng,
            &utility_map_fn,
            vec![
                (
                    "Range".to_string(),
                    Box::new(RangeMovingAverage::new(honest_ratio)),
                ),
                (
                    "Approval".to_string(),
                    Box::new(ApprovalMovingAverage::new(honest_ratio)),
                ),
                (
                    "Plurality".to_string(),
                    Box::new(Plurality::new(honest_ratio)),
                ),
                ("Bullet".to_string(), Box::new(Bullet::new(honest_ratio))),
                ("Borda".to_string(), Box::new(Borda::new(honest_ratio))),
                ("Dabagh".to_string(), Box::new(Dabagh::new(honest_ratio))),
            ],
            trials,
        )?;

        plot(
            &format!("images/{}/ratio_{:.2}_non_coaf.png", dir, honest_ratio),
            rng,
            &utility_map_fn,
            vec![
                (
                    "Plurality + runoff".to_string(),
                    Box::new(PluralityWithRunoff::new(honest_ratio)),
                ),
                (
                    "Condorcet Least Reversal".to_string(),
                    Box::new(CondorcetLeastReversal::new(honest_ratio)),
                ),
                (
                    "Condorcet Ranked Pairs".to_string(),
                    Box::new(CondorcetRankedPairs::new(honest_ratio)),
                ),
                (
                    "Single Transferable Fewest First".to_string(),
                    Box::new(SingleTransferableFewestFirst::new(honest_ratio)),
                ),
                (
                    "Single Transferable Most Last".to_string(),
                    Box::new(SingleTransferableMostLast::new(honest_ratio)),
                ),
                (
                    "Copeland".to_string(),
                    Box::new(Copeland::new(honest_ratio)),
                ),
                ("Black".to_string(), Box::new(Black::new(honest_ratio))),
                ("Bucklin".to_string(), Box::new(Bucklin::new(honest_ratio))),
                ("Star".to_string(), Box::new(Star::new(honest_ratio))),
            ],
            trials,
        )?;
    }

    plot(
        &format!("images/{}/strategic_coaf.png", dir),
        rng,
        &utility_map_fn,
        vec![
            (
                "Range (threshold)".to_string(),
                Box::new(<StrategicVotingStrategy<RangeThreshold>>::default()),
            ),
            (
                "Range (moving avg)".to_string(),
                Box::new(<StrategicVotingStrategy<RangeMovingAverage>>::default()),
            ),
            (
                "Plurality".to_string(),
                Box::new(<StrategicVotingStrategy<Plurality>>::default()),
            ),
            (
                "Bullet".to_string(),
                Box::new(<StrategicVotingStrategy<Bullet>>::default()),
            ),
            (
                "Borda".to_string(),
                Box::new(<StrategicVotingStrategy<Borda>>::default()),
            ),
            (
                "Dabagh".to_string(),
                Box::new(<StrategicVotingStrategy<Dabagh>>::default()),
            ),
        ],
        trials,
    )?;

    plot(
        &format!("images/{}/strategic_non_coaf.png", dir),
        rng,
        &utility_map_fn,
        vec![
            (
                "Plurality + runoff".to_string(),
                Box::new(<StrategicVotingStrategy<PluralityWithRunoff>>::default()),
            ),
            (
                "Condorcet Least Reversal".to_string(),
                Box::new(<StrategicVotingStrategy<CondorcetLeastReversal>>::default()),
            ),
            (
                "Condorcet Ranked Pairs".to_string(),
                Box::new(<StrategicVotingStrategy<CondorcetRankedPairs>>::default()),
            ),
            (
                "Single Transferable Fewest First".to_string(),
                Box::new(<StrategicVotingStrategy<SingleTransferableFewestFirst>>::default()),
            ),
            (
                "Single Transferable Most Last".to_string(),
                Box::new(<StrategicVotingStrategy<SingleTransferableMostLast>>::default()),
            ),
            (
                "Copeland".to_string(),
                Box::new(<StrategicVotingStrategy<Copeland>>::default()),
            ),
            (
                "Black".to_string(),
                Box::new(<StrategicVotingStrategy<Black>>::default()),
            ),
            (
                "Bucklin".to_string(),
                Box::new(<StrategicVotingStrategy<Bucklin>>::default()),
            ),
            (
                "Star".to_string(),
                Box::new(<StrategicVotingStrategy<Star>>::default()),
            ),
        ],
        trials,
    )?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plot_utility_map_fn(
        "standard_uniform",
        &mut rng(),
        |rng| UtilityMap::random(20, 5, rng, rand_distr::StandardUniform),
        1_000,
    )?;

    plot_utility_map_fn(
        "issue_based_2",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                20,
                5,
                rng,
                &[
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                ],
            )
        },
        1_000,
    )?;

    Ok(())
}
