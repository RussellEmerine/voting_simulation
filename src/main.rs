mod distr;
mod utility_map;
mod voting_strategy;

use crate::distr::{Bimodal, Truncate};
use crate::utility_map::UtilityMap;
use crate::voting_strategy::*;
use plotters::coord::ranged1d::SegmentedCoord;
use plotters::coord::types::{RangedCoordf32, RangedSlice};
use plotters::prelude::*;
use rand::prelude::Distribution;
use rand::rng;
use serde::Serialize;
use serde_json;
use std::fs;
use std::fs::File;

/// A representation of the data, specifically for JSON output.
#[derive(Clone, Debug, Serialize)]
struct Data {
    /// The quartiles, specifically minimum, first, quartile, median, third quartile, and maximum.
    ///
    /// For some reason, the `plotters` library stores the values as `f64` but only provides
    /// the method to access the quartiles with return value of type `[f32; 5]`. Strange.
    quartiles: [f32; 5],
    /// The mean, using `f32` for consistency with the quartiles.
    mean: f32,
}

/// A representation of the data with a label, specifically for JSON output.
/// We use a list of these instead of a map of label to data so that the output
/// is kept in order.
#[derive(Clone, Debug, Serialize)]
struct LabeledData {
    label: String,
    data: Data,
}

/// plot results some voting strategy
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
    scaled: bool,
) -> LabeledData {
    let regrets: Vec<_> = (0..trials)
        .map(|_| {
            let map = utility_map_fn(rng);
            if scaled {
                let c = voting_strategy.vote(&map.clone().scale_voter_utilities());
                map.regret(c) / map.voter_count() as f64
            } else {
                voting_strategy.regret(&map) / map.voter_count() as f64
            }
        })
        .collect();

    let quartiles = Quartiles::new(&regrets);
    let mean = regrets.iter().sum::<f64>() as f32 / regrets.len() as f32;

    chart
        .draw_series(vec![Boxplot::new_horizontal(
            SegmentValue::CenterOf(label),
            &quartiles,
        )])
        .unwrap();

    LabeledData {
        label: label.clone(),
        data: Data {
            quartiles: quartiles.values(),
            mean,
        },
    }
}

fn plot<Rng: rand::Rng>(
    path: &str,
    rng: &mut Rng,
    utility_map_fn: impl Fn(&mut Rng) -> UtilityMap,
    labeled_strategies: Vec<(String, Box<dyn VotingStrategy>)>,
    trials: usize,
    scaled: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // reverse to put the first at the top
    let (labels, strategies): (Vec<_>, Vec<_>) = labeled_strategies.into_iter().rev().unzip();

    let plot_path = path.to_string() + ".png";
    let json_path = path.to_string() + ".json";

    let root =
        BitMapBackend::new(&plot_path, (1000, 100 * labels.len() as u32)).into_drawing_area();
    root.fill(&WHITE)?;
    let root = root.margin(5, 5, 5, 5);
    let mut chart = ChartBuilder::on(&root)
        .x_label_area_size(50)
        .y_label_area_size(250)
        .caption("Regret under various voting systems", ("sans-serif", 20))
        .build_cartesian_2d(0.0_f32..0.2, labels[..].into_segmented())?;

    chart
        .configure_mesh()
        .x_desc("Regret")
        .y_desc("Voting system")
        .disable_y_mesh()
        .axis_style(WHITE)
        .light_line_style(WHITE)
        .draw()?;

    let mut data = vec![];

    for (label, strategy) in labels.iter().zip(strategies.iter()) {
        data.push(plot_voting_strategy(
            &mut chart,
            label,
            rng,
            &utility_map_fn,
            strategy,
            trials,
            scaled,
        ));
    }

    // To avoid the IO failure being ignored silently, we manually call the present function
    root.present().expect(
        "Unable to write result to file, please make sure 'images' dir exists under current dir",
    );

    let f = File::create(json_path)?;
    serde_json::to_writer_pretty(f, &data)?;

    println!("Result has been saved to {}{{.png,.json}}", path);

    Ok(())
}

fn plot_utility_map_fn<Rng: rand::Rng>(
    dir: &str,
    rng: &mut Rng,
    utility_map_fn: impl Fn(&mut Rng) -> UtilityMap,
    trials: usize,
    scaled: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all(format!("images/{}/", dir))?;

    plot(
        &format!("images/{}/frame_of_reference", dir),
        rng,
        &utility_map_fn,
        vec![
            ("Random winner".to_string(), Box::new(RandomWinner)),
            ("Random dictator".to_string(), Box::new(RandomDictator)),
            ("Worst candidate".to_string(), Box::new(WorstCandidate)),
        ],
        trials,
        scaled,
    )?;

    plot(
        &format!("images/{}/honest_coaf", dir),
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
        scaled,
    )?;

    plot(
        &format!("images/{}/honest_non_coaf", dir),
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
        scaled,
    )?;

    for honest_ratio in [0.25, 0.5, 0.75] {
        plot(
            &format!("images/{}/ratio_{:.2}_coaf", dir, honest_ratio),
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
            scaled,
        )?;

        plot(
            &format!("images/{}/ratio_{:.2}_non_coaf", dir, honest_ratio),
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
            scaled,
        )?;
    }

    plot(
        &format!("images/{}/strategic_coaf", dir),
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
        scaled,
    )?;

    plot(
        &format!("images/{}/strategic_non_coaf", dir),
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
        scaled,
    )?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plot_utility_map_fn(
        "standard_uniform",
        &mut rng(),
        |rng| UtilityMap::random(100, 5, rng, rand_distr::StandardUniform),
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "standard_uniform_scaled",
        &mut rng(),
        |rng| UtilityMap::random(100, 5, rng, rand_distr::StandardUniform),
        10_000,
        true,
    )?;

    plot_utility_map_fn(
        "normal",
        &mut rng(),
        |rng| {
            UtilityMap::random(
                100,
                5,
                rng,
                Truncate::new(rand_distr::Normal::new(0.5, 0.1).unwrap(), 0.0..1.0),
            )
        },
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "normal_scaled",
        &mut rng(),
        |rng| {
            UtilityMap::random(
                100,
                5,
                rng,
                Truncate::new(rand_distr::Normal::new(0.5, 0.1).unwrap(), 0.0..1.0),
            )
        },
        10_000,
        true,
    )?;

    plot_utility_map_fn(
        "bimodal",
        &mut rng(),
        |rng| {
            UtilityMap::random(
                100,
                5,
                rng,
                Truncate::new(
                    Bimodal::new(
                        rand_distr::Bernoulli::new(0.5).unwrap(),
                        rand_distr::Normal::new(0.25, 0.05).unwrap(),
                        rand_distr::Normal::new(0.75, 0.05).unwrap(),
                    ),
                    0.0..1.0,
                ),
            )
        },
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "bimodal_scaled",
        &mut rng(),
        |rng| {
            UtilityMap::random(
                100,
                5,
                rng,
                Truncate::new(
                    Bimodal::new(
                        rand_distr::Bernoulli::new(0.5).unwrap(),
                        rand_distr::Normal::new(0.25, 0.05).unwrap(),
                        rand_distr::Normal::new(0.75, 0.05).unwrap(),
                    ),
                    0.0..1.0,
                ),
            )
        },
        10_000,
        true,
    )?;

    plot_utility_map_fn(
        "issue_based_2",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                ],
            )
        },
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "issue_based_2_scaled",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                    Box::new(|rng| rand_distr::Uniform::new(-1.0, 1.0).unwrap().sample(rng)),
                ],
            )
        },
        10_000,
        true,
    )?;

    plot_utility_map_fn(
        "issue_based_2_normal",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| {
                        Truncate::new(rand_distr::Normal::new(0.0, 0.1).unwrap(), -1.0..1.0)
                            .sample(rng)
                    }),
                    Box::new(|rng| {
                        Truncate::new(rand_distr::Normal::new(0.0, 0.05).unwrap(), -1.0..1.0)
                            .sample(rng)
                    }),
                ],
            )
        },
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "issue_based_2_normal_scaled",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| {
                        Truncate::new(rand_distr::Normal::new(0.0, 0.1).unwrap(), -1.0..1.0)
                            .sample(rng)
                    }),
                    Box::new(|rng| {
                        Truncate::new(rand_distr::Normal::new(0.0, 0.05).unwrap(), -1.0..1.0)
                            .sample(rng)
                    }),
                ],
            )
        },
        10_000,
        true,
    )?;

    plot_utility_map_fn(
        "issue_based_2_bimodal",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| {
                        Truncate::new(
                            Bimodal::new(
                                rand_distr::Bernoulli::new(0.5).unwrap(),
                                rand_distr::Normal::new(-0.5, 0.05).unwrap(),
                                rand_distr::Normal::new(0.5, 0.05).unwrap(),
                            ),
                            -1.0..1.0,
                        )
                        .sample(rng)
                    }),
                    Box::new(|rng| {
                        Truncate::new(
                            Bimodal::new(
                                rand_distr::Bernoulli::new(0.5).unwrap(),
                                rand_distr::Normal::new(-0.5, 0.05).unwrap(),
                                rand_distr::Normal::new(0.5, 0.05).unwrap(),
                            ),
                            -1.0..1.0,
                        )
                        .sample(rng)
                    }),
                ],
            )
        },
        10_000,
        false,
    )?;

    plot_utility_map_fn(
        "issue_based_2_bimodal",
        &mut rng(),
        |rng| {
            UtilityMap::random_issue_based(
                100,
                5,
                rng,
                &[
                    Box::new(|rng| {
                        Truncate::new(
                            Bimodal::new(
                                rand_distr::Bernoulli::new(0.5).unwrap(),
                                rand_distr::Normal::new(-0.5, 0.05).unwrap(),
                                rand_distr::Normal::new(0.5, 0.05).unwrap(),
                            ),
                            -1.0..1.0,
                        )
                        .sample(rng)
                    }),
                    Box::new(|rng| {
                        Truncate::new(
                            Bimodal::new(
                                rand_distr::Bernoulli::new(0.5).unwrap(),
                                rand_distr::Normal::new(-0.5, 0.05).unwrap(),
                                rand_distr::Normal::new(0.5, 0.05).unwrap(),
                            ),
                            -1.0..1.0,
                        )
                        .sample(rng)
                    }),
                ],
            )
        },
        10_000,
        true,
    )?;

    Ok(())
}
