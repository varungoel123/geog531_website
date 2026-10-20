# Foundations of inference — Quantitative Methods in Geography
# All data are fictional. Run from the top, in a clean R session.
# Install once if needed:
# install.packages(c("tidyverse", "infer"))
library(tidyverse)
library(infer)

# Your instructor assigns everyone a different seat number: 1, 2, 3, ...
# Replace 1 with YOUR assigned number. Record it to reproduce your work.
seat_number <- 1
student_seed <- 1000 + seat_number
# Re-running a block with the same seed repeats the same random results.
# More replicates in generate() produce multiple simulations in one run.

# 1. SAMPLING VARIABILITY: known toy city, independent draws
city <- tibble(minutes = seq(5, 30, by = 5))
mean(city$minutes) # known population mean = 17.5

# FIRST: inspect one sample and contribute one mean to the whiteboard.
set.seed(student_seed)
my_sample <- city |>
  rep_sample_n(size = 5, reps = 1, replace = TRUE)
my_sample # five draws; a value may appear more than once
my_sample |>
  summarise(sample_mean = mean(minutes), .groups = "drop")

# THEN: produce many samples to see the pattern.
set.seed(student_seed + 1)
sampling_5 <- city |>
  rep_sample_n(size = 5, reps = 2000, replace = TRUE) |>
  summarise(stat = mean(minutes), .groups = "drop")
sampling_20 <- city |>
  rep_sample_n(size = 20, reps = 2000, replace = TRUE) |>
  summarise(stat = mean(minutes), .groups = "drop")

sampling_comparison <- bind_rows(
  mutate(sampling_5, sample_size = "n = 5"),
  mutate(sampling_20, sample_size = "n = 20")
)
ggplot(sampling_comparison, aes(stat)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#06796f", color = "white") +
  geom_vline(xintercept = 17.5, linetype = "dashed") +
  facet_wrap(~ sample_size, ncol = 1) +
  labs(x = "Sample mean commute time (minutes)", y = "Samples") +
  theme_minimal(base_size = 14)

sampling_comparison |>
  group_by(sample_size) |>
  summarise(center = mean(stat), estimated_se = sd(stat), .groups = "drop")

# 2. SHUFFLING: completely randomized experiment, 10 plots per group
temperature <- tibble(
  plot_id = seq_len(20),
  treatment = rep(c("shade", "sun"), each = 10),
  temp_c = c(28, 30, 31, 32, 33, 34, 35, 36, 38, 39,
             32, 34, 35, 36, 37, 38, 39, 40, 42, 43)
)
temperature |>
  group_by(treatment) |>
  summarise(n = n(), mean_c = mean(temp_c), .groups = "drop")

# Always subtract in the stated order: shade minus sun.
observed <- temperature |>
  specify(temp_c ~ treatment) |>
  calculate(stat = "diff in means", order = c("shade", "sun"))
observed # -4 degrees C

# One shuffle: inspect the new assignment of observed temperatures.
set.seed(student_seed + 10)
one_shuffle <- temperature |>
  specify(temp_c ~ treatment) |>
  hypothesize(null = "independence") |>
  generate(reps = 1, type = "permute")
one_shuffle

# Compare original and shuffled temperatures at each fixed group slot.
tibble(
  slot = seq_len(nrow(temperature)),
  treatment = temperature$treatment,
  original_temp = temperature$temp_c,
  shuffled_temp = one_shuffle$temp_c
)
one_shuffle |>
  group_by(treatment) |>
  summarise(n = n(), mean_c = mean(temp_c), .groups = "drop")

# Report THIS statistic as one dot on the whiteboard.
one_shuffle |>
  calculate(stat = "diff in means", order = c("shade", "sun"))

# Three more shuffles: each row below is one potential whiteboard dot.
set.seed(student_seed + 11)
my_three_shuffles <- temperature |>
  specify(temp_c ~ treatment) |>
  hypothesize(null = "independence") |>
  generate(reps = 3, type = "permute") |>
  calculate(stat = "diff in means", order = c("shade", "sun"))
my_three_shuffles

# 10,000 shuffles approximate the randomization distribution.
# Design justification: the sharp null of no shade effect on any plot.
set.seed(student_seed + 12)
null_dist <- temperature |>
  specify(temp_c ~ treatment) |>
  hypothesize(null = "independence") |>
  generate(reps = 10000, type = "permute") |>
  calculate(stat = "diff in means", order = c("shade", "sun"))

visualize(null_dist) +
  shade_p_value(obs_stat = observed, direction = "both") +
  labs(x = "Difference in mean temperature: shade minus sun (degrees C)")
null_dist |>
  get_p_value(obs_stat = observed, direction = "both")

# Manual two-tail counting, to match the classroom rule |D| >= 4.
# The exact null distribution here is symmetric. infer doubles a tail
# for its two-sided p-value, so finite-simulation answers can differ slightly.
tail_count <- sum(abs(null_dist$stat) >= abs(observed$stat) - 1e-10)
tibble(extreme = tail_count, simulations = nrow(null_dist),
       tail_fraction = tail_count / nrow(null_dist),
       corrected_mc_p = (tail_count + 1) / (nrow(null_dist) + 1))
# The +1 convention prevents reporting a zero Monte Carlo p-value.

# 3. BOOTSTRAPPING: a separate small random survey from a large population
commute <- tibble(
  respondent = seq_len(8),
  minutes = c(10, 15, 15, 20, 25, 30, 35, 50)
)
sample_mean <- commute |>
  specify(response = minutes) |>
  calculate(stat = "mean")
sample_mean # 25 minutes

# One bootstrap sample has eight rows. People can appear more than once.
set.seed(student_seed + 20)
one_bootstrap <- commute |>
  specify(response = minutes) |>
  generate(reps = 1, type = "bootstrap")
one_bootstrap # inspect the eight drawn values
one_bootstrap |>
  count(minutes, name = "appearances") # repeated values are allowed

# Report THIS statistic as one dot on the whiteboard.
one_bootstrap |> calculate(stat = "mean")

# Three more resamples: each row is a mean from eight draws.
set.seed(student_seed + 21)
my_three_bootstraps <- commute |>
  specify(response = minutes) |>
  generate(reps = 3, type = "bootstrap") |>
  calculate(stat = "mean")
my_three_bootstraps

set.seed(student_seed + 22)
boot_dist <- commute |>
  specify(response = minutes) |>
  generate(reps = 10000, type = "bootstrap") |>
  calculate(stat = "mean")

ci_95 <- boot_dist |>
  get_confidence_interval(level = 0.95, type = "percentile")
ci_95
visualize(boot_dist) +
  shade_confidence_interval(endpoints = ci_95) +
  geom_vline(xintercept = sample_mean$stat, linetype = "dashed") +
  labs(x = "Bootstrap mean commute time (minutes)")

# An estimate of the standard error of the sample mean:
boot_dist |> summarise(bootstrap_se = sd(stat))

# Same data and resamples, a different confidence level:
ci_90 <- boot_dist |>
  get_confidence_interval(level = 0.90, type = "percentile")
ci_99 <- boot_dist |>
  get_confidence_interval(level = 0.99, type = "percentile")
bind_rows(`90%` = ci_90, `95%` = ci_95, `99%` = ci_99, .id = "level") |>
  mutate(width = upper_ci - lower_ci)

# Teaching caution: n = 8 is small and skewed. This percentile interval
# is approximate; bootstrapping does not guarantee nominal 95% coverage.
# It does not correct a biased sample or account for spatial dependence.

# OPTIONAL: exact check of the shuffle (184,756 assignments).
# Run explicitly if desired. Distinct plot IDs matter even for equal values.
exact_randomization <- function() {
  choices <- combn(seq_len(nrow(temperature)), 10)
  first_sum <- colSums(matrix(temperature$temp_c[choices], nrow = 10))
  exact_stats <- (2 * first_sum - sum(temperature$temp_c)) / 10
  tibble(assignments = length(exact_stats),
         exact_p = mean(abs(exact_stats) >= abs(observed$stat) - 1e-10))
}
# exact_randomization()

# OPTIONAL: approximate bootstrap coverage across genuinely new samples.
# This function is defined, but not run automatically.
# Each repetition starts with a new sample from a known normal population.
bootstrap_coverage <- function(repetitions = 100, n = 30, reps = 500) {
  set.seed(535)
  intervals <- map_dfr(seq_len(repetitions), function(i) {
    new_sample <- tibble(minutes = rnorm(n, mean = 25, sd = 5))
    limits <- new_sample |>
      specify(response = minutes) |>
      generate(reps = reps, type = "bootstrap") |>
      calculate(stat = "mean") |>
      get_confidence_interval(level = 0.95, type = "percentile")
    mutate(limits, sample_number = i, covers = lower_ci <= 25 & upper_ci >= 25)
  })
  print(summarise(intervals, coverage_fraction = mean(covers)))
  ggplot(intervals, aes(y = sample_number, color = covers)) +
    geom_segment(aes(x = lower_ci, xend = upper_ci, yend = sample_number)) +
    geom_vline(xintercept = 25, linetype = "dashed") +
    labs(x = "Interval for the population mean", y = "New sample") +
    theme_minimal(base_size = 14)
}
# bootstrap_coverage()
