# GEOG 531. Open the RStudio project first. Run code in order.
# Read the lab HTML for questions and interpretation prompts.


# setup
library(tidyverse)
library(broom)

d <- read_csv("data/urban_heat.csv", show_col_types = FALSE) |>
  mutate(canopy10 = canopy_pct / 10)

d |>
  select(household_id, canopy_pct, deprivation, cool_roof, indoor_temp_c) |>
  slice_head(n = 6)

# fit-linear
m0 <- lm(indoor_temp_c ~ canopy10, data = d)
m1 <- lm(indoor_temp_c ~ canopy10 + deprivation, data = d)

comparison <- bind_rows(
  tidy(m0, conf.int = TRUE) |> mutate(model = "Crude"),
  tidy(m1, conf.int = TRUE) |> mutate(model = "Adjust deprivation")
) |>
  filter(term == "canopy10") |>
  select(model, estimate, conf.low, conf.high)

comparison

# fit-precision
m2 <- lm(indoor_temp_c ~ canopy10 + deprivation + cool_roof, data = d)

linear_results <- tidy(m2, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high)

linear_results

# roof-comparison
d |>
  group_by(cool_roof) |>
  summarise(n = n(), mean_temp_c = mean(indoor_temp_c), .groups = "drop")

roof_only <- lm(indoor_temp_c ~ cool_roof, data = d)

tidy(roof_only, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high)

# linear-diagnostic-plots
linear_diagnostics <- augment(m2, data = d)

linear_diagnostics |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, color = "#237ba0") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#d45a42") +
  labs(x = "Fitted temperature (°C)", y = "Residual (°C)") +
  theme_minimal()

# linear-qq-plot
linear_diagnostics |>
  ggplot(aes(sample = .std.resid)) +
  stat_qq(color = "#237ba0", alpha = 0.6) +
  stat_qq_line(color = "#d45a42") +
  labs(x = "Theoretical normal quantile", y = "Standardized residual") +
  theme_minimal()

# overlap-check
d |>
  mutate(deprivation_group = factor(
    deprivation, levels = c(0, 1), labels = c("Lower (0)", "Higher (1)"))) |>
  ggplot(aes(x = deprivation_group, y = canopy_pct)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.25, color = "#237ba0") +
  labs(x = "Deprivation", y = "Tree canopy (%)") +
  theme_minimal()

# Optional: mediator-extension (select code inside the block to run).
if (FALSE) {
m_shade <- lm(indoor_temp_c ~ canopy10 + deprivation + cool_roof + shade_index,
              data = d)

tidy(m_shade, conf.int = TRUE) |>
  filter(term == "canopy10") |>
  select(term, estimate, conf.low, conf.high)
}

# Optional: robust-extension (select code inside the block to run).
if (FALSE) {
# Install these optional packages once if needed:
# install.packages(c("lmtest", "sandwich"))

bind_rows(
  tidy(m2, conf.int = TRUE) |> mutate(uncertainty = "Ordinary"),
  lmtest::coeftest(m2, vcov. = sandwich::vcovHC(m2, type = "HC3")) |>
    tidy(conf.int = TRUE) |>
    mutate(uncertainty = "HC3 robust")
) |>
  filter(term == "canopy10") |>
  select(uncertainty, estimate, std.error, conf.low, conf.high)
}

# Optional: interaction-extension (select code inside the block to run).
if (FALSE) {
m_interaction <- lm(indoor_temp_c ~ canopy10 * cool_roof + deprivation, data = d)

tidy(m_interaction, conf.int = TRUE) |>
  select(term, estimate, conf.low, conf.high)
}

# Optional: influence-extension (select code inside the block to run).
if (FALSE) {
augment(m2, data = d) |>
  select(household_id, .cooksd, .hat, .std.resid) |>
  slice_max(order_by = .cooksd, n = 5, with_ties = FALSE)
}

# Optional: infer-bootstrap-extension (select code inside the block to run).
if (FALSE) {
# install.packages("infer")  # once, if needed
set.seed(531)

bootstrap_slopes <- d |>
  infer::specify(indoor_temp_c ~ canopy10) |>
  infer::generate(reps = 1000, type = "bootstrap") |>
  infer::calculate(stat = "slope")

bootstrap_slopes |>
  ggplot(aes(x = stat)) +
  geom_histogram(bins = 30, fill = "#237ba0", color = "white") +
  labs(x = "Bootstrap crude slope (°C per 10 percentage points)", y = "Samples") +
  theme_minimal()

bootstrap_slopes |>
  infer::get_confidence_interval(level = 0.95, type = "percentile")
}
