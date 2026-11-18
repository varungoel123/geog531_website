# GEOG 531. Open the RStudio project first. Run code in order.
# Read the lab HTML for questions and interpretation prompts.


# setup
library(tidyverse)
library(broom)

d <- read_csv("data/urban_heat.csv", show_col_types = FALSE) |>
  mutate(canopy10 = canopy_pct / 10)

# check-binary
d |>
  count(heat_illness) |>
  mutate(proportion = n / sum(n))

# fit-logistic
logit_model <- glm(heat_illness ~ canopy10 + deprivation + cool_roof,
                   family = binomial(link = "logit"), data = d)

# Log-odds coefficients, then odds ratios with 95% intervals:
tidy(logit_model) |>
  select(term, estimate, std.error)

logistic_results <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

logistic_results

# logistic-prediction
homes <- tibble(canopy10 = c(2, 4), deprivation = 0, cool_roof = 0)

predicted_homes <- augment(logit_model, newdata = homes,
                           type.predict = "response") |>
  transmute(canopy_pct = canopy10 * 10, deprivation, cool_roof,
            probability = .fitted)

predicted_homes

predicted_homes |>
  arrange(canopy_pct) |>
  summarise(difference_pp = 100 * (last(probability) - first(probability)))

# logistic-calibration
augment(logit_model, data = d, type.predict = "response") |>
  mutate(probability_bin = cut(.fitted, breaks = seq(0, 1, by = 0.2),
                               include.lowest = TRUE)) |>
  group_by(probability_bin) |>
  summarise(n = n(), observed = mean(heat_illness), predicted = mean(.fitted),
            .groups = "drop")

tibble(converged = purrr::pluck(logit_model, "converged"))

# check-count
d |>
  summarise(mean_episodes = mean(heat_episodes),
            zero_counts = sum(heat_episodes == 0),
            smallest_exposure = min(person_weeks),
            largest_exposure = max(person_weeks),
            valid_counts = all(heat_episodes >= 0 & heat_episodes %% 1 == 0),
            positive_exposure = all(person_weeks > 0))

# fit-poisson
count_model <- glm(heat_episodes ~ canopy10 + deprivation + cool_roof +
                     offset(log(person_weeks)),
                   family = poisson(link = "log"), data = d)

poisson_results <- tidy(count_model, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

poisson_results

# poisson-prediction
# All combinations of two canopy values and two exposure amounts:
profiles <- crossing(canopy10 = c(2, 4), person_weeks = c(100, 200)) |>
  mutate(deprivation = 0, cool_roof = 0)

predicted_counts <- augment(count_model, newdata = profiles,
                            type.predict = "response") |>
  transmute(canopy_pct = canopy10 * 10, person_weeks,
            expected_episodes = .fitted,
            rate_per_100_person_weeks = 100 * .fitted / person_weeks)

predicted_counts

# dispersion-check
extra_model <- glm(episodes_extra ~ canopy10 + deprivation + cool_roof +
                     offset(log(person_weeks)),
                   family = poisson(link = "log"), data = d)

# The diagnostic calculation is supplied; focus on its interpretation.
bind_rows(
  augment(count_model, data = d, type.residuals = "pearson") |>
    summarise(outcome = "heat_episodes",
              pearson_ratio = sum(.resid^2) / df.residual(count_model)),
  augment(extra_model, data = d, type.residuals = "pearson") |>
    summarise(outcome = "episodes_extra",
              pearson_ratio = sum(.resid^2) / df.residual(extra_model))
)

# Optional: quasi-extension (select code inside the block to run).
if (FALSE) {
quasi_model <- glm(episodes_extra ~ canopy10 + deprivation + cool_roof +
                     offset(log(person_weeks)),
                   family = quasipoisson(link = "log"), data = d)

bind_rows(
  tidy(extra_model) |> mutate(model = "Poisson"),
  tidy(quasi_model) |> mutate(model = "Quasi-Poisson")
) |>
  select(model, term, estimate, std.error)
}
