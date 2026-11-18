# GEOG 531. Read the Quarto source for explanations and assumptions.
# Optional installation, export and spatial chunks are wrapped in if (FALSE).
params <- list(run_spatial = FALSE)


# install-glm-packages
if (FALSE) {
install.packages(c("tidyverse", "broom", "knitr", "rmarkdown"))
# MASS is a recommended R package and is usually already installed.
# If it is missing, install a version compatible with your R installation:
# install.packages("MASS")
}

# glm-setup
needed <- c("tidyverse", "broom", "MASS", "knitr", "rmarkdown")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) stop("Install these packages first: ", paste(missing, collapse = ", "))
if (getRversion() < "4.1.0") stop("Use R 4.1 or newer.")

library(tidyverse)
library(broom)
theme_set(theme_minimal(base_size = 12))
options(dplyr.summarise.inform = FALSE,
        contrasts = c("contr.treatment", "contr.poly"))

# glm-simulation
simulate_sc <- function(seed = 5312025) {
  # Park–Miller uniforms, with exactly representable integer arithmetic.
  state <- as.double(seed)
  stopifnot(state > 0, state < 2147483647, state == floor(state))
  next_u <- function(n) {
    out <- numeric(n)
    for (j in seq_len(n)) {
      state <<- (16807 * state) %% 2147483647
      out[j] <- state / 2147483647
    }
    out
  }
  next_z <- function(n) qnorm(next_u(n))
  clamp <- function(x, lower, upper) pmin(upper, pmax(lower, x))

  # 46 artificial county blocks, each containing 16 artificial tracts.
  counties <- expand_grid(county_y = 1:6, county_x = 1:8) |>
    filter(!(county_y == 6 & county_x %in% c(1, 8))) |>
    mutate(county_number = row_number(),
           county_id = str_c("SCsim_", str_pad(county_number, 2, pad = "0")),
           hidden_context = next_z(n()))

  grid <- crossing(county_number = 1:46, local_y = 0:3, local_x = 0:3) |>
    left_join(counties, by = "county_number") |>
    arrange(county_number, local_y, local_x) |>
    mutate(x = (county_x - 1) * 4 + local_x + 1,
           y = (county_y - 1) * 4 + local_y + 1)

  n <- nrow(grid)
  east <- (grid$x - mean(grid$x)) / sd(grid$x)
  context <- grid$hidden_context
  poverty <- clamp(18 + 2 * context + 7 * next_z(n) + 3 * east, 4, 40)
  impervious <- clamp(40 + 4 * context + 14 * next_z(n) + 8 * east, 5, 90)
  imp <- (impervious - 40) / 10
  heat_rate <- exp(2.1 + .018 * poverty + .13 * imp + .28 * context + .12 * east)
  heat <- as.integer(qpois(next_u(n), lambda = heat_rate))
  hardship <- 2 * poverty + .4 * next_z(n)

  # Rook adjacency: share a side on the artificial grid.
  A <- 1 * ((abs(outer(grid$x, grid$x, "-")) +
               abs(outer(grid$y, grid$y, "-"))) == 1)
  stopifnot(all(rowSums(A) > 0))
  W <- A / rowSums(A)
  rho <- .70
  spatial_filter <- diag(n) - rho * W
  sigma <- .25 + .035 * heat + .015 * poverty
  z_m <- next_z(n)
  z_y <- next_z(n)
  components <- solve(spatial_filter, cbind(.35 * z_m, sigma * z_y))
  disruption <- .20 * heat + .02 * poverty + .15 * imp + components[, 1]

  # Total heat effect: .10 directly + (.20 * .40) through disruption = .18.
  mu_total <- 3 + .18 * heat + .048 * poverty + .31 * imp +
    .20 * imp^2 + .65 * context
  health <- 3 + .10 * heat + .40 * disruption + .04 * poverty +
    .25 * imp + .20 * imp^2 + .65 * context + components[, 2]

  dat <- tibble(
    tract_id = str_c("T", str_pad(seq_len(n), 4, pad = "0")),
    county_id = factor(grid$county_id), year = 2025L,
    x = grid$x, y = grid$y, heat_days = heat,
    poor_health_days = health, poverty_pct = poverty,
    hardship_index = hardship, impervious_pct = impervious,
    impervious_c10 = imp, disruption_index = disruption
  )
  audit <- tibble(
    tract_id = dat$tract_id[371],
    verified_poor_health_days = dat$poor_health_days[371],
    audit_note = "Source entry check: added 35 days during data entry."
  )
  dat$poor_health_days[371] <- dat$poor_health_days[371] + 35
  dimnames(W) <- list(dat$tract_id, dat$tract_id)

  list(data = dat, audit = audit, W = W,
       truth = list(total_effect = .18, direct_effect = .10,
                    indirect_effect = .08, rho = rho, mu_total = mu_total,
                    hidden_context = context, innovation_sd = sigma))
}


add_modifier_data <- function(data, seed = 531917) {
  state <- as.double(seed)
  next_u <- function(n) {
    out <- numeric(n)
    for (j in seq_len(n)) {
      state <<- (16807 * state) %% 2147483647
      out[j] <- state / 2147483647
    }
    out
  }
  n <- nrow(data)
  u <- next_u(n)
  cooling <- case_when(u < .40 ~ "Limited", u < .75 ~ "Partial", TRUE ~ "Reliable")
  housing <- if_else(next_u(n) < plogis(-.5 + .035 * (data$poverty_pct - 20)),
                     "Vulnerable", "Standard")
  program <- if_else(next_u(n) < .50, "Yes", "No")
  h <- (data$heat_days - 15) / 5
  p <- (data$poverty_pct - 20) / 10
  part <- as.numeric(cooling == "Partial")
  reliable <- as.numeric(cooling == "Reliable")
  vulnerable <- as.numeric(housing == "Vulnerable")
  yes <- as.numeric(program == "Yes")

  modifier_contribution <- -.40 * part - .80 * reliable + .45 * vulnerable -
    .25 * yes + .25 * h * p - .20 * h * part - .45 * h * reliable -
    .65 * yes * vulnerable

  data |>
    mutate(heat5 = h, poverty10 = p,
           impervious_c10_sq = impervious_c10^2,
           cooling_access = factor(cooling, levels = c("Limited", "Partial", "Reliable")),
           housing_profile = factor(housing, levels = c("Standard", "Vulnerable")),
           warning_program = factor(program, levels = c("No", "Yes")),
           poor_health_days_modified = poor_health_days + modifier_contribution)
}

add_glm_outcomes <- function(data, seed = 5311125) {
  state <- as.double(seed)
  next_u <- function(n) {
    out <- numeric(n)
    for (j in seq_len(n)) {
      state <<- (16807 * state) %% 2147483647
      out[j] <- state / 2147483647
    }
    out
  }
  n <- nrow(data)
  h <- data$heat5
  p <- data$poverty10
  i <- data$impervious_c10
  part <- as.numeric(data$cooling_access == "Partial")
  reliable <- as.numeric(data$cooling_access == "Reliable")
  vulnerable <- as.numeric(data$housing_profile == "Vulnerable")
  yes <- as.numeric(data$warning_program == "Yes")

  eta <- -1.25 + .40 * h + .35 * p + .18 * i + .10 * i^2 -
    .35 * part - .75 * reliable + .40 * vulnerable - .25 * yes -
    .10 * h * part - .22 * h * reliable
  probability <- plogis(eta)
  illness <- as.integer(qbinom(next_u(n), size = 1, prob = probability))
  population <- as.integer(floor(600 + 3400 * next_u(n)))
  log_rate <- -6 + .28 * h + .30 * p + .15 * i + .08 * i^2 -
    .20 * part - .45 * reliable + .35 * vulnerable - .25 * yes -
    .08 * h * part - .16 * h * reliable
  mu <- population * exp(log_rate)
  visits <- as.integer(qpois(next_u(n), lambda = mu))
  theta <- 1.5
  visits_extra <- as.integer(qnbinom(next_u(n), size = theta, mu = mu))

  list(data = data |>
         mutate(resident_heat_illness = illness, population_2025 = population,
                person_years = population, heat_visits = visits,
                heat_visits_extra = visits_extra),
       truth = list(probability = probability, expected_visits = mu,
                    nb_theta = theta, outcomes_conditionally_independent = TRUE))
}

sim <- simulate_sc()
base_data <- sim$data |>
  left_join(sim$audit, by = "tract_id") |>
  mutate(poor_health_days = coalesce(verified_poor_health_days, poor_health_days)) |>
  select(-verified_poor_health_days, -audit_note) |>
  add_modifier_data()

glm_sim <- add_glm_outcomes(base_data)
gdat <- glm_sim$data
glm_truth <- glm_sim$truth

# binary-audit
gdat |> count(resident_heat_illness) |> mutate(proportion = n / sum(n))

gdat |>
  group_by(cooling_access, housing_profile) |>
  summarise(n = n(), events = sum(resident_heat_illness),
            non_events = sum(resident_heat_illness == 0), .groups = "drop")

# additive-logistic
logit_add <- glm(
  resident_heat_illness ~ heat5 + poverty10 + impervious_c10 + impervious_c10_sq +
    cooling_access + housing_profile + warning_program,
  family = binomial(link = "logit"), data = gdat
)

tidy(logit_add, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

# logistic-interaction
logit_model <- glm(
  resident_heat_illness ~ heat5 * cooling_access + poverty10 +
    impervious_c10 + impervious_c10_sq + housing_profile + warning_program,
  family = binomial(link = "logit"), data = gdat
)

tidy(logit_model, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

# glm-profile-helper
make_glm_profiles <- function(heat_days = 15, cooling_access = "Limited",
                              poverty_pct = 20, housing_profile = "Standard",
                              warning_program = "No", person_years = 1000) {
  crossing(heat_days = heat_days, cooling_access = cooling_access,
           poverty_pct = poverty_pct, housing_profile = housing_profile,
           warning_program = warning_program, person_years = person_years) |>
    mutate(heat5 = (heat_days - 15) / 5, poverty10 = (poverty_pct - 20) / 10,
           impervious_c10 = 0, impervious_c10_sq = 0,
           cooling_access = factor(cooling_access, levels = levels(gdat$cooling_access)),
           housing_profile = factor(housing_profile, levels = levels(gdat$housing_profile)),
           warning_program = factor(warning_program, levels = levels(gdat$warning_program)))
}

# logistic-predictions
probability_grid <- make_glm_profiles(heat_days = 5:30,
                                      cooling_access = levels(gdat$cooling_access))

probability_predictions <- augment(logit_model, newdata = probability_grid,
                                    type.predict = "response")

probability_predictions |>
  ggplot(aes(heat_days, .fitted, color = cooling_access)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(x = "Extreme-heat days", y = "Predicted illness probability", color = "Cooling")

# probability-contrasts
augment(logit_model,
        newdata = make_glm_profiles(heat_days = c(10, 15),
                                    cooling_access = levels(gdat$cooling_access)),
        type.predict = "response") |>
  select(cooling_access, heat_days, .fitted) |>
  pivot_wider(names_from = heat_days, values_from = .fitted, names_prefix = "heat_") |>
  mutate(probability_difference_pp = 100 * (heat_15 - heat_10))

# group-specific-odds-ratios
augment(logit_model,
        newdata = make_glm_profiles(heat_days = c(10, 15),
                                    cooling_access = levels(gdat$cooling_access)),
        type.predict = "link") |>
  select(cooling_access, heat_days, .fitted) |>
  pivot_wider(names_from = heat_days, values_from = .fitted, names_prefix = "heat_") |>
  transmute(cooling_access, OR_per_5_heat_days = exp(heat_15 - heat_10))

# logistic-convergence
tibble(converged = purrr::pluck(logit_model, "converged"))

tidy(logit_model) |>
  arrange(desc(std.error)) |>
  select(term, estimate, std.error) |>
  slice_head(n = 5)

# logistic-calibration
logit_diagnostics <- augment(logit_model, data = gdat,
                             type.predict = "response", type.residuals = "pearson")

calibration <- logit_diagnostics |>
  mutate(bin = ntile(.fitted, 10)) |>
  group_by(bin) |>
  summarise(n = n(), predicted = mean(.fitted), observed = mean(resident_heat_illness))

calibration

calibration |>
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(size = 3, color = "#237ba0") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Mean fitted probability", y = "Observed event proportion")

# logistic-residual-shape
logit_diagnostics |>
  ggplot(aes(heat5, .resid, color = cooling_access)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Centered heat, in five-day units", y = "Pearson residual", color = "Cooling")

logit_diagnostics |>
  select(tract_id, cooling_access, resident_heat_illness, .fitted, .hat, .cooksd) |>
  slice_max(.cooksd, n = 5, with_ties = FALSE)

# logistic-fit-summary
logit_diagnostics |>
  summarise(Brier_score = mean((resident_heat_illness - .fitted)^2),
            intercept_only_Brier = mean((resident_heat_illness - mean(resident_heat_illness))^2))

bind_rows(glance(logit_add) |> mutate(model = "Additive"),
          glance(logit_model) |> mutate(model = "Heat × cooling")) |>
  select(model, nobs, AIC, deviance)

# count-audit
gdat |>
  summarise(tracts = n(), total_visits = sum(heat_visits),
            zero_count_tracts = sum(heat_visits == 0),
            minimum_person_years = min(person_years), maximum_person_years = max(person_years),
            valid_counts = all(heat_visits >= 0 & heat_visits %% 1 == 0),
            positive_exposure = all(person_years > 0))

# poisson-model
poisson_model <- glm(
  heat_visits ~ heat5 * cooling_access + poverty10 +
    impervious_c10 + impervious_c10_sq + housing_profile + warning_program +
    offset(log(person_years)),
  family = poisson(link = "log"), data = gdat
)

tidy(poisson_model, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

# count-predictions
count_profiles <- make_glm_profiles(
  heat_days = c(10, 20), cooling_access = levels(gdat$cooling_access),
  person_years = c(1000, 2000)
)

augment(poisson_model, newdata = count_profiles, type.predict = "response") |>
  transmute(heat_days, cooling_access, person_years, expected_visits = .fitted,
            rate_per_1000_person_years = 1000 * .fitted / person_years)

# count-rate-plot
augment(poisson_model,
        newdata = make_glm_profiles(heat_days = 5:30,
                                    cooling_access = levels(gdat$cooling_access),
                                    person_years = 1000),
        type.predict = "response") |>
  ggplot(aes(heat_days, .fitted, color = cooling_access)) +
  geom_line(linewidth = 1) +
  labs(x = "Extreme-heat days", y = "Expected visits per 1,000 person-years", color = "Cooling")

# group-specific-rate-ratios
augment(poisson_model,
        newdata = make_glm_profiles(heat_days = c(10, 15),
                                    cooling_access = levels(gdat$cooling_access),
                                    person_years = 1000),
        type.predict = "link") |>
  select(cooling_access, heat_days, .fitted) |>
  pivot_wider(names_from = heat_days, values_from = .fitted, names_prefix = "heat_") |>
  transmute(cooling_access, IRR_per_5_heat_days = exp(heat_15 - heat_10))

# poisson-dispersion
poisson_diagnostics <- augment(poisson_model, data = gdat,
                               type.predict = "response", type.residuals = "pearson")

poisson_diagnostics |>
  summarise(Pearson_ratio = sum(.resid^2) / df.residual(poisson_model),
            observed_zero_fraction = mean(heat_visits == 0),
            predicted_zero_fraction = mean(dpois(0, lambda = .fitted)))

poisson_diagnostics |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = .3, color = "#237ba0") +
  geom_smooth(method = "loess", se = FALSE, color = "#d45a42") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Expected count", y = "Pearson residual")

# poisson-influence
poisson_diagnostics |>
  select(tract_id, heat_visits, person_years, .fitted, .resid, .hat, .cooksd) |>
  slice_max(.cooksd, n = 5, with_ties = FALSE)

# overdispersed-poisson
poisson_extra <- glm(
  heat_visits_extra ~ heat5 * cooling_access + poverty10 +
    impervious_c10 + impervious_c10_sq + housing_profile + warning_program +
    offset(log(person_years)),
  family = poisson(link = "log"), data = gdat
)

extra_diagnostics <- augment(poisson_extra, data = gdat,
                             type.predict = "response", type.residuals = "pearson")

extra_diagnostics |>
  summarise(Pearson_ratio = sum(.resid^2) / df.residual(poisson_extra),
            observed_zero_fraction = mean(heat_visits_extra == 0),
            predicted_zero_fraction = mean(dpois(0, lambda = .fitted)))

# quasi-poisson
quasi_model <- glm(formula(poisson_extra), family = quasipoisson(link = "log"), data = gdat)

bind_rows(tidy(poisson_extra) |> mutate(model = "Poisson"),
          tidy(quasi_model) |> mutate(model = "Quasi-Poisson")) |>
  filter(term %in% c("heat5", "heat5:cooling_accessReliable")) |>
  select(model, term, estimate, std.error)

# negative-binomial
nb_model <- MASS::glm.nb(formula(poisson_extra), data = gdat)

tidy(nb_model, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high)

tibble(model = c("Poisson for extra outcome", "Negative binomial for extra outcome"),
       AIC = c(AIC(poisson_extra), AIC(nb_model)))

# nb-diagnostics
nb_expected <- predict(nb_model, type = "response")

tibble(theta = nb_model$theta,
       Pearson_ratio = sum(residuals(nb_model, type = "pearson")^2) / df.residual(nb_model),
       observed_zero_fraction = mean(gdat$heat_visits_extra == 0),
       predicted_zero_fraction = mean(dnbinom(0, mu = nb_expected, size = nb_model$theta)))

# glm-residual-map
poisson_diagnostics |>
  ggplot(aes(x, y, fill = .resid)) +
  geom_tile() + coord_equal() +
  scale_fill_gradient2(low = "#237ba0", mid = "white", high = "#d45a42", midpoint = 0) +
  labs(x = NULL, y = NULL, fill = "Pearson residual",
       title = "Count-model residuals on the artificial grid",
       caption = "Simulated tracts, not actual South Carolina geography") +
  theme(axis.text = element_blank(), panel.grid = element_blank())
