# GEOG 531. Read the Quarto source for explanations and assumptions.
# Optional installation, export and spatial chunks are wrapped in if (FALSE).
params <- list(run_spatial = FALSE)


# install-core
if (FALSE) {
# Run once in the Console, not every time the document is rendered:
install.packages(c("tidyverse", "broom", "lmtest", "sandwich", "knitr", "rmarkdown"))
}

# setup
needed <- c("tidyverse", "broom", "lmtest", "sandwich", "knitr", "rmarkdown")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Install these packages in the Console first: ", paste(missing, collapse = ", "))
}
if (getRversion() < "4.1.0") stop("Use R 4.1 or newer for the |> pipe.")

library(tidyverse)
library(broom)
theme_set(theme_minimal(base_size = 12))
options(dplyr.summarise.inform = FALSE,
        contrasts = c("contr.treatment", "contr.poly"))

# simulate-data
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

sim <- simulate_sc()
dat_raw <- sim$data
truth <- sim$truth

# inspect-data
dat_raw |> glimpse()

dat_raw |>
  summarise(tracts = n(), counties = n_distinct(county_id),
            missing_outcomes = sum(is.na(poor_health_days)),
            minimum_health = min(poor_health_days),
            maximum_health = max(poor_health_days))

flagged <- dat_raw |>
  filter(poor_health_days < 0 | poor_health_days > 30)
flagged |> select(tract_id, county_id, poor_health_days)

# source-audit
sim$audit

dat <- dat_raw |>
  left_join(sim$audit, by = "tract_id") |>
  mutate(poor_health_days = coalesce(verified_poor_health_days, poor_health_days)) |>
  select(-verified_poor_health_days, -audit_note)

stopifnot(nrow(dat) == 736, !anyDuplicated(dat$tract_id),
          !anyNA(dat), all(between(dat$poor_health_days, 0, 30)))

# crude-plot
dat |>
  ggplot(aes(heat_days, poor_health_days)) +
  geom_point(alpha = .4, color = "#237ba0") +
  geom_smooth(method = "lm", se = TRUE, color = "#d45a42") +
  labs(x = "Extreme-heat days in the hypothetical year",
       y = "Mean poor-health days per resident")

# crude-model
m0 <- lm(poor_health_days ~ heat_days, data = dat)

tidy(m0, conf.int = TRUE) |>
  select(term, estimate, std.error, conf.low, conf.high)

# adjusted-model
m1 <- lm(poor_health_days ~ heat_days + poverty_pct + impervious_c10, data = dat)

bind_rows(
  tidy(m0, conf.int = TRUE) |> mutate(model = "Crude"),
  tidy(m1, conf.int = TRUE) |> mutate(model = "Measured-confounder adjusted")
) |>
  filter(term == "heat_days") |>
  select(model, estimate, conf.low, conf.high)

# observed-estimation-error
bind_rows(tidy(m0) |> mutate(model = "Crude"),
          tidy(m1) |> mutate(model = "Adjusted, initial form")) |>
  filter(term == "heat_days") |>
  transmute(model, estimate, known_effect = truth$total_effect,
            estimation_error = estimate - known_effect)

# conditional-bias
oracle_data <- dat |>
  mutate(poor_health_days = truth$mu_total)

expected_slope <- function(model) {
  fitted_mean_model <- lm(formula(model), data = oracle_data)
  unname(coef(fitted_mean_model)["heat_days"])
}

tibble(model = c("Crude", "Adjusted, initial form"),
       expected_heat_slope = c(expected_slope(m0), expected_slope(m1))) |>
  mutate(known_effect = truth$total_effect,
         conditional_bias = expected_heat_slope - known_effect)

# collinear-model
m_collinear <- lm(poor_health_days ~ heat_days + poverty_pct +
                    impervious_c10 + hardship_index, data = dat)

dat |> summarise(poverty_hardship_correlation = cor(poverty_pct, hardship_index))

tidy(m_collinear, conf.int = TRUE) |>
  filter(term %in% c("poverty_pct", "hardship_index")) |>
  select(term, estimate, std.error, conf.low, conf.high)

# vif-helper
numeric_vif <- function(model) {
  X <- model.matrix(model)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  map_dfr(seq_len(ncol(X)), function(j) {
    target <- X[, j]
    other <- X[, -j, drop = FALSE]
    r_squared <- summary(lm(target ~ other))$r.squared
    tibble(term = colnames(X)[j], VIF = 1 / (1 - r_squared))
  })
}

# vif-comparison
bind_rows(numeric_vif(m_collinear) |> mutate(model = "With duplicate proxy"),
          numeric_vif(m1) |> mutate(model = "Keep poverty, omit proxy")) |>
  select(model, term, VIF)

# initial-residual-shape
diagnostics_m1 <- augment(m1, data = dat)

diagnostics_m1 |>
  ggplot(aes(impervious_c10, .resid)) +
  geom_point(alpha = .4, color = "#237ba0") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#d45a42") +
  labs(x = "Impervious cover: (percentage − 40) / 10", y = "Residual")

# curved-model
dat <- dat |> mutate(impervious_c10_sq = impervious_c10^2)

m_curve <- lm(poor_health_days ~ heat_days + poverty_pct +
                impervious_c10 + impervious_c10_sq, data = dat)

augment(m_curve, data = dat) |>
  ggplot(aes(impervious_c10, .resid)) +
  geom_point(alpha = .4, color = "#237ba0") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "#d45a42") +
  labs(x = "Centered impervious cover, in 10-percentage-point units", y = "Residual")

# variance-plots
diagnostics_curve <- augment(m_curve, data = dat)

diagnostics_curve |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = .4, color = "#237ba0") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted mean poor-health days", y = "Residual")

diagnostics_curve |>
  mutate(fitted_group = ntile(.fitted, 5)) |>
  group_by(fitted_group) |>
  summarise(n = n(), residual_sd = sd(.resid), mean_fitted = mean(.fitted))

# variance-test
lmtest::bptest(m_curve) |> tidy()

# hc3-comparison
bind_rows(
  tidy(m_curve, conf.int = TRUE) |> mutate(uncertainty = "Ordinary"),
  lmtest::coeftest(m_curve, vcov. = sandwich::vcovHC(m_curve, type = "HC3")) |>
    tidy(conf.int = TRUE) |> mutate(uncertainty = "HC3")
) |>
  filter(term == "heat_days") |>
  select(uncertainty, estimate, std.error, conf.low, conf.high)

# qq-plot
diagnostics_curve |>
  ggplot(aes(sample = .std.resid)) +
  stat_qq(alpha = .5, color = "#237ba0") +
  stat_qq_line(color = "#d45a42") +
  labs(x = "Theoretical normal quantile", y = "Standardized residual")

# influence
diagnostics_curve |>
  select(tract_id, county_id, heat_days, poor_health_days,
         .cooksd, .hat, .std.resid) |>
  slice_max(order_by = .cooksd, n = 6, with_ties = FALSE)

# within-county-variation
dat |>
  group_by(county_id) |>
  summarise(tracts = n(), heat_sd = sd(heat_days),
            minimum_heat = min(heat_days), maximum_heat = max(heat_days)) |>
  slice_head(n = 8)

# county-fe
m_fe <- lm(poor_health_days ~ heat_days + poverty_pct +
             impervious_c10 + impervious_c10_sq + county_id, data = dat)

bind_rows(
  tidy(m_curve, conf.int = TRUE) |> mutate(model = "Curved mean, no county effects"),
  tidy(m_fe, conf.int = TRUE) |> mutate(model = "County fixed effects")
) |>
  filter(term == "heat_days") |>
  select(model, estimate, conf.low, conf.high)

# final-bias-comparison
# Recreate oracle_data so it includes the new squared column.
oracle_data <- dat |> mutate(poor_health_days = truth$mu_total)

models <- list(Crude = m0, Adjusted = m1, Curved = m_curve, County_FE = m_fe)

model_comparison <- imap_dfr(models, function(model, label) {
  tibble(model = label,
         observed_heat_slope = tidy(model) |>
           filter(term == "heat_days") |> pull(estimate),
         expected_heat_slope = expected_slope(model))
}) |>
  mutate(known_total_effect = truth$total_effect,
         conditional_bias = expected_heat_slope - known_total_effect,
         error_in_this_sample = observed_heat_slope - known_total_effect)

model_comparison

# mediator-model
m_direct <- update(m_fe, . ~ . + disruption_index)

bind_rows(tidy(m_fe) |> mutate(target = "Total-effect model"),
          tidy(m_direct) |> mutate(target = "Mediator-adjusted model")) |>
  filter(term == "heat_days") |>
  select(target, term, estimate)

# map-helper
county_bounds <- dat |>
  group_by(county_id) |>
  summarise(xmin = min(x) - .5, xmax = max(x) + .5,
            ymin = min(y) - .5, ymax = max(y) + .5, .groups = "drop")

map_tracts <- function(data, variable, title, diverging = FALSE) {
  p <- ggplot(data, aes(x, y, fill = .data[[variable]])) +
    geom_tile(width = .98, height = .98) +
    geom_rect(data = county_bounds,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = "grey30", linewidth = .35, inherit.aes = FALSE) +
    coord_equal() + labs(title = title, x = NULL, y = NULL, fill = NULL,
                        caption = "Artificial grid, not actual South Carolina geography") +
    theme_minimal() + theme(axis.text = element_blank(), panel.grid = element_blank())
  if (diverging) p + scale_fill_gradient2(low = "#237ba0", mid = "white",
                                          high = "#d45a42", midpoint = 0)
  else p + scale_fill_viridis_c()
}

# maps
map_tracts(dat, "heat_days", "Extreme-heat days")

diagnostics_fe <- augment(m_fe, data = dat)
map_tracts(diagnostics_fe, ".resid", "County-FE model residuals", diverging = TRUE)

# weights-check
stopifnot(identical(dat$tract_id, rownames(sim$W)),
          all(abs(rowSums(sim$W) - 1) < 1e-12), all(diag(sim$W) == 0))

tibble(tract_id = dat$tract_id,
       neighbors = rowSums(sim$W > 0)) |>
  count(neighbors)

# moran-helper
moran_I <- function(values, W) {
  centered <- values - mean(values)
  as.numeric(length(values) / sum(W) *
               crossprod(centered, W %*% centered) / crossprod(centered))
}

# residual-moran
imap_dfr(models, function(model, label) {
  tibble(model = label, residual_Moran_I = moran_I(residuals(model), sim$W))
})

# install-spatial
if (FALSE) {
install.packages(c("spdep", "sphet"))
}

# spatial-fit
if (FALSE) {
if (!requireNamespace("spdep", quietly = TRUE) ||
    !requireNamespace("sphet", quietly = TRUE)) {
  stop("Install spdep and sphet in the Console before enabling run_spatial.")
}

stopifnot(identical(dat$tract_id, rownames(sim$W)))
listw <- spdep::mat2listw(sim$W, row.names = dat$tract_id,
                         style = "W", zero.policy = FALSE)

spatial_error <- sphet::spreg(
  poor_health_days ~ heat_days + poverty_pct +
    impervious_c10 + impervious_c10_sq + county_id,
  data = as.data.frame(dat), listw = listw,
  model = "error", het = TRUE
)

summary(spatial_error)
}

# final-ols-working-uncertainty
lmtest::coeftest(m_fe, vcov. = sandwich::vcovHC(m_fe, type = "HC3")) |>
  tidy(conf.int = TRUE) |>
  filter(term == "heat_days") |>
  select(term, estimate, std.error, conf.low, conf.high)

# generate-modifiers
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

dat <- add_modifier_data(dat)

# inspect-factors
dat |> count(cooling_access)
dat |> count(warning_program, housing_profile)

levels(dat$cooling_access)

# categorical-predictor
m_category <- lm(
  poor_health_days_modified ~ heat5 + poverty10 + cooling_access +
    warning_program + housing_profile + impervious_c10 + impervious_c10_sq + county_id,
  data = dat
)

tidy(m_category, conf.int = TRUE) |>
  filter(str_detect(term, "cooling_access")) |>
  select(term, estimate, std.error, conf.low, conf.high)

# interaction-model
m_interactions <- lm(
  poor_health_days_modified ~ heat5 * poverty10 + heat5 * cooling_access +
    warning_program * housing_profile + impervious_c10 + impervious_c10_sq + county_id,
  data = dat
)

tidy(m_interactions, conf.int = TRUE) |>
  filter(!str_detect(term, "^county_id")) |>
  select(term, estimate, std.error, conf.low, conf.high)

# interaction-profile-helper
make_ols_profiles <- function(heat_days = 15, poverty_pct = 20,
                              cooling_access = "Limited", warning_program = "No",
                              housing_profile = "Standard") {
  crossing(heat_days = heat_days, poverty_pct = poverty_pct,
           cooling_access = cooling_access, warning_program = warning_program,
           housing_profile = housing_profile) |>
    mutate(heat5 = (heat_days - 15) / 5, poverty10 = (poverty_pct - 20) / 10,
           impervious_c10 = 0, impervious_c10_sq = 0,
           county_id = factor(levels(dat$county_id)[1], levels = levels(dat$county_id)),
           cooling_access = factor(cooling_access, levels = levels(dat$cooling_access)),
           warning_program = factor(warning_program, levels = levels(dat$warning_program)),
           housing_profile = factor(housing_profile, levels = levels(dat$housing_profile)))
}

# continuous-continuous-plot
cc_profiles <- make_ols_profiles(heat_days = 5:30, poverty_pct = c(10, 20, 30))

augment(m_interactions, newdata = cc_profiles) |>
  ggplot(aes(heat_days, .fitted, color = factor(poverty_pct))) +
  geom_line(linewidth = 1) +
  labs(x = "Extreme-heat days", y = "Fitted mean poor-health days",
       color = "Poverty (%)", title = "Limited cooling, same other profile values")

# continuous-continuous-contrasts
cc_contrasts <- augment(
  m_interactions,
  newdata = make_ols_profiles(heat_days = c(15, 20), poverty_pct = c(10, 20, 30))
) |>
  select(poverty_pct, heat_days, .fitted) |>
  pivot_wider(names_from = heat_days, values_from = .fitted, names_prefix = "heat_") |>
  mutate(heat_contrast_5days = heat_20 - heat_15)

cc_contrasts

# continuous-category-plot
hc_profiles <- make_ols_profiles(heat_days = 5:30,
                                 cooling_access = levels(dat$cooling_access))

augment(m_interactions, newdata = hc_profiles) |>
  ggplot(aes(heat_days, .fitted, color = cooling_access)) +
  geom_line(linewidth = 1) +
  labs(x = "Extreme-heat days", y = "Fitted mean poor-health days",
       color = "Cooling access", title = "20% poverty, same other profile values")

# continuous-category-contrasts
augment(m_interactions,
        newdata = make_ols_profiles(heat_days = c(15, 20),
                                    cooling_access = levels(dat$cooling_access))) |>
  select(cooling_access, heat_days, .fitted) |>
  pivot_wider(names_from = heat_days, values_from = .fitted, names_prefix = "heat_") |>
  mutate(heat_contrast_5days = heat_20 - heat_15)

# category-category-cells
program_cells <- augment(
  m_interactions,
  newdata = make_ols_profiles(warning_program = levels(dat$warning_program),
                              housing_profile = levels(dat$housing_profile))
)

program_cells |>
  select(warning_program, housing_profile, .fitted)

program_cells |>
  ggplot(aes(warning_program, .fitted, color = housing_profile, group = housing_profile)) +
  geom_point(size = 3) + geom_line(linewidth = 1) +
  labs(x = "Warning program", y = "Fitted mean poor-health days", color = "Housing profile")

# category-category-contrasts
program_contrasts <- program_cells |>
  select(warning_program, housing_profile, .fitted) |>
  pivot_wider(names_from = warning_program, values_from = .fitted) |>
  mutate(program_difference = Yes - No)

program_contrasts

program_contrasts |>
  select(housing_profile, program_difference) |>
  pivot_wider(names_from = housing_profile, values_from = program_difference) |>
  mutate(difference_in_differences = Vulnerable - Standard)

# spatial-interaction-fit
if (FALSE) {
spatial_interactions <- sphet::spreg(
  formula(m_interactions), data = as.data.frame(dat), listw = listw,
  model = "error", het = TRUE
)
summary(spatial_interactions)
}

# export-data
if (FALSE) {
write_csv(dat_raw, "sc_heat_health_2025_raw.csv")
write_csv(sim$audit, "sc_heat_health_2025_audit.csv")
write_csv(dat, "sc_heat_health_2025_clean.csv")
}
