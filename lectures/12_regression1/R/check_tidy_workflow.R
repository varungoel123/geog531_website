# Run from the open project after installing the core packages.
# These reference checks could not be run in the creation environment.
library(tidyverse)
library(broom)
d <- read_csv("data/urban_heat.csv", show_col_types = FALSE) |>
  mutate(canopy10 = canopy_pct / 10)
m <- lm(indoor_temp_c ~ canopy10 + deprivation + cool_roof, data = d)
l <- glm(heat_illness ~ canopy10 + deprivation + cool_roof,
         family = binomial(), data = d)
p <- glm(heat_episodes ~ canopy10 + deprivation + cool_roof +
           offset(log(person_weeks)), family = poisson(), data = d)
linear <- tidy(m, conf.int = TRUE) |> filter(term == "canopy10")
stopifnot(abs(pull(linear, estimate) - (-.567530623)) < 1e-6,
          abs(pull(linear, conf.low) - (-.667975120)) < 1e-6,
          abs(pull(linear, conf.high) - (-.467086125)) < 1e-6)
ratio <- tidy(l, conf.int = TRUE, exponentiate = TRUE) |>
  filter(term == "canopy10")
stopifnot(abs(pull(ratio, estimate) - .71164667) < 1e-5,
          abs(pull(ratio, conf.low) - .57086016) < .002,
          abs(pull(ratio, conf.high) - .88078920) < .002)
homes <- tibble(canopy10 = c(2, 4), deprivation = 0, cool_roof = 0)
prob <- augment(l, newdata = homes, type.predict = "response") |> pull(.fitted)
stopifnot(max(abs(prob - c(.308337376, .184184354))) < 1e-5)
profiles <- crossing(canopy10 = c(2, 4), person_weeks = c(100, 200)) |>
  mutate(deprivation = 0, cool_roof = 0)
counts <- augment(p, newdata = profiles, type.predict = "response") |>
  pull(.fitted)
stopifnot(max(abs(counts - c(2.77854982, 5.55709964,
                            1.58101255, 3.16202510))) < 1e-5)
disp <- augment(p, data = d, type.residuals = "pearson") |>
  summarise(ratio = sum(.resid^2) / df.residual(p)) |> pull(ratio)
stopifnot(abs(disp - .974440403) < 1e-5,
          nrow(augment(m, data = d)) == 400,
          all(c(".fitted", ".resid", ".std.resid", ".cooksd") %in%
                names(augment(m, data = d))))
message("Tidyverse/broom reference checks passed.")
