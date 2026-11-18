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
