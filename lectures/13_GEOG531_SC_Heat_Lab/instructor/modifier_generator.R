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
