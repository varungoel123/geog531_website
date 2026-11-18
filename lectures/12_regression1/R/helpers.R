# GEOG 531: display and diagnostic helpers. No non-base packages required.
# Students can use the helpers without memorizing their implementation.

coef_table <- function(model, exponentiate = FALSE, robust = FALSE) {
  b <- coef(model)
  V <- if (robust) hc3_vcov(model) else vcov(model)
  se <- sqrt(diag(V))
  is_linear <- inherits(model, "lm") && !inherits(model, "glm")
  critical <- if (is_linear) qt(.975, df.residual(model)) else qnorm(.975)
  lower <- b - critical * se
  upper <- b + critical * se
  # Wald intervals for GLMs; t intervals for linear models.
  if (exponentiate) {
    b <- exp(b)
    lower <- exp(lower)
    upper <- exp(upper)
  }
  data.frame(term = names(b), estimate = unname(b),
             lower_95 = unname(lower), upper_95 = unname(upper),
             row.names = NULL)
}

hc3_vcov <- function(model) {
  stopifnot(inherits(model, "lm"), !inherits(model, "glm"))
  X <- model.matrix(model)
  bread <- solve(crossprod(X))
  adjusted_residual <- residuals(model) / (1 - hatvalues(model))
  meat <- crossprod(X * as.numeric(adjusted_residual))
  bread %*% meat %*% bread
}

dispersion_ratio <- function(model) {
  sum(residuals(model, type = "pearson")^2) / df.residual(model)
}

calibration_table <- function(model, observed) {
  p <- predict(model, type = "response")
  bin <- cut(p, breaks = seq(0, 1, by = .2), include.lowest = TRUE)
  means <- aggregate(cbind(observed = observed, predicted = p),
                     by = list(probability_bin = bin), FUN = mean)
  counts <- aggregate(observed, by = list(probability_bin = bin), FUN = length)
  names(counts)[2] <- "n"
  merge(means, counts, by = "probability_bin", sort = TRUE)
}
