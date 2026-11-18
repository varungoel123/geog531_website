# Run from the open RStudio project before class.
d <- read.csv("data/urban_heat.csv")
source("R/helpers.R")
stopifnot(nrow(d) == 400, !anyNA(d), !anyDuplicated(d$household_id),
          all(abs(d$canopy10 - d$canopy_pct / 10) < 1e-8),
          all(d$person_weeks == d$household_members * d$weeks_observed))
m0 <- lm(indoor_temp_c ~ canopy10, data = d)
m2 <- lm(indoor_temp_c ~ canopy10 + deprivation + cool_roof, data = d)
l <- glm(heat_illness ~ canopy10 + deprivation + cool_roof,
         family = binomial(), data = d)
p <- glm(heat_episodes ~ canopy10 + deprivation + cool_roof +
           offset(log(person_weeks)), family = poisson(), data = d)
stopifnot(abs(coef(m0)["canopy10"] - (-1.053096869)) < 1e-6,
          abs(coef(m2)["canopy10"] - (-.567530623)) < 1e-6,
          abs(coef(l)["canopy10"] - (-.340173742)) < 1e-5,
          abs(coef(p)["canopy10"] - (-.281931823)) < 1e-5,
          abs(dispersion_ratio(p) - .974440403) < 1e-5)
homes <- data.frame(canopy10 = c(2, 4), deprivation = 0, cool_roof = 0)
stopifnot(max(abs(predict(l, newdata = homes, type = "response") -
                   c(.308337376, .184184354))) < 1e-5)
roof <- lm(indoor_temp_c ~ cool_roof, data = d)
stopifnot(abs(coef(roof)["cool_roof"] -
                diff(tapply(d$indoor_temp_c, d$cool_roof, mean))) < 1e-8)
stopifnot(abs(sqrt(diag(hc3_vcov(m2)))["canopy10"] - .050917180) < 1e-6)
message("All native-R model checks passed.")
