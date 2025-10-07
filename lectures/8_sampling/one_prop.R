
library(infer)
ggplot(data = dat_1prop, aes(x = climhuman)) +
  geom_bar()

dat_1prop |> 
  summarize(prop_clim = sum(climhuman=="yes")/40)


dat_1prop$climhuman <- factor(dat_1prop$climhuman, levels = c("yes","no"))
set.seed(2018)

p_hat <- dat_1prop|>
  specify(response = climhuman, success = "yes")  |>
  calculate(stat = "prop")
p_hat

null_distn_one_prop <- dat_1prop |>
  specify(response = climhuman, success = "yes") |>
  hypothesize(null = "point", p = 0.5) |>
  generate(reps = 10000) |>
  calculate(stat = "prop")


null_distn_one_prop |>
  visualize() +
  shade_p_value(obs_stat = p_hat, direction = "both")

pvalue <- null_distn_one_prop |>
  get_pvalue(obs_stat = p_hat, direction = "both")
pvalue

## confidence interval

boot_distn_one_prop <- dat_1prop |>
  specify(response = climhuman, success = "yes") |>
  generate(reps = 10000) |>
  calculate(stat = "prop")

ci <- boot_distn_one_prop |>
  get_ci()
ci

boot_distn_one_prop |>
  visualize() +
  shade_ci(endpoints = ci)



prop.test(
  x = table(dat_1prop$climhuman),
  n = length(dat_1prop$climhuman),
  alternative = "two.sided",
  p = 0.5,
  correct = FALSE
)

