library(tidyverse)

urban_yes <- 45
urban_total <- 60
rural_yes <- 24
rural_total <- 40

# Create combined data

group <- c(rep("urban", urban_total), rep("rural", rural_total))
responses <- c(rep("yes",45), rep("no",15), rep("yes",24), rep("no",16))

dat_2prop<- tibble(area_type = factor(group, levels = c("urban","rural")),
                   climhuman = factor(responses, levels = c("yes","no")))

dat_1prop <- dat_2prop |>
  filter(area_type == "rural")




ggplot(dat, aes(x = area_type, fill = climhuman)) +
  geom_bar(position = "fill") +
  labs(x = "Area Type", y = "Proportion (Climate change caused by humans)") +
  coord_flip()


## age_data

dat <- read_csv("./lectures/8_sampling/data/dat_2prop_with_age.csv")
dat_1mean <- dat_2prop |>
  mutate(age = dat$age)

write_rds(dat_1mean,"./lectures/8_sampling/data/dat_1mean.rds")




#### paired means


df <- read_rds("lectures/8_sampling/data/dat_1mean.rds")

set.seed(42)

# Add a new variable: climate concern score before and after campaign
# Assume initial concern slightly higher for those who already believe humans cause climate change

df$concern_before <- ifelse(df$climhuman == "yes",
                            rnorm(nrow(df), mean = 7, sd = 1.2),
                            rnorm(nrow(df), mean = 5.5, sd = 1.5))

# After campaign, we assume both groups increased slightly
df$concern_after <- df$concern_before + rnorm(nrow(df), mean = 0.8, sd = 0.8)

# Keep scores within 1–10 range
df$concern_before <- pmin(pmax(df$concern_before, 1), 10) %>% as.integer
df$concern_after <- pmin(pmax(df$concern_after, 1), 10) %>% as.integer()

# Inspect
head(df)