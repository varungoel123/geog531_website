####
#### R script to calculate Poisson probabilities for each
#### observation in a spatial data layer given a count of
#### events and a population
####
## install these packages using install.packages("packagename") if you have not already
#### Libraries
library(tidyverse)
library(tigris)
library(sf)
library(spdep)
library(tmap)


#### Load data (classic NC SIDS data from spdep)
nc_sids <- nc.sids

## Subset and update
nc_sids <- tibble(COUNTY = row.names(nc_sids),
                  BIRTHS = nc_sids$BIR79,
                  SIDS = nc_sids$SID79)

#### Get polygon data for mapping
nc_cty <- counties(state = "NC",
                   cb = TRUE)

## Subset and Table Join
nc_sids <- left_join(nc_cty |> select(GEOID, NAME),
                     nc_sids,
                     by = c("NAME" = "COUNTY"))

#### Use function from spdep to calculate
#### Raw rates and Poisson probabilities
nc_sids_rates <- probmap(n = nc_sids |> st_drop_geometry() |> pull(SIDS),
                         x = nc_sids |> st_drop_geometry() |> pull(BIRTHS), 
                         row.names = nc_sids |> st_drop_geometry() |> pull(GEOID), 
                         alternative = "greater")

## Some housekeeping
nc_sids_rates <- bind_cols(GEOID = row.names(nc_sids_rates),
                           nc_sids_rates)

#### Join back to original data layer
nc_sids <- left_join(nc_sids,
                     nc_sids_rates,
                     by = "GEOID")

#### Map Raw Rate per 10,000 births
tm_shape(nc_sids |> mutate(rate_map = raw * 10000)) +
  tm_polygons("rate_map", palette = "Reds")

#### Map Poisson p-value (greater than)
tm_shape(nc_sids) +
  tm_polygons("pmap", 
              palette = "-Reds",
              breaks=c(0, 0.001, 0.01, 0.05, 0.1, 1))
