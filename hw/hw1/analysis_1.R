library(tidyverse)
library(sf)

s2015 <- read_csv("data/events_2015.csv")
s2024 <- read_csv("data/events_2024.csv")

dat2015 <- s2015 |> filter(STATE_FIPS == "45") |>
  select(STATE:YEAR,EVENT_TYPE, INJURIES_DIRECT:DAMAGE_PROPERTY,
         BEGIN_LAT:BEGIN_LON,EPISODE_NARRATIVE:EVENT_NARRATIVE)
sc15 <- dat2015 


dat2024 <- s2024 |>
  select(STATE:YEAR,EVENT_TYPE, INJURIES_DIRECT:DAMAGE_PROPERTY,
         BEGIN_LAT:BEGIN_LON,EPISODE_NARRATIVE:EVENT_NARRATIVE)
sc24 <- dat2024 |> filter(STATE_FIPS == "45")

event_type_filter <- intersect(sc15$EVENT_TYPE,sc24$EVENT_TYPE)

sc15 <- sc15 %>% filter(EVENT_TYPE %in% event_type_filter) %>%
  filter(!is.na(BEGIN_LAT))

sc24 <- sc24 %>% filter(EVENT_TYPE %in% event_type_filter) %>%
  filter(!is.na(BEGIN_LAT))

write_csv(sc15,"sc_nhaz_15.csv")
write_csv(sc24,"sc_nhaz_24.csv")

sc_sf <- st_as_sf(sc15, coords = c("BEGIN_LON","BEGIN_LAT"), crs = 4326)


acs_24 <- get_acs_geographies(
  geography = c("county"),
  state = "SC",
  table = "B08134",
  year = 2023,
  quiet = TRUE,
  geometry = T,
  output = "wide"
)

acs_15 <- get_acs_geographies(
  geography = c("county"),
  state = "SC",
  table = "B08134",
  year = 2015,
  quiet = TRUE,
  geometry = T,
  output = "wide"
)