library(tidyverse)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")
wwc_passes <- wwc_passes |> 
  janitor::clean_names()

spain_passes <- filter(wwc_passes, team_name == "Spain Women's")
spain_passes

cr_passes <- filter(wwc_passes, team_name == "Costa Rica Women's")

library(dplyr)
library(tidyr)

sp_games <- spain_passes %>%
  mutate(
    prev_period = lag(period),
    # start of a new game
    new_game = if_else(period == 1 & lag(period) %in% c(2, 3, 4), 1, 0),
    new_game = replace_na(new_game, 1),
    # game numbers
    game = cumsum(new_game) +1
  ) %>%
  select(-prev_period, -new_game)

spain_passes <- sp_games |>
  select(game, everything())

