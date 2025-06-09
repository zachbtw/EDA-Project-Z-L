library(tidyverse)
library(ggplot2)
# install.packages("ggpubr")
library(ggpubr)
library(gganimate)
# install.packages("sportyR")
library(sportyR)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")
wwc_passes <- wwc_passes |> 
  janitor::clean_names()

spain_passes <- filter(wwc_passes, team_name == "Spain Women's")

library(dplyr)
library(tidyr)

wwc_passes_2 <- wwc_passes %>%
  group_by(team_name) |>
  mutate(
    prev_period = lag(period),
    # start of a new game
    new_game = if_else(period == 1 & lag(period) %in% c(2, 3, 4), 1, 0),
    new_game = replace_na(new_game, 1),
    # game numbers
    game = cumsum(new_game) +1
  ) %>%
  select(game, everything(), -prev_period, -new_game)

wwc_
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

spain_g1 <- spain_passes |>
  filter(game == 1)

spain_g1 |>
  group_by(position_name) 

cr_passes <- filter(wwc_passes, team_name == "Costa Rica Women's")

cr_games <- cr_passes |>
  mutate(
  prev_period = lag(period),
  # start of a new game
  new_game = if_else(period == 1 & lag(period) %in% c(2, 3, 4), 1, 0),
  new_game = replace_na(new_game, 1),
  # game numbers
  game = cumsum(new_game) +1
) %>%
  select(game, everything(), -prev_period, -new_game)

cr_passes <- cr_games |>
  select(game, everything())

cr_g1 <- cr_passes |>
  filter(game == 1)
  
table(cr_g1$pass_outcome_name)
table(spain_g1$pass_outcome_name)

library(dplyr)

spain_g1 <- spain_g1 |>
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far
  )
cr_g1 <- cr_g1 %>%
  mutate(
    complete_so_far = cumsum(pass_outcome_name == "Complete"),
    total_so_far = row_number(),
    completion_pct = complete_so_far / total_so_far
  )

spain_graph <- spain_g1 |>
  ggplot(aes(x = total_so_far, y = completion_pct)) +
  geom_line()

cr_graph <- cr_g1 |>
  ggplot(aes(x = total_so_far, y = completion_pct)) +
  geom_line() 

ggarrange(spain_graph, cr_graph)

game1 <- rbind(spain_g1, cr_g1)

game1 |>
  ggplot(aes(x = total_so_far, y = completion_pct, color = team_name)) +
  geom_line() 

eng_passes <- filter(wwc_passes, team_name == "England Women's")

