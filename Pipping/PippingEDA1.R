##################################################
# Title: WWC Passing EDA
# By: Luke and Zach
##################################################

# Loading in packages/data
library(tidyverse)
# install.packages("janitor")
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")

# Cleaning the names 
wwc_passes <- wwc_passes |> 
  janitor::clean_names()

## Cleaning the data
# Replacing NAs with FALSE for pass columns
wwc_passes_2 <- wwc_passes |>
  select(under_pressure, counterpress, pass_switch:pass_miscommunication)

wwc_passes_2[is.na(wwc_passes_2)] <- FALSE
wwc_passes[c(5, 6, 11:23)] <- wwc_passes_2

# Replacing NAs in pass outcomes with "Complete"
wwc_passes_3 <- wwc_passes |> 
  select(pass_outcome_name) |>
  mutate(pass_outcome_name = ifelse(is.na(pass_outcome_name == TRUE), 
                                    "Complete", pass_outcome_name))

names(wwc_passes[11:23]) <- c("switch", "aerial_won", "cross", "through_ball",
                              "shot_assist", "outswinging", "straight", "inswinging",
                              "goal_assist", "cut_back", "no_touch", "deflected",
                              "miscommunication")
wwc_passes <- wwc_passes |> 
  mutate(team_name = str_remove_all(team_name, " Women's"))

wwc_passes
wwc_passes[28] <- wwc_passes_3
sum(rowSums(wwc_passes[11:23]) == 0) # 52347
sum(rowSums(wwc_passes[11:23]) == 1) # 3629
sum(rowSums(wwc_passes[11:23]) == 2) # 637
sum(rowSums(wwc_passes[11:23] ) == 3) # 97

wwc_passes <-
wwc_passes |>
  mutate(tot_poss = time_in_poss + time_to_poss_end)
sum(is.na(wwc_passes$pass_body_part_name)) ## 4810/56710
mean(is.na(wwc_passes$pass_body_part_name)) ## 8.48%

team_stats

wwc_passes |>
  group_by(team_name) |>
  summary()
sum(wwc_passes$pass_goal_assist)

wwc_passes |> 
  group_by(pass_outcome_name, under_pressure) |> 
  count(pass_outcome_name, name = "Passes") |> 
  filter(pass_outcome_name != "Unknown") |> 
  ggplot(aes(x = reorder(under_pressure, Passes), y = Passes, fill = pass_outcome_name)) +
  geom_col() +
  scale_fill_manual("Under Pressure?", values = c("forestgreen", "goldenrod","red", "blue","yellow")) +
  theme_bw()  +
  scale_y_log10() +
  labs(title = "WWC pass outcomes and their pressure rates",
       y = "Pass Outcome")

