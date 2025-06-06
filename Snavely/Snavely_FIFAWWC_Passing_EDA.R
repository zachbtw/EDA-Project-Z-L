##################################################
# Title: WWC Passing EDA
# By: Luke and Zach
##################################################

# Loading in packages/data
library(tidyverse)
install.packages("janitor")
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

wwc_passes[28] <- wwc_passes_3


## EDA
# Looking at pass outcome due to pressure
wwc_passes |> 
  group_by(pass_outcome_name, under_pressure) |> 
  count(pass_outcome_name, name = "Passes") |> 
  filter(pass_outcome_name != "Unknown") |> 
  ggplot(aes(y = reorder(pass_outcome_name, Passes), x = Passes, fill = under_pressure)) +
  geom_col() +
  scale_fill_manual("Under Pressure?", values = c("forestgreen", "gold")) +
  theme_bw() +
  scale_x_sqrt() +
  labs(title = "WWC pass outcomes and their pressure rates",
       y = "Pass Outcome")