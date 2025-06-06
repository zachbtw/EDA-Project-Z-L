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

c(1, 2, 3, 4, 5, 6, 6)