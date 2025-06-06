##################################################
# Title: WWC Passing EDA
# By: Luke and Zach
##################################################

# Loading in packages/data
library(tidyverse)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")


## Cleaning the data
# Replacing NAs with FALSE for pass columns
wwc_passes_2 <- wwc_passes |>
  select(under_pressure, counterpress, pass.switch:pass.miscommunication)

wwc_passes_2[is.na(wwc_passes_2)] <- FALSE

wwc_passes[c(5, 6, 11:23)] <- wwc_passes_2

table(wwc_passes$pass.type.name)
table(wwc_passes$pass.outcome.name)
