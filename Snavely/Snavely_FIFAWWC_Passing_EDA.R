library(tidyverse)
wwc_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/wwc_passes.csv")
install.packages("StatsBombR")

## Cleaning the data
# Replacing NAs with FALSE
wwc_passes_2 <- wwc_passes |>
  select(pass.switch:pass.miscommunication)

wwc_passes_2[is.na(wwc_passes_2)] <- FALSE

